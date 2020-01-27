% Subtle 2.2, SWI Prolog version
% Author: Hendrik Blockeel
% improvements of 2.1 over 2.0:
% - reimplemented make_table with tail recursion, avoiding out-of-stack error
% - made lgg check in advance if one clause subsumes the other (if yes, saves work)
% - cleaned up the code a bit
% improvements over 2.1:
% - added cut in first clause for filter (filter left useless choice points, causing out-of-stack)
% - a better-scaling dbSelect (lookup in binary search tree instead of linear list), called only for long lists
% - additional optimization in makeTable: if InstLit has length 1, immediately instantiate the variables
% - similar optimization in sjrows (last lines handle singleton instantiation-lists separately)

:- module(subtle, [lgg/3, reduceClause/2, subsumes/2, subsumes/3, equiv/2, lggI/5]).

:- use_module(library(lists)).
:- use_module(library(charsio)).

% NOTE: All clauses are represented as lists of literals.
% Negative literals should be represented as not(p(X)).
% There should not be double-nots: not(not(p(X))) will not be recognized as equivalent to p(X)

% EXPORTED PREDICATES:
% lgg(+X,+Y,-Z) <=> Z is the lgg under theta-subsumption of X and Y
% reduce(+X,-Y) <=> Y is the reduced clause of X (shortest clause for which equiv(X,Y) holds)
% subsumes(+X,+Y) <=> X subsumes Y
% subsumes(+X,+Y,-S) <=> X subsumes Y and S is a substitution that proves this
% equiv(+X,+Y) <=> X subsumes Y and Y subsumes X

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART 1 : computing the lgg of 2 clauses %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lgg(Clause1, Clause2, Result) :-
    (subsumes(Clause1, Clause2) -> Clause3=Clause1;   % if one subsumes the other, trivial
     subsumes(Clause2, Clause1) -> Clause3=Clause2;   %
     lggI(Clause1, Clause2, [], Clause3, _)),         % otherwise, compute unreduced lgg
    reduceClause(Clause3, Result).                    % always reduce resulting clause


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lgg computation step 1 (no reduction yet) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% compute the lgg of each atom of first clause with each atom of second clause;
% include all defined lggs in the result
lggI([], Clause, Varlist, [], Varlist).
lggI([A|RClause], Clause2, Varlist, Clause3, NewVarlist) :-
	lggI2(A, Clause2, Varlist, AClause, Varlist2),
	append(AClause, Clause4, Clause3),
	lggI(RClause, Clause2, Varlist2, Clause4, NewVarlist).

% compare one atom with all atoms in second clause
lggI2(Atom, [], VL, [], VL) :- !.
lggI2(Atom, [A|Rest], Varlist, NewClause, NewVarlist) :-
	lggAtomI(Atom, A, Varlist, A2, VL2) 
	-> NewClause=[A2|R2], lggI2(Atom, Rest, VL2, R2, NewVarlist)
	; lggI2(Atom, Rest, Varlist, NewClause, NewVarlist).

% compute lgg of two atoms
lggAtomI(not(X), not(Y), Varlist, not(Z), NewVarlist) :-
	!,
	X =.. [PredX|ArgsX], 
	Y =.. [PredY|ArgsY],
	PredX = PredY,
	lggTermsI(ArgsX, ArgsY, Varlist, ArgsZ, NewVarlist),
	Z =.. [PredX | ArgsZ].
lggAtomI(X, Y, Varlist, Z, NewVarlist) :-  
	X =.. [PredX|ArgsX], 
	Y =.. [PredY|ArgsY],
	PredX = PredY,
	lggTermsI(ArgsX, ArgsY, Varlist, ArgsZ, NewVarlist),
	Z =.. [PredX | ArgsZ].

lggTermsI([], [], Varlist, [], Varlist).
lggTermsI([T1|Rest1], [T2|Rest2], Varlist, [T3|Rest3], NewVarlist) :-
	lggTermI(T1, T2, Varlist, T3, Varlist2),
	lggTermsI(Rest1, Rest2, Varlist2, Rest3, NewVarlist).

lggTermI(X, Y, Varlist, Z, Varlist2) :- 
	(var(X); var(Y)), 
	!,
	(X == Y -> Z=X, Varlist2 = Varlist;
	 membervar(var(X,Y,Z), Varlist) -> Varlist2=Varlist; 
	 Varlist2 = [var(X,Y,Z)|Varlist]).
lggTermI(X, Y, Varlist, Z, Varlist2) :-
	X =.. [FX|AX], 
	Y =.. [FY|AY],
	FX = FY -> lggTermsI(AX, AY, Varlist, AZ, Varlist2), Z =.. [FX | AZ];
	membervar(var(X,Y,Z), Varlist) -> Varlist2=Varlist;
	Varlist2 = [var(X,Y,Z)|Varlist].

membervar(var(X,Y,Z), [var(A,B,C)|D]) :- X==A, Y==B, Z=C.
membervar(var(X,Y,Z), [_|D]) :- membervar(var(X,Y,Z), D).


%%%%%%%%%%%%%%%%%%%%%%%
% reduction of clause %
%%%%%%%%%%%%%%%%%%%%%%%

% implements "uniqueness propagation", see ILP 2016 short paper
% + optimization proposed by Maloberti & Suzuki
% + quick removal of literals that subsume something and don't share variables with anything
% this version assumes the original clause is free of '$VAR'(_) terms !

reduceClause(C, RedC) :- 
	constructUnique(C, 0, [], Unique, Rest),
	dropLitsWithUniqueVars(Rest, [], Unique, Rest2),
	redC(Rest2,Unique,RedC1),
	write_to_chars(RedC1,Charlist), read_from_chars(Charlist, RedC). %unnumbervars

constructUnique(C, N, PrevU, U, Rest) :- 
	findUnique(C, U1, Rest1), 
	(U1 = PrevU 
	 -> U=U1, Rest=Rest1
	 ; numbervars(U1, N, N1), constructUnique(C, N1, U1, U, Rest)
	).

findUnique([], [], []).
findUnique([A|B], Unique, Rest) :-
	getEquiv(A, B, Unifs, RestB, Sub), 
	(Sub=false
	-> Unique = [A|U], append(Unifs, R, Rest), findUnique(RestB, U, R)
	; append([A|Unifs], R, Rest), findUnique(RestB, Unique, R)).

% returns Sub=true iff A subsumes at least one atom, + returns all atoms that subsume A - those certainly aren't unique
getEquiv(A, [], [], [], false)  :- !.
getEquiv(A, [B|C], Unifs, RestB, Sub) :-
	subsumes_term(A,B)
	-> Sub=true, 
	   (subsumes_term(B,A) -> Unifs=[B|U], RestB=RestC; Unifs=U, RestB=[B|RestC]),
	   getEquiv(A, C, U, RestC, _)
	; (subsumes_term(B,A) -> Unifs=[B|U], RestB=RestC; Unifs=U, RestB=[B|RestC]),
	  getEquiv(A, C, U, RestC, Sub).

dropLitsWithUniqueVars([], Earlier, Unique, Earlier).
dropLitsWithUniqueVars([A|Later], Earlier, Unique, Out) :-
	noSharedVars(A,Earlier), noSharedVars(A,Later), \+ \+ member(A, Unique)  
	-> dropLitsWithUniqueVars(Later, Earlier, Unique, Out)
	; dropLitsWithUniqueVars(Later, [A|Earlier], Unique, Out).

  noSharedVars(A, L) :- term_variables(A, VA), term_variables(L, VL), varsDisjoint(VA, VL).

  varsDisjoint([], _).
  varsDisjoint([A|B], L) :- \+ (member(X, L), X == A), varsDisjoint(B, L).


% Gottlob's method: check each literal once, to see if it can be removed
% + Maloberti & Suzuki's optimization: immediately remove all literals affected by the found substitution

redC([], Necessary, Necessary).
redC([A|B], Necessary, RedC) :-
	append(Necessary, B, All),
	(subsumes([A|All], All, Subst)
		-> removeAffected(B, Subst, B2), redC(B2, Necessary, RedC)
		; redC(B, [A|Necessary], RedC)).

removeAffected([], _, []).
removeAffected([Lit|Rest], Subst, Rest2) :- 
	affected(Lit, Subst) 
	-> removeAffected(Rest, Subst, Rest2)
	; Rest2=[Lit|Rest3], removeAffected(Rest, Subst, Rest3).

affected(Lit, Subst) :- term_variables(Lit, TV), member(X/Y, Subst), memberID(X, TV), !.

memberID(Var, [A|B]) :- Var==A -> true; memberID(Var, B).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART 2 : theta-subsumption testing %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% method for testing whether C subsumes D: 
% first skolemize D and divide C into independent components
% then, for each independent component C:
% CONSTRUCT DB:
% for each literal Lit in C, 
% . construct a tuple of variables as they occur in Lit
% . try to match Lit with all literals in D, keep a list of all instantiations thus obtained (= VIL)
% . example:  [f(X,Y)] / [f(a,b), f(a,c), f(d,e)]  lists possible instantiations of tuple X,Y
% . if the variables in this tuple are a subset of those in another variable tuple, or vice versa: 
%   semi-join superset tuple with subset tuple, drop subset tuple
% . if the variables in this tuple overlap with those of another tuple: replace both by their semijoin with the other
% SIMPLIFY:
% for all pairs of tuples with overlapping variables: semi join their VILS; repeat until no change
% SEARCH:
% among all vartuples, choose the one with shortest VIL / most vars
% for each member of this VIL:
% . unify vartuple with it
% . filter other VILs: remove inconsistent members
% . if any VIL becomes empty, fail
% . if no free vars remain, succeed
% . otherwise, call SEARCH recursively

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% theta-subsumption and equivalence tests %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equiv(X,Y) :- subsumes(X,Y,_), subsumes(Y,X,_).

subsumes(X, Y) :- subsumes(X, Y, _).

subsumes(X, Y, Sub) :- 
	copyTerm(X, X2, Sub1, _), copyTerm(Y, Y2, _, Sub2),
	skolemize(Y2), 
	makeCC(X2, CCList), 
	processCC(CCList, Y2),
	composeSubst(Sub1, Sub2, Sub).

    % this version of copy_term returns the mapping between the variables, in both directions
    copyTerm(Term, Copy, Sub, InvSub) :- 
	    copy_term(Term, Copy), term_variables(Term, V1), term_variables(Copy, V2),
	    makeSubst(V1, V2, Sub, InvSub).

    makeSubst([], [], [], []).
    makeSubst([V|VRest], [I|IRest], Sub, InvSub) :- 
	    V==I
	    -> makeSubst(VRest, IRest, Sub, InvSub)
	    ; Sub=[V/I|VIR], InvSub = [I/V|IVR], makeSubst(VRest, IRest, VIR, IVR).

    % assumes I is always instantiated!  this is true where itâ€™s called in our code
    composeSubst([], _, []).
    composeSubst([V/I|Rest], Subs, NewSubst) :- 
		(member(I/J, Subs)  
			-> (J == V -> NewSubst=Rest2; NewSubst=[V/J|Rest2])
			; NewSubst=[V/I|Rest2]),
		composeSubst(Rest, Subs, Rest2).


%%%%%%%%%%%%%
% skolemize %
%%%%%%%%%%%%%

% skolemize uses numbervars, but avoids use of $VAR(N) terms that already occur

skolemize(C) :- nextNumber(C, N), numbervars(C, N, _).

    nextNumber(C, N) :- findall(X, inside('$VAR'(X), C), List), (max_list(List, Max) -> N is Max+1; N=0).

    inside(X, C) :- var(C), !, fail; C=X; C =.. [F|Args], insideList(X, Args).

    insideList(X, [A|B]) :- inside(X,A); insideList(X, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partition a list into maximal sublists such that sublists do not share variables %
% - used for partitioning clauses and tables into independent components           %
% - a list of excluded variables can be provided, when you know these will be      %
%   instantiated but can't instantiate them yet - used when partitioning tables    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

makeCC(Clause, CC) :- makeCC(Clause, [], CC).

makeCC([], Excl, []).
makeCC([A|B], Excl, [CC|Rest]) :- oneCC(B, [A], Excl, CC, Unlinked), makeCC(Unlinked, Excl, Rest).

    oneCC([], Prev, Excl, Prev, []) :- !.
    oneCC(Clause, Prev, Excl, CC, Rest) :-
        gatherLinkedLits(Clause, Prev, Excl, Linked, Unlinked),
        (Linked = []
         -> CC=Prev, Rest=Unlinked
         ; append(Linked, Prev, NewPrev), oneCC(Unlinked, NewPrev, Excl, CC, Rest)).

    gatherLinkedLits([], Prev, _, [], []).
    gatherLinkedLits([A|Rest], Prev, Excl, Linked, Unlinked) :-
        gatherLinkedLits(Rest, Prev, Excl, L1, U1),
        (linked(A, Prev, Excl) -> Linked = [A|L1], Unlinked=U1; Linked=L1, Unlinked=[A|U1]).

    linked(A, List, Excluded) :- 
        term_variables(A, Vars1), sort(Vars1, SVars1),
        term_variables(List, Vars2), sort(Vars2, SVars2),
        intersect(SVars1, SVars2, Shared),
        \+ subset(Shared, Excluded).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solve each independent component independently %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processCC([], _).
processCC([CC|Rest], GroundClause) :- 
	makeTable(CC, GroundClause, [], Table),   % makeTable can fail => no solutions
	simplifyTable(Table,STable),              % simplifyTable can fail => no solutions
	solve(STable), !,                         % solve can fail => no solutions
	processCC(Rest, GroundClause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the table with all VILs %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this version semijoins rows already while creating the table,
% instead of first creating the full table and then reducing it

% for Django benchmarks, solve is rarely called (<5%): simplify usually fails!  So making this more efficient should help a lot.
% unfortunately, early pruning (semi-joining while making the table) doesn't seem to help much

makeTable([], _, Acc, Acc).
makeTable([A|B], GrClause, Acc, SimpleTable) :-
    filter(A, GrClause, IL),
    term_variables(A, Vars),
    sort(Vars, SV), A2 =.. [f|SV],
    findall(A2, member(A, IL), InstL),
    sort(InstL, IL2),
    checkRow(row(A2,IL2), Acc, Row2, Acc2),
    (Row2==drop -> NewAcc=Acc2; 
     Row2 = row(Vars2, [Vars2]) -> NewAcc=Acc2;   % 2.2 optimization: if only 1 value for vars, ground vars and don't add row
     NewAcc=[Row2|Acc2]),
    makeTable(B, GrClause, NewAcc, SimpleTable).


filter(Lit, [], []) :- !.
filter(Lit, [A|B], C) :- Lit \= A -> filter(Lit, B, C); C=[A|C1], filter(Lit, B, C1).

%%%%%%%%%%%%%%%%%%%%%%
% simplify the table %
%%%%%%%%%%%%%%%%%%%%%%

simplifyTable(Table, NewTable) :-
	simpTab(Table, Table2), 
	(Table == Table2 -> NewTable=Table2; simplifyTable(Table2, NewTable)).

simpTab([], []).
simpTab([drop|Rest], NewTable) :- !, simpTab(Rest, NewTable).   % skip dropped rows
simpTab([Row|Rest], NewTable) :-
	checkRow(Row, Rest, Row2, Rest2),
	(Row2==drop -> simpTab(Rest2, NewTable);
	 NewTable=[Row2|Rest3], simpTab(Rest2, Rest3)).

% semi join row with all other rows in the database
% if X subset of X': stop semijoining X, since X' will have same effect later on

checkRow(Row, [], Row, []) :- !.
checkRow(Row, [drop|Rest], NewRow, NewRest) :- !, checkRow(Row, Rest, NewRow, NewRest).  % skip dropped row
checkRow(Row, Rest, NewRow, NewRest) :-
	Rest=[Row2|Rest2],
	sjrow(Row, Row2, InterRow, NewRow2),   % drop,NewRow2; InterRow, drop; InterRow, NewRow2
	(InterRow==drop 
	-> NewRow=drop, NewRest=[NewRow2|Rest2]  % stop pruning with this row: will be caused by NewRow2
	; NewRest=[NewRow2|NewRest2], 
	  checkRow(InterRow, Rest2, NewRow, NewRest2)).

% if semi join finds an empty instantiation list, it should fail
sjrow(row(Vars1,Inst1), row(Vars2, Inst2), Row1, Row2) :-
	Vars1 == Vars2 -> intersect(Inst1, Inst2, Inst), Inst \= [], Row1=drop, Row2=row(Vars1, Inst); % fail if [], i.e. no solution 
	Vars1 =.. [f|VarList1], Vars2 =.. [f|VarList2], 
	intersect(VarList1, VarList2, VarIntersect), 
	(VarIntersect = [] -> Row1=row(Vars1,Inst1), Row2=row(Vars2,Inst2); % vars are disjoint: no change
	VarsInt =.. [f|VarIntersect],
	filterboth(Vars1/Inst1, Vars2/Inst2, VarsInt, NewInst1, NewInst2),
	 NewInst1 \= [], NewInst2 \= [],  % fail if no solution 
         (NewInst1 = [Vars1] -> Row1=drop; true),   % *** this line and below: now optimize for singleton instance lists (cf. 2.1)
         (NewInst2 = [Vars2] -> Row2=drop; true),   
         (subset(VarList1, VarList2), Row2 \== drop -> Row1 = drop, Row2 = row(Vars2, NewInst2); 
          subset(VarList2, VarList1), Row1 \== drop -> Row1 = row(Vars1,NewInst1), Row2=drop;   
          (Row1 \== drop -> Row1 = row(Vars1,NewInst1); true),   
          (Row2 \== drop -> Row2 = row(Vars2,NewInst2); true))). 


filterboth(Vars1/Inst1, Vars2/Inst2, VarsInt, Result1, Result2) :-
	dbProject(Vars1/Inst1, VarsInt/I1), sort(I1, SI1), 
	dbProject(Vars2/Inst2, VarsInt/I2), sort(I2, SI2), 
	intersect(SI1, SI2, SI),
	(length(SI, Len), Len>1000    % depending on the length of Len, use a simple or advanced lookup mechanism
	 -> list2tree(Len, SI, SIT), dbSelect2(Vars1/Inst1, VarsInt=SIT, Result1), dbSelect2(Vars2/Inst2, VarsInt=SIT, Result2)
 	 ; dbSelect(Vars1/Inst1, VarsInt=SI, Result1), dbSelect(Vars2/Inst2, VarsInt=SI, Result2)).

% project Vars1/Inst1 onto Vars2/Inst2 -  must be in f-format
dbProject(Vars1/Inst1, Vars2/Inst2) :- findall(Vars2, member(Vars1, Inst1), InstM), sort(InstM, Inst2).

% select tuples from Inst1 where Vars2=Inst2  - all must be in f-format
dbSelect(Vars1/Inst1, Vars2=Inst2, Result) :- findall(Vars1, (member(Vars1, Inst1), memberchk(Vars2, Inst2)), Sel), sort(Sel, Result).

% advanced version of dbSelect: uses a binary search tree instead of linear search - speeds up lookups in longer lists
dbSelect2(Vars1/Inst1, Vars2=Inst2, Result) :- findall(Vars1, (member(Vars1, Inst1), occursInBST(Vars2, Inst2)), Sel), sort(Sel, Result).

% change a sorted list of f(...) terms into a binary search tree
list2tree(0, [], []) :- !.
list2tree(1, [A], A) :- !.
list2tree(2, [A,B], tree(A,B,[])) :- !.
list2tree(3, [A,B,C], tree(A,B,C)) :- !.
list2tree(N, List, tree(LeftT, Mid, RightT)) :-
	H is N div 2, H2 is H-1+(N mod 2),
	divList(List, H, Left, Mid, Right), 
	list2tree(H, Left, LeftT), list2tree(H2, Right, RightT).

    divList([A|B], N, Left, Mid, Right) :- N>0 -> N1 is N-1, Left = [A|Rest], divList(B, N1, Rest, Mid, Right) ; Left=[], Mid=A, Right=B.

% lookup in binary search tree
occursInBST(N, N) :- !.
occursInBST(N, tree(Left, Mid, Right)) :- N @< Mid -> occursInBST(N, Left); N @> Mid -> occursInBST(N, Right); N=Mid.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% union, intersection and subset for sorted varlists %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subset([], _).
subset([A|B], [C|D]) :- A==C -> subset(B,D); A @> C, subset([A|B],D).

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([A|B], [C|D], Result) :-
	A @< C -> Result=[A|Rest], merge(B, [C|D], Rest);
	A @> C -> Result=[C|Rest], merge([A|B], D, Rest);
	Result=[A|Rest], merge(B, D, Rest).

intersect([], L, []) :- !.
intersect(L, [], []) :- !.
intersect([A|B], [C|D], Result) :-
	A @< C -> intersect(B, [C|D], Result);
	A @> C -> intersect([A|B], D, Result);
	Result=[A|Rest], intersect(B, D, Rest).



%%%%%%%%%%%%%%%%%%%
% solve the table %
%%%%%%%%%%%%%%%%%%%

% settings for the solver
setting(simplify_in_search, no).
setting(recursive_decomp, no).

tableSize([], 0).
tableSize([row(_,I)|B], TS) :- length(I, Ln), tableSize(B, TS1), TS is Ln+TS1.

solve(Table) :- 
	(setting(simplify_in_search, yes) -> simplifyTable(Table, Stable); Stable=Table),
	getBest(Stable, Best, RestTable),  
	(Best = row(Lit, Instlist) -> 
		(setting(recursive_decomp, no)
		 ->
		 member(Lit, Instlist), filterTable(RestTable, RT2), solve(RT2)
		 ;
		 Lit=..[_|Args], makeCC(RestTable, Args, ICRT), member(Lit, Instlist), 
		 solveAll(ICRT))  
	; true).

  solveAll([]).
  solveAll([Table|MoreTables]) :-
	filterTable(Table, FTable), 
	solve(FTable),
	!,
	solveAll(MoreTables).

filterTable([],[]).
filterTable([row(Lit,Instlist)|Rest], [row(Lit,IL2)|Rest2]) :-
	filter(Lit, Instlist, IL2),
	IL2 \= [],
	filterTable(Rest, Rest2).

% getBest finds the highest quality row in the table and returns that row and the rest of the table
% returns none if no variables remain => solution found
% fails if an empty instlist was found

getBest(Table, Best, RestTable) :- gb1(Table, none, 0, Best, RestTable).

gb1([], PrevBest, PrevQ, PrevBest, []).
gb1([row(Lit, Instlist)|Rest], PrevBest, PrevQ, Best, RestTable) :-
	quality(Lit, Instlist, Q),
	(Q > PrevQ 
         -> NewBest=row(Lit,Instlist), NewQ=Q, (PrevBest=none -> RestTable=RT; RestTable=[PrevBest|RT])
         ; NewBest=PrevBest, NewQ=PrevQ, RestTable=[row(Lit,Instlist)|RT]),
	gb1(Rest, NewBest, NewQ, Best, RT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% quality returns 0 if the tuple contains no vars,                        %
% fails if Instlist is empty,                                             %
% and returns a positive number otherwise (higher means better candidate) %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quality(Lit, [], _) :- !, fail.
quality(Lit, Instlist, Q) :- 
	Lit =.. [F|Args], 
	freevars(Args, A), 
	(A=0 -> Q=0; length(Instlist, L), L>0, Q is 5^A/L).   % was: L>0, (L=1 -> Q=100000; Q is 5^A/L)).

    freevars([A|B], N) :- freevars(B, N1), (var(A) -> N is N1+1; N=N1).
    freevars([], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                 %
%                                  some unit tests                                %
%                                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unittest(sub-1, subsumes([p(a),p(X)], [p(a)])).
unittest(sub-2, subsumes([p(X),q(X,Y)], [p(a),q(a,c)])).
unittest(sub-3, \+ subsumes([p(a),p(X)], [p(X)])).
unittest(sub-4, \+ subsumes([p(X),q(X,Y)], [p(a),q(b,c)])).
unittest(sub-5, \+ subsumes([p(X),p(Y),q(X,Y)], [p(a),q(a,b)])).
unittest(sub-6, subsumes([a, b(X), c(Y,X)], [a, b(1), b(2), b(3), c(1,2), c(5,4), c(7,1)])).
unittest(sub-7, \+ subsumes([a, b(X), c(Y,X)], [a, b(1), b(2), b(3), c(1,5), c(5,4), c(1,4)])).
unittest(sub-8, \+ subsumes([p(X,Y),p(Y,Z),p(Z,U),p(U,V)], [p(1,2), p(2,3), p(3,4)])).
unittest(sub-9, subsumes([p(X,Y),p(Y,Z),p(Z,U),p(U,V)], [p(1,2), p(2,3), p(2,1)])).
unittest(sub-10, subsumes([p(X,Y),p(Y,Z),p(Z,U),p(U,V)], [p(1,1)])).
unittest(sub-11, subsumes([p(a(X),Y)], [p(a(1),a(2))])).
unittest(sub-12, \+ subsumes([p(a(X),X)], [p(a(1),a(1))])).
unittest(sub-13, subsumes([p(X), not(Z), not(q(Y))], [p(A), not(q(B)), not(q(C))])).
unittest(sub-14, \+ subsumes([p(X), not(q(X)), not(q(Y))], [p(A), not(q(B)), not(q(C))])).



unittest(lgg-1, ( lgg([p(a), p(b)], [p(b),p(c)], Result), Result=[p(b)] )).
unittest(lgg-2, ( lgg([p(a,b), p(b,c)], [p(b,c),p(c,d)], Result), equiv(Result, [p(X,Y), p(Y,Z), p(b,c)]) )).
unittest(lgg-3, ( lgg([p(X,a), p(Y,b)], [p(X,c),p(X,d)], Result), equiv(Result, [p(X,Y)]) )).
unittest(lgg-4, ( lgg([p(X,a), p(Y,b)], [p(X,c),p(X,b)], Result), equiv(Result, [p(X,b)]) )).
unittest(lgg-5, ( lgg([p(Y), not(q(X)), not(q(Y))], [p(2), not(q(1)), not(p(2))], Result), equiv(Result, [not(q(X)), p(Y)]) )).
unittest(lgg-6, ( lgg([p(Y), not(q(X)), not(q(Y))], [p(2), not(q(1)), not(q(2))], Result), equiv(Result, [not(q(X)), p(X)]) )).

ut :- time( (unittest(Name, Test), write(Name), (Test -> writeln(' succeeds'); writeln(' fails ***********************')), fail)).
