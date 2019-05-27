:- use_module('clause.py').

% Clause parsing
:- Scope:Z, apply_clause(Scope,Z).
%P::Scope:X :- contains_clauses(Scope), Scope:Z, Z \== X, unify_clause(Z,X,P,Y), evaluate(Scope,Y).

contains_clauses(dummyscopeble).

% Evaluation rules
evaluate(Scope,Y) :- Scope:Y.
evaluate(Scope,var(Y)) :- Scope:Y.
%% \+ Y
evaluate(Scope, \+ Y) :- \+ evaluate(Scope, Y). % (evaluate(Scope,Y), store_existing_term(Y)); is_not_existing_term(Y).
evaluate(Scope, not(Y)) :- evaluate(Scope, \+ Y).
%% X < Y
evaluate(Scope,(X < Y)) :- ground(X), ground(Y), X < Y.
%% X <= Y
evaluate(Scope,(X =< Y)) :- ground(X), ground(Y), X =< Y.
%% X > Y
evaluate(Scope,(X > Y)) :- ground(X), ground(Y), X > Y.
%% X >= Y
evaluate(Scope,(X >= Y)) :- ground(X), ground(Y), X >= Y.
%% X == Y
evaluate(Scope, (X == Y)) :- ground(X), X == Y.
evaluate(Scope, (X == Y)) :- ground(Y), X == Y.
%% X \== Y
evaluate(Scope, (X \== Y)) :- ground(X), ground(Y), X \== Y.
%% X is Y
evaluate(Scope,(X is Y)) :- ground(Y), X is Y.
%% X, Y
evaluate(Scope,(X,Y)) :- evaluate(Scope,X), evaluate(Scope,Y).
evaluate(Scope,(X,Y)) :- evaluate(Scope,Y), evaluate(Scope,X).
%% X; Y
evaluate(Scope,(X;Y)) :- evaluate(Scope,X); evaluate(Scope,Y).
evaluate(Scope,(X;Y)) :- evaluate(Scope,Y); evaluate(Scope,X).