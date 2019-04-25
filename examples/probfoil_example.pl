:- use_module('../synthlog/rules.py').

magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X).
%query(magic_tables:table(_,_,_,_,_)).
%query(magic_tables:table_header(_,_,_,_,_)).

%query(magic_tables:_).


% table(<table_name>, <start_row>, <start_col>, <height>, <width>)
% table_header(<table_name>, <table_column>, <table_header>), <header_type>)
% table_cell(<table_name>, <cell_row>, <cell_column>, <cell_value>)
% table_cell_type(<table_name>, <cell_row>, <cell_column>, <cell_type>)


% Stitch cell_types to table_atoms(<table_name>, <cell_header>, <cell_row>, <cell_value>, <cell_type>)
magic_tables:table_atoms(Table_name, Cell_header, Cell_row, Column_type, Column_unique_values, Cell_value) :-
    magic_tables:table(Table_name, Start_row, Start_col, Height, Width),
    magic_tables:table_header(Table_name, Cell_column, Cell_header, Column_type, Column_unique_values),
    magic_tables:table_cell(Table_name, Cell_row, Cell_column, Cell_value),
    magic_tables:table_cell_type(Table_name, Cell_row, Cell_column, Cell_type).
% query(magic_tables:table_atoms(_,_,_,_,_,_)).

% Convert the matrix of cells to atoms
magic_atoms:Atom :-
    magic_tables:table_atoms(Table_name, Cell_header, Cell_row, Column_type, Column_unique_values, Cell_value),
    cell_to_atoms(Table_name, Cell_header, Cell_row, Column_type, Column_unique_values, Cell_value, Atom).
%query(magic_atoms:_).

%P::magic_atoms1:X :- subquery(magic_atoms:X, P, []).
%P::magic_atoms1:X :- magic_atoms:P::X.
magic_atoms1:X :- magic_atoms:X, X =.. ['profit_yes'|_].
query(magic_atoms1:_).

% Learn probfoil rules for all atoms in the scope 'magic_atoms' with 'profit' as our target predicate
probfoil_rules:Rule :- probfoil(magic_atoms, 'profit', Rule).
probfoil_rules:Rule :- probfoil_loop(magic_atoms, 'country', Rule).
% query(probfoil_rules:blackbox_rule(_,_,_)).

% Unify the facts with the rules
rules_and_facts:X :- magic_atoms:X; probfoil_rules:X.
% query(rules_and_facts:_).

%:- parse_clause_from_term(Rule),rules_and_facts:blackbox_rule(Target, Rule_number, Rule).
rules:Rule :- probfoil_rules:blackbox_rule(Target, Rule_num, Rule), parse_clause(Rule). %, writenl('Parsed rule: ', Rule).
% query(rules:_).

%% Predict cells from rules
% Input:
%   Scope of independent cells : All cells - cells to be predicted
%   Scope of rules
%   Target_header
%   Table_name
%   Cell_row
%   Cell_column
% Output:
%   Predicted Cell Value

atoms_without_profit:X :-
    magic_atoms:X,
    \+ X =.. [profit|_],
    \+ X =.. [profit_yes|_],
    \+ X =.. [profit_no|_].

%profit:X :- magic_atoms:X, X =.. [profit|_].
%profit:profit(X) :- profit:profit(X, 'yes').
%0::profit:profit(X) :- profit:profit(X, 'no').
%atoms_with_profit:X :- profit:X ; atoms_without_profit:X.
%query(profit:_).

atoms_with_rules:X :- atoms_without_profit:X; rules:X.

atoms_with_rules:profit(A) :-
    atoms_with_rules:type_speculaas(A),
    \+atoms_with_rules:country_de(A),
    \+atoms_with_rules:country_nl(A).

P::profit(X) :- subquery(atoms_with_rules:profit(X), P, []), writenl('Probability of ', X, ':', P).
%query(atoms_with_rules:_).
%query(atoms_with_rules:profit(_)).
%query(profit(_)).

