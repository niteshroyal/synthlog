:- use_module('spreadsheet.py').

dummy_scope:table('T',1,1,1,1).
dummy_scope:table_cell('T',1,1,1).

wrong :- S:table_cell(T, X, Y, V2), S:table_cell(T, X, Y, V1), V1 \== V2. % no more than 1 prediction per table for each x,y
ens_true :- train_tables:table(T, _, _, MaxX, MaxY), between(1, MaxX, X), between(1, MaxY, Y), train_tables:table_cell(T, X, Y, V). % at least one pred per cell

detect_tables(Scope, Term) :- findall((P::cell(X,Y,Z)), subquery(Scope:cell(X,Y,Z), P), List), detect_cell_tables(List, Term).
evidence(ens_true).