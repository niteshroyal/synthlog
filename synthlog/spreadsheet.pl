:- use_module('spreadsheet.py').

wrong :- S:table_cell(T, X, Y, V2), S:table_cell(T, X, Y, V1), V1 \== V2. % no more than 1 prediction per table for each x,y
wrong :- possible(S:table_cell(T, X, Y, _)), \+ S:table_cell(T, X, Y, V). % at least one pred per cell

detect_tables(Scope, Term) :- findall((P::cell(X,Y,Z)), subquery(Scope:cell(X,Y,Z), P), List), detect_cell_tables(List, Term).