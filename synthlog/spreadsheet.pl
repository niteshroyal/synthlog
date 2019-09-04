:- use_module('spreadsheet.py').

dummy_scope:table('T',1,1,1,1).
dummy_scope:table_cell('T',1,1,1).

wrong :- S:table_cell(T, X, Y, V2), S:table_cell(T, X, Y, V1), V1 \== V2. % no more than 1 prediction per table for each x,y
wrong :- train_tables:table(T, _, _, MaxX, MaxY), between(1, MaxX, X), between(1, MaxY, Y), possible(train_tables:table_cell(T, X, Y, V)), \+ train_tables:table_cell(T, X, Y, V). % at least one pred per cell

detect_tables(Scope, Term) :- findall((P::cell(X,Y,Z)), subquery(Scope:cell(X,Y,Z), P), List), detect_cell_tables(List, Term).
%evidence(ens_true).

%:- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), add_annotated_disj().