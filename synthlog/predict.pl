:- use_module('predict.py').
Prob::predict(S, Pred, C, X) :- predict(S, Pred, C, X, Prob).

dummyblablascope:blablbadummyterm. % This is to avoid errors when including this file in problog program with no scope

wrong :- S:cell_pred(X, Y, V2, C), S:cell_pred(X, Y, V1, C), V1 \== V2. % no more than 1 prediction per classifier for each x,y
wrong :- possible(S:cell_pred(X, Y, _, C)), \+ S:cell_pred(X, Y, V, C). % at least one pred per cell

random_forest(Scope, Source, Target, X) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), random_forest(Scope, Source, Target, L, X).
predict(Scope, Pred, Cols, X) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), predict(Scope, Pred, Cols, L, X).