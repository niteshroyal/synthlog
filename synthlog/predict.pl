:- use_module('predict.py').
Prob::predict(S, Pred, C, X) :- predict(S, Pred, C, X, Prob).

dummyblablascope:blablbadummyterm. % This is to avoid errors when including this file in problog program with no scope

wrong :- S:cell_pred(X, Y, V2, C), S:cell_pred(X, Y, V1, C), V1 \== V2. % no more than 1 prediction per classifier for each x,y
wrong :- possible(S:cell_pred(X, Y, _, C)), \+ S:cell_pred(X, Y, V, C). % at least one pred per cell

sklearn_predictor(Scope, PredName, Source, Target, Y) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), sklearn_predictor(Scope, PredName, Source, Target, L, Y).
sklearn_clustering(Scope, PredName, Source, Y) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), sklearn_clustering(Scope, PredName, Source, L, Y).

random_forest(Scope, Source, Target, Y) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), random_forest(Scope, Source, Target, L, Y).
decision_tree(Scope, Source, Target, Y) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), decision_tree(Scope, Source, Target, L, Y).

kmeans(Scope, Source, Y) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), kmeans(Scope, Source, L, Y).

predict(Scope, Pred, Cols, Y, Prob) :- findall((P::X), (subquery(Scope:X, P), X=table_cell(_,_,_,_)), L), predict(Scope, Pred, Cols, L, Y, Prob).