# :- use_module('../synthlog/spreadsheet.py').
# :- use_module('../synthlog/predict.py').


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Train a classifier
magic_models:X :-random_forest(magic_tables,
                               [column('T1', 2), column('T1', 3)],
                               [column('T1', 4)],
                               X).

% Train a second one
magic_models:X :-random_forest(magic_tables,
                               [column('T1', 2), column('T1', 5)],
                               [column('T1', 4)],
                               X).

% Do a probabilistic prediction
Prob::predict(S, Pred, C, X) :- predict(S, Pred, C, X, Prob).


magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 3)], X), magic_models:source(Y, column('T1', 2)), magic_models:source(Y, column('T1', 3)).
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 5)], X), magic_models:source(Y, column('T1', 2)), magic_models:source(Y, column('T1', 5)).

% We get the confidence of each classifier
confidence1 :- magic_predict:source(C1, column('T1', 3)), magic_predict:cell_pred(X,Y,V,C1).
confidence2 :- magic_predict:source(C2, column('T1', 5)), magic_predict:cell_pred(X,Y,V,C2).

% These 2 predictors should be in an annotated disjunction
% This part seems to work but I am not 100 pct sure of it....
\+ confidence2 :- confidence1.

% We combine the prediction using the confidences
magic_predict:final_pred(X,Y,V):- confidence1, magic_predict:source(C1, column('T1', 3)), magic_predict:cell_pred(X,Y,V,C1).
magic_predict:final_pred(X,Y,V):- confidence2, magic_predict:source(C2, column('T1', 5)), magic_predict:cell_pred(X,Y,V,C2).



query(magic_predict:final_pred(_,_,_)).
%query(magic_predict:cell_pred(_,_,_, _)).
