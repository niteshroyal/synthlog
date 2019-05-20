:- use_module('../synthlog/learn_constraints.py').


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

magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 3)], X), magic_models:source(Y, column('T1', 2)), magic_models:source(Y, column('T1', 3)).
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 5)], X), magic_models:source(Y, column('T1', 2)), magic_models:source(Y, column('T1', 5)).

% This emulates an annotated disjunction
weight(X) :- magic_predict:source(C1, column('T1', 3)), magic_predict:confidence(C1, Conf1), magic_predict:source(C2, column('T1', 5)), magic_predict:confidence(C2, Conf2), select_weighted(1, [Conf1, Conf2], [C1, C2], X, _).

% We combine the prediction using the confidences
magic_predict:final_pred(X,Y,V):- weight(C1), magic_predict:source(C1, column('T1', 3)), magic_predict:cell_pred(X,Y,V,C1).
magic_predict:final_pred(X,Y,V):- weight(C2), magic_predict:source(C2, column('T1', 5)), magic_predict:cell_pred(X,Y,V,C2).

magic_constraints:X :- learn_dummy_constraint(X).
% The constraints returned by learn_dummy_constraint are:
% wrong :- magic_predict:final_pred(8,1, V), V < 310.
% 0.3::wrong :- magic_predict:final_pred(8,1, V), V < 600.

contains_clauses(magic_predict).
contains_clauses(magic_constraints).

magic_predict:X :- magic_constraints:X.

%query(magic_constraints:final_pred(_,_,_)).
query(magic_predict:_).
