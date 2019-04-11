:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Train a classifier
magic_models:X :-decision_tree(magic_tables,
                                [column('T1', 2), column('T1', 3)],
                                [column('T1', 4)],
                                X).

magic_models:X :-decision_tree(magic_tables,
                                [column('T1', 2), column('T1', 5)],
                                [column('T1', 3)],
                                X).

% Collect all predictors
query(magic_models:predictor(_)).

% All sources of predictors
all_sources(Z) :- magic_models:predictor(X), magic_models:source(X, Z).
query(all_sources(_)).

% All sources of predictors
all_targets(Z) :- magic_models:predictor(X), magic_models:target(X, Z).
query(all_targets(_)).

% Get predictor that predicts target 4
target_4_predictors(X) :- magic_models:target(X,column('T1',4)).
query(target_4_predictors(_)).

% Get predictors that have as input column 5
column_5_predictors(X) :- magic_models:source(X, column('T1', 5)).
query(column_5_predictors(_)).

% Get predictors that have as input column 2
column_2_predictors(X) :- magic_models:source(X, column('T1', 2)).
query(column_2_predictors(_)).


% Do a prediction
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 3)], X).
query(magic_predict:prediction(_)).

% Get source columns from prediction
source_column_pred(X) :- magic_predict:prediction(Y), magic_predict:source(Y, X).
query(source_column_pred(_)).
% Should get [column('T1', 2), column('T1', 3)]

% Get target columns of prediction
target_columns_pred(X) :- magic_predict:prediction(Y), magic_predict:predictor(Y, Z), magic_models:target(Z,X).
query(target_columns_pred(_)).
% We expect to get column 3 and 4


