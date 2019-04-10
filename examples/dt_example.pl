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

% Do a prediction
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 3)], X).

% Get all predictions in scope
my_predictions(Z):-magic_predict:prediction(Z). % All prediction objects
query(my_predictions(_)).

% Get source columns of all these predictions
prediction_src(Z):-my_predictions(X), magic_predict:source(X, Z). % Sources of all the prediction objects
query(prediction_src(_)).

% All target columns of your predictions (you have to look back at its predictor)
prediction_tgt(Z):-my_predictions(X), magic_predict:predictor(X, Y), magic_models:target(Y, Z).
query(prediction_tgt(_)).


% Get all predictors in your scope
my_predictors(Z):-magic_models:predictor(Z).
query(my_predictors(_)).

% All targets of these predictors
tgt_predictors(Z):-my_predictors(X), magic_models:target(X, Z).
query(tgt_predictors(_)).

% Get all the predictors (in your scope) that predict a certain target column
relevant_predictors(Z):- magic_models:target(Z, column('T1', 4)).

query(relevant_predictors(_)).
