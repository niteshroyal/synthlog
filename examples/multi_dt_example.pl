%:- use_module('../synthlog/spreadsheet.py').
%:- use_module('../synthlog/predict.py').


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Train a classifier
magic_models:X :-decision_tree(magic_tables,
                               [column('T1', 2), column('T1', 3)],
                               [column('T1', 4)],
                               X).

magic_models:X :-decision_tree(magic_tables,
                               [column('T1', 3), column('T1', 5)],
                               [column('T1', 4)],
                               X).

% Get all predictors in your scope
my_predictors(Z):-magic_models:predictor(Z).
% query(my_predictors(_)).

% All sources of these predictors
src_predictors(Z):-my_predictors(X), magic_models:source(X, Z).
% query(src_predictors(_)).

% All targets of these predictors
tgt_predictors(Z):-my_predictors(X), magic_models:target(X, Z).
% query(tgt_predictors(_)).

dt_one(X):- magic_models:decision_tree(X), magic_models:source(X, column('T1', 2)).
dt_two(X):- magic_models:decision_tree(X), magic_models:source(X, column('T1', 5)).

query(dt_one(_)).
query(dt_two(_)).

src_one(Z):- dt_one(X), magic_models:source(X, Z).
src_two(Z):- dt_two(X), magic_models:source(X, Z).
query(src_one(_)).
query(src_two(_)).

% Get all their sources
all_src(Z) :- src_one(Z), src_two(Z).
query(all_src(_)).