:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').
:- use_module('../synthlog/mercs.py').
:- use_module(library(aggregate)).
:- use_module(library(lists)).

% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Learn a simple predictor
magic_models_1:X :-decision_tree(magic_tables, [column('T1', 2), column('T1', 3)], [column('T1', 4)], X).
magic_models_1:X :-decision_tree(magic_tables, [column('T1', 2), column('T1', 3)], [column('T1', 5)], X).

magic_models_2:X :-decision_tree(magic_tables, [column('T1', 5)], [column('T1', 4)], X).

list(X,X).

% Get dt
% my_dt(list<Z>):-magic_models:decision_tree(Z), writeln(Z).
query(magic_models_1:decision_tree(_)).
query(magic_models_2:decision_tree(_)).

% All inputs of dt
% src_dt(Z):-magic_models:source(H, Z), my_dt([H|T]), writeln(H).
% query(src_dt(_)).

% Learn a MERCS model
magic_models:X :-mercs(magic_tables,
                       [column('T1', 2),
                        column('T1', 3),
                        column('T1', 4),
                        column('T1', 5)],
                       X).
% Examine scope
% query(magic_models:_).

% Get all predictors in your scope
my_predictors(Z):-magic_models:predictor(Z).
% query(my_predictors(_)).

% Get mercs
my_mercs(Z):-magic_models:mercs(Z).
% query(my_mercs(_)).

% All inputs of mercs
src_mercs(Z):-my_mercs(X), magic_models:source(X, Z).
% query(src_mercs(_)).

% Get dt
my_dt(Z):-magic_models:decision_tree(Z).
% query(my_dt(_)).

% All inputs of dt
src_dt(Z):-my_dt(X), magic_models:source(X, Z).
% query(src_dt(_)).


% Predict using the MERCS model

% Pass parameters to the MERCS prediction

% Extract single predictors from MERCS

% Do predictions with predictors extracted from MERCS

