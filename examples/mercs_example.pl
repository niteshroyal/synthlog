:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').
:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).

magic:X :- load_spreadsheet("magic.xlsx", X).

% Get the tables and types
spreadsheet(magic, tables): X :- detect_tables(magic, X).
spreadsheet(magic, types): X :- detect_types(spreadsheet(magic, tables), X).

% Construct the union
spreadsheet(magic, union):Term :- spreadsheet(magic, types):Term.
spreadsheet(magic, union):Term :- spreadsheet(magic, tables):Term.

% Learn a simple predictor
decision_tree(spreadsheet(magic, union, sales), [col0], model(magic, decisionTrees)).

% Use a simple predictor

% Learn a MERCS model

% Predict using the MERCS model

% Pass parameters to the MERCS prediction

% Extract single predictors from MERCS

% Do predictions with predictors extracted from MERCS

