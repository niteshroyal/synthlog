:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').

% Load the spreadsheet
%magic:X :- load_spreadsheet('magic.xlsx', X).

% Get the tables
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

query(magic_tables:_).

% Train a classifier
magic_models:X :-decision_tree(magic_tables,
                                [column('T1', 2), column('T1', 3)],
                                [column('T1', 4)],
                                X).

query(magic_models:_).