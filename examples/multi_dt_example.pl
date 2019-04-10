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
