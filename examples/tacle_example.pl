:- use_module('../synthlog/spreadsheet.py').

magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X)
