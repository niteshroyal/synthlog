% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

query(magic_tables:_).