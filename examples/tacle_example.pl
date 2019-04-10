:- use_module('../synthlog/spreadsheet.py').

magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X) ; magic_cells:X.
% query(magic_tables:_).

magic_constraints:X :- tacle(magic_tables, X).
query(magic_constraints:_).


magic_cells_error:X :- load_csv('../data/magic_ice_cream_error.csv', X).
magic_tables_error:X :- detect_tables(magic_cells_error, X) ; magic_cells_error:X.
magic_constraints_error:X :- tacle(magic_tables_error, X).
query(magic_constraints_error:_).

%magic_diff:X :- scope_minus(magic_constraints, magic_constraints_error, X).
%query(magic_diff:_).
