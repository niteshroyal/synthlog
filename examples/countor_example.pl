:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/countor_wrapper.py').

nurse_cells:X :- load_csv('../data/nurse_new_format.csv', X).
nurse_tables:X :- detect_tables(nurse_cells, X).

%query(nurse_tables:_).

% Train a classifier
    
nurse_constraints:X :-learn_count_or_constraints(nurse_tables, X).

query(nurse_constraints:_).