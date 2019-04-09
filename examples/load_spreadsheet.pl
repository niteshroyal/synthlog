:- use_module('../synthlog/spreadsheet.py').

test:X :- load_spreadsheet('../data/test.xlsx', X).

query(test:_).