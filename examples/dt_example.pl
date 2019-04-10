:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').

% Load the spreadsheet
%magic:X :- load_spreadsheet('magic.xlsx', X).

% Get the tables
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Train a classifier
magic_models:X :-decision_tree(magic_tables,
                                [column('T1', 2), column('T1', 3)],
                                [column('T1', 4)],
                                X).



%query(magic_models:_).

magic_predict:X :- magic_models:predictor(Y), predict(magic_tables, Y, [column('T1', 2), column('T1', 3)], X).

res_src(X):-magic_predict:prediction(Z), magic_predict:source(Z, X).
% All prediction objects
query(res_src(_)).

%res(X):-magic_predict:prediction(X).
%query(res(_)).