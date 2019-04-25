:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/transformers.py').
:- use_module('../synthlog/predict.py').


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).

magic_cells(train):cell(X,Y,V) :- magic_cells:cell(X,Y,V), X<7.
magic_tables(train):X :- detect_tables(magic_cells(train), X).

% Fit a transformer
magic_transforms:X :-ordinal_encoder(magic_tables(train),
                                [column('T1', 0)],
                                X).
% Use the transformer to transform the column
magic_cells(train, transformed_column):X :- magic_transforms:transformer(T), transform(magic_tables(train), T, [column('T1', 0)], X).
% We convert cell_transform into table_cells
magic_cells(train, transformed_column):table_cell('T1', X,Y,V) :- magic_cells(train, transformed_column):cell_transform(X,Y,V,_).
magic_cells(train, transformed_column):table_cell('T1', X,Y,V) :- magic_tables(train):table_cell('T1',X,Y,V), \+ magic_cells(train, transformed_column):cell_transform(X,Y,V2,_).

% Train a classifier
magic_models:X :-decision_tree(magic_cells(train, transformed_column),
                                [column('T1', 0), column('T1', 2)],
                                [column('T1', 3)],
                                X).

% We now apply the transformer on the test set
magic_cells(test):cell(X,Y,V) :- magic_cells:cell(X,Y,V), X>=7.

magic_tables(test):X :- detect_tables(magic_cells(test), X).
magic_cells(test, transformed_column):X :- magic_transforms:transformer(T), transform(magic_tables(test), T, [column('T1', 0)], X).

% We recreate a full table
magic_cells(test, transformed_column):table_cell('T1', X, Y,V) :- magic_cells(test, transformed_column):cell_transform(X,Y,V,_).
magic_cells(test, transformed_column):table_cell('T1', X, Y,V) :- magic_tables(test):table_cell('T1',X,Y,V), \+ magic_cells(test, transformed_column):cell_transform(X,Y,V2,_).

% Make a prediction
magic_predict:X :- magic_models:predictor(Y), predict(magic_cells(test, transformed_column), Y, [column('T1', 0), column('T1', 2)], X).

%In the test table, we get no String in column 0, only their number representation
%query(magic_predict:X).

% We recreate a full table
magic_cells(final):table_cell('T1', A,8,C) :- magic_predict:cell_pred(A,B,C,_).
magic_cells(final):table_cell(T,A,B,V1) :- magic_tables(test):table_cell(T,A, B,V1).%, \+ magic_predict:cell_pred(A,B,V2,_).
query(magic_cells(final):_).