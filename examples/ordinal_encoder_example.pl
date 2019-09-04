:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(string)).
%:- use_module(library(scope)).
:- use_module('../synthlog/spreadsheet.pl').
:- use_module('../synthlog/transformers.pl').
:- use_module('../synthlog/utils.py').
:- use_module('../synthlog/predict.pl').
%:- use_module('../synthlog/clause.pl').
list(X,X).

wrong :- S:wrong.

evidence(\+wrong).


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

% We now apply the transformer on the test set
magic_cells(test):cell(X,Y,V) :- magic_cells:cell(X,Y,V), X>=7.

magic_tables(test):X :- detect_tables(magic_cells(test), X).
magic_cells(test, transformed_column):X :- magic_transforms:transformer(T), transform(magic_tables(test), T, [column('T1', 0)], X).

% We recreate a full table
magic_cells(test, transformed_column):table_cell('T1', X, Y,V) :- magic_cells(test, transformed_column):cell_transform(X,Y,V,_).
magic_cells(test, transformed_column):table_cell('T1', X, Y,V) :- magic_tables(test):table_cell('T1',X,Y,V), \+ magic_cells(test, transformed_column):cell_transform(X,Y,V2,_).

%In the test table, we get no String in column 0, only their number representation
query(magic_cells(test, transformed_column):table_cell(_, _, _, _)).

% We recreate a full table
magic_cells(test, original_column):X :- magic_transforms:transformer(T), inverse_transform(magic_cells(test, transformed_column), T, [column('T1', 0)], X).
magic_cells(test, original_column):table_cell('T1', X, Y,V) :- magic_cells(test, original_column):cell_transform(X,Y,V,_).
magic_cells(test, original_column):table_cell('T1', X, Y,V) :- magic_tables(test):table_cell('T1',X,Y,V), \+ magic_cells(test, original_column):cell_transform(X,Y,V2,_).
query(magic_cells(test, original_column):table_cell(_,_,_,_)).