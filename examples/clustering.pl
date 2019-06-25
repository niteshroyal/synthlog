%:- use_module(library(collect)).
%:- use_module(library(aggregate)).
%:- use_module(library(lists)).
%:- use_module(library(string)).
%:- use_module(library(scope)).
%:- use_module('../synthlog/spreadsheet.pl').
%:- use_module('../synthlog/transformers.pl').
%:- use_module('../synthlog/utils.py').
%:- use_module('../synthlog/predict.pl').
%:- use_module('../synthlog/clause.pl').
%list(X,X).

%wrong :- S:wrong.

%evidence(\+wrong).

magic_cells:X :- load_csv('../data/magic_train.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

magic_models:X :- kmeans(magic_tables,
                    [column('T1', 3), column('T1', 4)],
                    X).

magic_cluster:X :- magic_models:clustering(C), predict(magic_tables, C, [column('T1', 3), column('T1', 4)], X).


query(magic_models:_).
query(magic_cluster:_).