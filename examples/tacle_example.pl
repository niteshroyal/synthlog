:- use_module('../synthlog/spreadsheet.py').
:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).


magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X).
query(magic_tables:_).
