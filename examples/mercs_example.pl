:- use_module('../synthlog/spreadsheet.py').
:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).

magic:X :- load_spreadsheet("magic.xlsx", X).

spreadsheet(magic, types):X :- detect_types(spreadsheet(magic, tables), X).

model(magic, mercsPredictors):X :- mercs(spreadsheet(magic, union, sales), X).

