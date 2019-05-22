:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
list(X,X).
:- use_module('spreadsheet.pl').
:- use_module('clause.pl').
:- use_module('../synthlog/utils.py').
:- use_module('predict.pl').
:- use_module('transformers.pl').

wrong :- S:wrong.

evidence(\+wrong).