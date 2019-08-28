:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(string)).
%:- use_module(library(scope)).
:- use_module('spreadsheet.pl').
:- use_module('transformers.pl').
:- use_module('utils.py').
:- use_module('predict.pl').
%:- use_module('clause.pl').
list(X,X).

:- use_module('inductive_db.py').
:- use_module('inductive_db.pl').

wrong :- S:wrong.

evidence(\+wrong).