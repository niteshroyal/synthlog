:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(string)).
%:- use_module(library(scope)).
:- use_module('spreadsheet.pl').
:- use_module('transformers.pl').
:- use_module('../synthlog/utils.py').
:- use_module('predict.pl').
%:- use_module('clause.pl').
list(X,X).

wrong :- S:wrong.

evidence(\+wrong).