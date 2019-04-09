:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).


%%%% Collect scope predicates %%%%
collect_raw(CodeBlock, GroupBy, AggVar, AggRes) :-
   aggregate(raw, AggVar, GroupBy, CodeBlock, (GroupBy, AggRes)).
raw(X,X).

%%%% Collect all predicates in scope Scope %%%%
collect_scope(Scope, X) :- (Scope:Y) => Scope / raw(Y, X).
%%%% Collect only predicates in scope Scope with name Pred %%%%
collect_scope(Scope, Pred, X) :- (Scope:Y, Y =.. [Pred|_]) => Scope / raw(Y, X).

detect_tables(Scope, cells(X)) :- collect_scope(Scope, cell, X).





magic:cell(1,1,1,1,1).
magic:cell(2).
magic:test(1).

spreadsheet(magic, tables):X :- detect_tables(magic, X).

query(spreadsheet(magic, tables):_).