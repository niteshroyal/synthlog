:- use_module('../synthlog/spreadsheet.py').
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



magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X).
query(magic_tables:_).
