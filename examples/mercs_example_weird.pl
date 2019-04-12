:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').
:- use_module('../synthlog/mercs.py').
:- use_module(library(aggregate)).
:- use_module(library(lists)).

% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Learn a MERCS model
magic_models:X :-mercs(magic_tables,
                       [column('T1', 2),
                        column('T1', 3),
                        column('T1', 4),
                        column('T1', 5)],
                       X).

magic_models:X :-decision_tree(magic_tables,
                               [column('T1', 2), column('T1', 5)],
                               [column('T1', 3)],
                               X).

magic_models:X :-random_forest(magic_tables,
                               [column('T1', 3), column('T1', 5)],
                               [column('T1', 3)],
                               X).

all_dts(T) :- magic_models:decision_tree(T).
all_mercs(M) :- magic_models:mercs(M).

src_rfs(S) :- magic_models:random_forest(T), magic_models:source(T, S).
src_dts(S) :- magic_models:decision_tree(T), magic_models:source(T, S).
src_mercs(S) :- magic_models:mercs(M), magic_models:source(M, S).

src_overlap(S) :- src_mercs(S), \+src_rfs(S).
% src_overlap(S) :- magic_models:mercs(M), magic_models:source(M, S), magic_models:decision_tree(T), magic_models:source(T, S).
query(src_overlap(_)).

query(magic_models:source(_,_)).
query(magic_models:decision_tree(_)).
% query(src_mercs(_)).
% query(src_dts(_)).
% query(src_overlap(_)).

% source2(A,B) :- magic_models:source(A,B).
% query(source2(_,_)).
% query(src_mercs(_)).
% query(src_overlap_direct(_)).
