:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/predict.py').
:- use_module('../synthlog/mercs.py').
:- use_module(library(aggregate)).
:- use_module(library(lists)).

% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

% Learn a MERCS model
magic_models:X :-white_mercs(magic_tables,
                       [column('T1', 2),
                        column('T1', 3),
                        column('T1', 4),
                        column('T1', 5)],
                       X).

% Examine scope
query(magic_models:_).

all_dts(T) :- magic_models:decision_tree(T).
all_mercs(M) :- magic_models:mercs(M).

src_dts(S) :- magic_models:decision_tree(T), magic_models:source(T, S).
src_mercs(S) :- magic_models:mercs(M), magic_models:source(M, S).

src_overlap(S) :- src_dts(S), src_mercs(S).
src_overlap_direct(S) :- magic_models:mercs(M), magic_models:source(M, S), magic_models:decision_tree(T), magic_models:source(T, S).

% query(src_overlap(_)). % Does work
% query(src_overlap_direct(_)). % Does not work
