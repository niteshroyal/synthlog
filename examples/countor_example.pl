:- use_module('../synthlog/inductive_db.py').
:- use_module('../synthlog/matrix.py').
:- use_module('countor_wrapper.py').

:- sqlite_load('nurse.db').
:- excel_into_sqlite('../data/nurse.xlsx', 'nurse.db').

nurse(M) :- init_matrix(M), load_blocks(M, 'nurse.db', 1, 1, 1:14, 1:22).
constraint(C) :- nurse(M), learn_count_or_constraints(M, C).
a(1) :- constraint(C), save_count_or_constraint('nurse.db', 1, 1, C).
constraint_load(C) :- load_count_or_constraints(1, 1, C).

%query(constraint_load(_)).
query(constraint(_)).
