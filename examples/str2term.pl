:- use_module('../synthlog/utils.py').

transform(A, B, X) :- str2term(A,Y), X =.. [Y,B].

test:X :- transform('a', 1, X).

query(test:_).