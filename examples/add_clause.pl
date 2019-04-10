:- use_module('../synthlog/utils.py').

a(1).
a(2).
% c :- a(X), a(Y), \+ (X == Y).
:- parse_clause('c :- a(A), a(B), A == B.').
query(c).