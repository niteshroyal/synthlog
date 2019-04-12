%:- use_module('../synthlog/utils.py').

a(1).
a(2).
% c :- a(X), a(Y), \+ (X == Y).
%:- parse_clause('c :- a(A), a(B), A == B.').
%query(c).

d('a').
d('c :- a(A), a(B), A == B.').
e(X) :- d(X), X \= 'a', parse_clause(X).
query(e(_)).
query(c).
query(d(_)).