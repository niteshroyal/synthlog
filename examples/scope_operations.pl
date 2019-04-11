:- consult('../synthlog/utils.pl').

a:a.
a:b.

b:b.
b:c.

c:X :- scope_difference(a,b,X).
d:X :- scope_union(a,b,X).
e:X :- scope_intersection(a,b,X).

query(c:_).
query(d:_).
query(e:_).