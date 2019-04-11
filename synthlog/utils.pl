scope_difference(A, B, X) :- A:X, \+ B:X.
scope_intersection(A, B, X) :- A:X, B:X.
scope_union(A, B, X) :- A:X; B:X.
