:- use_module('../synthlog/clause.pl').

a:(a:-b).
a:(c:-a,b).
a:(0.8::e:-c;d).
a:(f:-a,c,e).
a:(g:-support(X), X < Y, Y is 2).
a:b.
a:support(1).

query(a:_).