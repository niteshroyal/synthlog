%:- use_module('../synthlog/clause.pl').

a:(a:-b).
a:(c:-a,b).
a:(0.8::e:-c).
a:(f:-a,e,c).
a:b.

a:support(2).
0.2::a:support(1).
a:test(1).
a:(t:-not(support(1))).


contains_clauses(a).

query(a:_).
