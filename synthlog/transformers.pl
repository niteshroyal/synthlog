:- use_module('transformers.py').

inverse_transform(Scope, T, ColL, Res) :- findall((P, X), (subquery(Scope:X, P), X=..[Name|Rest], Name == table_cell, proper_length(Rest,4)), L), inverse_trans(Scope, L, T, ColL, Res).
transform(Scope, T, ColL, Res) :- findall((P, X), (subquery(Scope:X, P), X=..[Name|Rest], Name == table_cell, proper_length(Rest,4)), L), trans(Scope, L, T, ColL, Res).