:- use_module('clause.py').

% Clause parsing
Scope:X :- Scope:Z, unify_clause(Z,X,Y), evaluate(Scope,Y).

% Evaluation rules
evaluate(Scope,Y) :- Scope:Y.
evaluate(Scope,(X < Y)) :- ground(X), ground(Y), X < Y.
evaluate(Scope,(X is Y)) :- ground(Y), X is Y.
evaluate(Scope,(X,Y)) :- evaluate(Scope,X), evaluate(Scope,Y).
evaluate(Scope,(X,Y)) :- evaluate(Scope,(Y,X)).
evaluate(Scope,(X;Y)) :- evaluate(Scope,X); evaluate(Scope,Y).
evaluate(Scope,(X;Y)) :- evaluate(Scope,(Y;X)).
