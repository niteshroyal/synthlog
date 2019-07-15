% :- use_module('inductive_db.py').

scope_in_database('dummy__','dummy__',1).
P::X:Y :- scope_in_database(X,Y,P), X \= 'dummy__', P < 1.
X:Y :- scope_in_database(X,Y,1), X \= 'dummy__'.

query_save_term(1,1).
query_save_term(1,1,1).
:- query_save_term(Scope:Term, IDB), subquery(Scope:Term, P), save_term(Scope, Term, P, IDB).
:- query_save_term(Scope, X:Term, IDB), subquery(X:Term, P), save_term(Scope, Term, P, IDB).
