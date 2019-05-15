:- use_module('../../synthlog/inductive_db.py').

idb(IDB) :- load_inductive_db('db_file.db', IDB).
:- idb(_).

a:c.
:- a:A, idb(IDB), save_term(a, A, IDB).

:(a,c).

query(a:c).
