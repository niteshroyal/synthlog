:- use_module('../../synthlog/inductive_db.py').

idb(IDB) :- load_inductive_db('db_file.db', IDB).
:- idb(_).

test:test.

query(_:_).