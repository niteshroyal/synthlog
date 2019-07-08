:- use_module('../../synthlog/inductive_db.pl').

idb(IDB) :- load_prob_inductive_db('db_file.db', IDB).
0.9::(:(a,c)).
query_save_term(a:A, IDB):- a:A, idb(IDB).
