idb(IDB) :- excel:idb(X), load_inductive_db(X, IDB).
S:something :- S:X.
query(_:something). 