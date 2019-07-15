idb(IDB) :- excel(parameters):idb(X), load_prob_inductive_db(X, IDB). 
query_save_term(Scope, excel(data):X, IDB) :- excel(parameters):scope(Scope), idb(IDB). 