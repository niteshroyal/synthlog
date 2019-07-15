:- excel(parameters):idb(IDB), load_prob_inductive_db(IDB). 
query_save_term(Scope, excel(data):X, IDB) :- 
    excel(parameters):scope(Scope), 
    excel(parameters):idb(IDB). 