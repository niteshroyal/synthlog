idb(IDB) :- excel(parameters):idb(X), load_prob_inductive_db(X, IDB). 
result:X :- idb(IDB), excel(parameters):scope(Scope), Scope:X. 
theory:Scope :- excel(parameters):scope(Scope). 
active:Scope :- theory:Scope. 

query(theory:_). 
query(active:_). 
query(result:_). 