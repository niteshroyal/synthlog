idb(IDB) :- excel:idb(X), load_inductive_db(X, IDB). 
init:X :- excel:workbook_path(Path), load_spreadsheet(Path, X). 
:- init:X, idb(IDB), save_term(init, X, IDB). 
query(init:_).