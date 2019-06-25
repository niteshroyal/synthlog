% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

%% Train set
magic_tables(train):X :- magic_tables:X, X=..[Name|Rest], Name \== table_cell, Name \== table_cell_type.
% Maybe provide a predefined way to slice tables in a clean way?
magic_tables(train):table_cell_type(T, X,Y,Type) :- magic_tables:table_cell_type(T, X,Y,Type), X < 7.
magic_tables(train):table_cell(T, X,Y,Type) :- magic_tables:table_cell(T, X,Y,Type), X < 7.

query(magic_tables(train):_).