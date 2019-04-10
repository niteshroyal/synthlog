magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X).
%query(magic_tables:_).


% table(<table_name>, <start_row>, <start_col>, <height>, <width>)
% table_header(<table_name>, <table_column>, <table_header>), <header_type>)
% table_cell(<table_name>, <cell_row>, <cell_column>, <cell_value>)
% table_cell_type(<table_name>, <cell_row>, <cell_column>, <cell_type>)


% Stitch cell_types to table_atoms(<table_name>, <cell_header>, <cell_row>, <cell_value>, <cell_type>)
magic_tables:table_atoms(A, G, H, J, I) :-
    magic_tables:table(A, B, C, D, E),
    magic_tables:table_header(A, F, G),
    magic_tables:table_cell(A, H, F, I),
    magic_tables:table_cell_type(A, H, F, J).
%query(magic_tables:table_atoms(_,_,_,_,_)).


magic_atoms:X :-
    magic_tables:table_atoms(A, B, C, D, E),
    matrix_to_atoms(A, B, C, D, E, X).

query(magic_atoms:_).