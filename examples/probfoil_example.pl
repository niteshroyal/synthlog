:- use_module('../synthlog/rules.py').

magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
% query(magic_cells:_).

magic_tables:X :- detect_tables(magic_cells, X).
% query(magic_tables:table(_,_,_,_,_)).
%query(magic_tables:_).


% table(<table_name>, <start_row>, <start_col>, <height>, <width>)
% table_header(<table_name>, <table_column>, <table_header>), <header_type>)
% table_cell(<table_name>, <cell_row>, <cell_column>, <cell_value>)
% table_cell_type(<table_name>, <cell_row>, <cell_column>, <cell_type>)


% Stitch cell_types to table_atoms(<table_name>, <cell_header>, <cell_row>, <cell_value>, <cell_type>)
magic_tables:table_atoms(Table_name, Cell_header, Cell_row, Cell_type, Cell_value) :-
    magic_tables:table(Table_name, Start_row, Start_col, Height, Width),
    magic_tables:table_header(Table_name, Cell_column, Cell_header),
    magic_tables:table_cell(Table_name, Cell_row, Cell_column, Cell_value),
    magic_tables:table_cell_type(Table_name, Cell_row, Cell_column, Cell_type).
%query(magic_tables:table_atoms(_,_,_,_,_)).


% Convert the matrix of cells to atoms
magic_atoms:X :-
    magic_tables:table_atoms(Table_name, Cell_header, Cell_row, Cell_type, Cell_value),
    matrix_to_atoms(Table_name, Cell_header, Cell_row, Cell_type, Cell_value, X).
%query(magic_atoms:_).

% Learn probfoil rules for all atoms in the scope 'magic_atoms' with 'profit' as our target predicate
probfoil_rules:X :- probfoil(magic_atoms, 'profit', X).
probfoil_rules:X :- probfoil_loop(magic_atoms, 'country', X).
query(probfoil_rules:predictor(_)).

