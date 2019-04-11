:- use_module('../synthlog/spreadsheet.py').
:- use_module('../synthlog/transformers.py').


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).

magic_cells(train):cell(X,Y,V) :- magic_cells:cell(X,Y,V), X<4.
magic_tables(train):X :- detect_tables(magic_cells(train), X).

% Fit a transformer
magic_transforms:X :-ordinal_encoder(magic_tables(train),
                                [column('T1', 0)],
                                X).
% Use the transformer to transform the column
magic_cells(train, transformed_column):X :- magic_transforms:transformer(T), transform(magic_tables(train), T, [column('T1', 0)], X).
% We convert cell_transform into table_cells
magic_cells(train, transformed_column):table_cell(NewX,NewY,V) :- magic_cells(train, transformed_column):cell_transform(X,Y,V,_),magic_tables(train):table('T1', RowT, ColT, _, _), NewX is X, NewY is Y.
magic_cells(train, transformed_column):table_cell(X,Y,V) :- magic_tables(train):table_cell('T1',X,Y,V), \+ magic_cells(train, transformed_column):cell_transform(X,Y,V,_).


%
magic_cells(test):cell(X,Y,V) :- magic_cells:cell(X,Y,V), X>=4.
%query(magic_cells(train, transformed_column):_).
query(magic_cells(train, transformed_column):table_cell(_,_,_)).