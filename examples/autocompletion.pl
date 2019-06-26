%:- use_module(library(collect)).
%:- use_module(library(aggregate)).
%:- use_module(library(lists)).
%:- use_module(library(string)).
%:- use_module(library(scope)).
%:- use_module('../synthlog/spreadsheet.pl').
%:- use_module('../synthlog/transformers.pl').
%:- use_module('../synthlog/utils.py').
%:- use_module('../synthlog/predict.pl').
%:- use_module('../synthlog/clause.pl').
%list(X,X).

%wrong :- S:wrong.

%evidence(\+wrong).


% Get the tables (from CSV)
magic_cells:X :- load_csv('../data/magic_ice_cream.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

%% Train set
magic_tables(train):X :- magic_tables:X, X=..[Name|Rest], Name \== table_cell, Name \== table_cell_type.
% Maybe provide a predefined way to slice tables in a clean way?
magic_tables(train):table_cell_type(T, X,Y,Type) :- magic_tables:table_cell_type(T, X,Y,Type), X < 7.
magic_tables(train):table_cell(T, X,Y,Type) :- magic_tables:table_cell(T, X,Y,Type), X < 7.

%% Test set
magic_tables(test):X :- magic_tables:X, X=..[Name|Rest], Name \== table_cell, Name \== table_cell_type.
% Maybe provide a predefined way to slice tables in a clean way?
magic_tables(test):table_cell_type(T, NewX,Y,Type) :- magic_tables:table_cell_type(T, X,Y,Type), X >= 7, Y \== 7, NewX is X-6.
magic_tables(test):table_cell(T, NewX,Y,Type) :- magic_tables:table_cell(T, X,Y,Type), X >= 7, Y \== 7, NewX is X-6.

% We fit a transformer for the profit column
magic_transforms:X :-ordinal_encoder(magic_tables(train),
                                [column('T1', 6)],
                                X).

% Use the transformer to transform the column
magic_cells(train, transformed_column):X :- magic_transforms:transformer(T), transform(magic_tables(train), T, [column('T1', 6)], X).
% We convert cell_transform into table_cells
magic_cells(train, transformed_column):table_cell('T1', X,7,V) :- magic_cells(train, transformed_column):cell_transform(X,Y,V,_).
magic_cells(train, transformed_column):table_cell('T1', X,Y,V) :- magic_tables(train):table_cell('T1',X,Y,V), Y \== 7.

% Train a classifier
magic_models:X :-random_forest(magic_cells(train, transformed_column),
                               [column('T1', 2), column('T1', 3)],
                               [column('T1', 6)],
                               X).
% Train a second one
magic_models:X :-sklearn_predictor(magic_cells(train, transformed_column),
                                'linear_model.LogisticRegression',
                               [column('T1', 3), column('T1', 4)],
                               [column('T1', 6)],
                               X).

% Do a probabilistic prediction, by using predictors trained on col 2 and 3
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables(test), Y, [column('T1', 2), column('T1', 3)], X), magic_models:source(Y, column('T1', 2)), magic_models:source(Y, column('T1', 3)).
% Do a probabilistic prediction, by using predictors trained on col 3 and 4
magic_predict:X :- magic_models:predictor(Y), predict(magic_tables(test), Y, [column('T1', 3), column('T1', 4)], X), magic_models:source(Y, column('T1', 3)), magic_models:source(Y, column('T1', 4)).

% This emulates an annotated disjunction between both predictors
weight(X) :- magic_models:random_forest(C1), magic_predict:confidence(C1, Conf1), magic_predict:source(C2, column('T1', 4)), magic_predict:confidence(C2, Conf2), select_weighted(1, [Conf1, Conf2], [C1, C2], X, _).

% We combine the prediction using the confidences
magic_predict:final_pred(X,Y,V):- weight(C1), magic_models:random_forest(C1), magic_predict:cell_pred(X,Y,V,C1).
magic_predict:final_pred(X,Y,V):- weight(C2), magic_predict:source(C2, column('T1', 4)), magic_predict:cell_pred(X,Y,V,C2).

magic_final_pred:X :- magic_tables(test):X.
magic_final_pred:table_cell('T1', X, 7, V) :- magic_predict:final_pred(X,_,V).

% We add constraints (these could be learned as well!)
magic_constraints:(0.7::wrong:-table_cell(T,X,5,V), V > 500, table_cell(T,X,7,0)).
magic_constraints:(0.8::wrong:-table_cell(T,X,5,V), V < 300, table_cell(T,X,7,1)).

% We apply the constraints to the scope
magic_final_pred:X :- magic_constraints:X.
%contains_clauses(magic_final_pred).

% And finally, we query for the result
query(magic_final_pred:table_cell(_,_,_,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%OLD CODE
%magic_final_pred(transformed):table_cell('T1', X, Y, V) :- magic_predict:table_cell('T1',X,Y,V).
%magic_final_pred(transformed):X :- magic_transforms:transformer(T), inverse_transform(magic_final_pred, T, [column('T1', 6)], X).
%magic_final_pred(transformed):table_cell('T1', X, Y,V) :- magic_final_pred(transformed):cell_transform(X,Y,V,_).
%magic_final_pred(transformed):table_cell('T1', X, Y,V) :- magic_final_pred(transformed):table_cell('T1',X,Y,V), \+ magic_final_pred(transformed):cell_transform(X,Y,V2,_).
%query(magic_final_pred:_).

%query(magic_constraints:final_pred(_,_,_)).
%query(magic_predict:_).
