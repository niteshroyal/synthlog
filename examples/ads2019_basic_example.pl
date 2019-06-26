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

magic_cells:X :- load_csv('../data/magic_train.csv', X).
magic_tables:X :- detect_tables(magic_cells, X).

magic_models:X :-sklearn_predictor(magic_tables,
                    'linear_model.LogisticRegression',
                    [column('T1', 3), column('T1', 4)],
                    [column('T1', 6)], X).


missing_data_cells:X :- load_csv('../data/magic_test1.csv', X).
missing_data:X :- detect_tables(missing_data_cells, X).
missing_data:table_cell('T1', X,Y,V) :- magic_tables(train):table_cell('T1',X,Y,V), Y \== 7.

magic_predict:X :- magic_models:predictor(Y),
            magic_models:source(Y, column('T1', 3)),
            magic_models:source(Y, column('T1', 4)),
            predict(missing_data, Y, [column('T1', 3),
                        column('T1', 4)], X).

final_pred:table_cell('T1', X, 7, V) :- magic_predict:cell_pred(X,Y,V,_).
final_pred:table_cell('T1', X, Y, V) :- missing_data:table_cell('T1', X,Y,V), Y \== 7.

magic_constraints:(0.7::table_cell(T,X,7,0):-table_cell(T,X,5,V), V < 300).
%magic_constraints:(0.3::table_cell(T,X,7,1):-table_cell(T,X,5,V), V < 300).
%magic_constraints:(table_cell('T1',X,7,0):- between(1, 2, X), \+ table_cell('T1',X,7,1)).
magic_constraints:table_cell('T1', X, Y, V) :- missing_data:table_cell('T1', X,Y,V), Y \== 7.

combined_pred:table_cell(T,X,Y,V) :- magic_constraints:table_cell(T,X,Y,V);final_pred:table_cell(T,X,Y,V).

query(combined_pred:_).