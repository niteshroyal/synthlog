:- use_module(library(collect)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(string)).
:- use_module(library(scope)).
:- use_module('../synthlog/spreadsheet.pl').
:- use_module('../synthlog/transformers.pl').
:- use_module('../synthlog/utils.py').
:- use_module('../synthlog/predict.pl').
:- use_module('../synthlog/clause.pl').
list(X,X).

%wrong :- S:wrong.

%evidence(\+wrong).
train_data:cell(X,Y,V) :- load_csv('../data/titanic/train.csv', cell(X,Y,V)), Y < 8.
train_tables:X :- detect_tables(train_data, X).

titanic_models:X :- sklearn_predictor(train_tables, 'ensemble.RandomForestClassifier',
                        [column('T1', 2)],
                        [column('T1', 1)],
                        X).

test_data:cell(X,Y,V) :- load_csv('../data/titanic/test.csv', cell(X,Y,V)), Y<7.
test_tables:X :- detect_tables(test_data, X).

titanic_preds:X :- titanic_models:predictor(Y), predict(test_tables,Y,
                                                    [column('T1', 1)],
                                                    X).

titanic_constraints:(0.8::table_cell('T1', X, 7, 0) :- table_cell('T1', X, 4, 'male')).

titanic_final_pred:X :- titanic_constraints:X.
titanic_final_pred:table_cell('T1',X, 7, V) :- titanic_preds:cell_pred(X,1,V,_).
titanic_final_pred:table_cell('T1',X, Y, V) :- test_tables:table_cell('T1', X, Y, V).

%query(train_tables:table(_,_,_,_,_)).
query(titanic_final_pred:_).

