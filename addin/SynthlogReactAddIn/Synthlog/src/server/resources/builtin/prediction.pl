:- excel(parameters):idb(IDB), load_prob_inductive_db(IDB). 

train_cells:cell(X,Y,V) :- excel(parameters):scope(S), S:cell(X,Y,V), excel(parameters):train_row(X).
pred_cells:cell(X,Y,V) :-  excel(parameters):scope(S), S:cell(X,Y,V), excel(parameters):pred_row(X).

train_tables_scope:X :- detect_tables(train_cells, X).

train_column_list(list<Col>) :- excel(parameters):train_column(Col).
pred_column_list(list<Col>) :- excel(parameters):pred_column(Col).

create_col_rec([H|Cols], Res) :- create_col_rec(Cols, Res2), Res = [column('T1', H) |Res2].
create_col_rec([], []).

formatted_train_cols(Z) :- train_column_list(L), create_col_rec(L, Z).
formatted_pred_cols(Z) :- pred_column_list(L), create_col_rec(L, Z).

models_scope:X :- formatted_train_cols(TrainCols),
                    formatted_pred_cols(PredCols),
                    sklearn_predictor(train_tables_scope,
                    'linear_model.LogisticRegression',
                    TrainCols,
                    PredCols, X).

pred_tables:X :- detect_tables(pred_cells, X).

pred_scope:X :- models_scope:predictor(Y),
                formatted_train_cols(TrainCols),
                predict(pred_tables, Y, TrainCols, X).

final_pred:table_cell('T1', X, Y, V) :- pred_scope:cell_pred(X,Y,V,_).
%final_pred:table_cell('T1', X, Y, V) :- excel(parameters):pred_column(Col), pred_tables:table_cell('T1', X,Y,V), Y \== Col.

%query_save_term(my_scope_final_term, final_pred:X, IDB) :- final_pred:X, excel(parameters):idb(IDB). 


query(final_pred:_).