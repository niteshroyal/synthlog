from __future__ import print_function

from problog.extern import (
    problog_export,
    problog_export_class,
    problog_export_raw,
    problog_export_nondet,
)

from itertools import product
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import LabelEncoder

import importlib
import sys
import ast

import numpy as np

from synthlog.keywords import init_cell_pred
from problog import engine
from problog.errors import UserError
from problog.logic import (
    Term,
    Object,
    term2list,
    Constant,
    is_list,
    term2str,
    Var,
    unquote,
)
from problog.engine_unify import unify_value, UnifyError


@problog_export("+term", "+term")
def comparison(column1, column2):
    print("Term: " + str(column1))
    print("Term: " + str(column2))
    print("Functor: " + str(column1.functor))
    print("Functor: " + str(column2.functor))
    print(column1.args[0])
    print(column2.args[0])
    return ()


@problog_export_nondet("+term", "+list", "+list", "-term")
def random_forest(scope, source_columns, target_columns, **kwargs):
    """
    Learn a random forest predictor on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the scikit-learn predictor object.
    random_forest(<predictor> is created, with <predictor> the scikit-learn predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    """

    def short_str(_self):
        return "RF({})".format(id(_self))

    RandomForestClassifier.__repr__ = short_str
    RandomForestClassifier.__str__ = short_str

    clf = RandomForestClassifier()
    problog_obj = Object(clf)
    sklearn_res, problog_obj_back = scikit_learn_predictor(
        scope, source_columns, target_columns, problog_obj, **kwargs
    )
    rf_term = [Term("random_forest", problog_obj_back)]
    return sklearn_res + rf_term


@problog_export_nondet("+term", "+list", "+list", "-term")
def decision_tree(scope, source_columns, target_columns, **kwargs):
    """
    Learn a decision tree predictor on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the scikit-learn predictor object.
    decision_tree(<predictor> is created, with <predictor> the scikit-learn predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    """

    def short_str(_self):
        return "DT({})".format(id(_self))

    DecisionTreeClassifier.__repr__ = short_str
    DecisionTreeClassifier.__str__ = short_str

    clf = DecisionTreeClassifier()
    problog_obj = Object(clf)
    sklearn_res, problog_obj_back = scikit_learn_predictor(
        scope, source_columns, target_columns, problog_obj, **kwargs
    )

    decision_tree_term = [Term("decision_tree", problog_obj_back)]
    return sklearn_res + decision_tree_term


def scikit_learn_predictor(
    scope, source_columns, target_columns, problog_obj, **kwargs
):
    """
    Learn scikit learn predictor clf on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the scikit-learn predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    """
    engine = kwargs["engine"]
    database = kwargs["database"]

    clf = problog_obj.functor

    # We try to retrieve the model trained with the same parameters
    res_predictor_object = [
        t
        for t in engine.query(
            database, Term("predictor_object", None, None, None, None), subcall=True
        )
    ]

    # If we succeed, we retrieve the previously trained object.
    # If not, we train a new one
    for r in res_predictor_object:
        if (
            term2str(scope) == r[0].functor
            and r[1].functor == source_columns
            and r[2].functor == target_columns
        ):
            problog_obj = r[3]
            source_columns = r[1].functor
            target_columns = r[2].functor

            predictor_term = Term("predictor", problog_obj)
            target_terms = [Term("target", problog_obj, t) for t in target_columns]
            source_terms = [Term("source", problog_obj, s) for s in source_columns]
            return [predictor_term] + source_terms + target_terms, problog_obj

    table_cell_term_list = [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == "table_cell"
    ]

    relevant_table = [
        t for t in table_cell_term_list if t.args[0] == target_columns[0].args[0]
    ]

    matrix = cells_to_matrix(relevant_table)

    src_cols = [s.args[1].value for s in source_columns]
    tgt_cols = [s.args[1].value for s in target_columns]

    clf.fit(matrix[:, src_cols], matrix[:, tgt_cols])

    # We add the new predictor in the database to be able to retrieve it in future calls
    database.add_fact(
        Term(
            "predictor_object",
            scope,
            Object(source_columns),
            Object(target_columns),
            problog_obj,
        )
    )

    predictor_term = Term("predictor", problog_obj)
    target_terms = [Term("target", problog_obj, t) for t in target_columns]
    source_terms = [Term("source", problog_obj, s) for s in source_columns]

    return [predictor_term] + source_terms + target_terms, problog_obj


@problog_export_nondet("+term", "+term", "+list", "-term")
def predict(scope, predictor, source_columns, **kwargs):
    """
    Predict values using a predictor that was fitted on data. It uses source_columns of scope to predict the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param predictor: A scikit-learn predictor, stored as a Problog Object (accessible through predictor(<predictor>) of the decision_tree funtion).
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
    :param kwargs:
    :return: Predictions from predictor using source_columns of scope, as well as predictions metadata.
    prediction(<scope>, <predictor>, <source_columns>) is created. <scope> is the scope parameter, as a Problog object, <predictor> is the predictor parameter, as a Problog object and <source_columns> are the source_columns parameter as a Problog object.
        This whole prediction/3 is used as a key for the prediction object. In the future, it might be better to use a unique identifier or something else!
    cell_pred(<row_id>, <col_id>, <value>, <prediction_term>) are created for each prediction. <row_id> and <col_id> are (1,1) indexed, NOT from the table_cell row and column ids.
        The <col_id> corresponds to the index of the target column of predictor. <value> is the predicted value. <prediction_term> is whole prediction(<scope>, <predictor>, <source_columns>) defined above.
    predictor(<prediction_term>, <predictor>) is created. <prediction_term> is whole prediction(<scope>, <predictor>, <source_columns>) defined above, <predictor> is the predictor parameter, as a Problog object
    source(<prediction_term>, <source_column>) are created for each source_column. <prediction_term> is whole prediction(<scope>, <predictor>, <source_columns>) defined above, <source_column> is column(<table_name>, <col_number>)
    """
    prediction_term_3 = Term(
        "prediction", Object(scope), predictor, Object(source_columns)
    )

    prediction_term_1 = Term("prediction", prediction_term_3)

    engine = kwargs["engine"]
    database = kwargs["database"]
    table_cell_term_list = [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == "table_cell"
    ]

    relevant_table = [
        t for t in table_cell_term_list if t.args[0] == source_columns[0].args[0]
    ]

    matrix = cells_to_matrix(relevant_table)

    src_cols = [s.args[1].value for s in source_columns]

    clf = predictor.functor
    y_pred = clf.predict(matrix[:, src_cols])

    if len(y_pred.shape) == 1:
        y_pred = np.atleast_2d(y_pred).T

    n_rows, n_cols = y_pred.shape

    cell_pred_terms = []
    for r, c in product(range(n_rows), range(n_cols)):
        cell_pred_terms.append(
            init_cell_pred(r + 1, c + 1, y_pred[r, c], prediction_term_3)
        )

    predictor_term = [Term("predictor", prediction_term_3, predictor)]
    source_terms = [Term("source", prediction_term_3, s) for s in source_columns]

    return (
        [prediction_term_1, prediction_term_3]
        + cell_pred_terms
        + predictor_term
        + source_terms
    )


def cells_to_matrix(cell_term_list):
    min_y, max_y, min_x, max_x = [None, None, None, None]
    for cell_term in cell_term_list:
        y, x = cell_term.args[1].value, cell_term.args[2].value
        if min_y is None or y < min_y:
            min_y = y
        if max_y is None or y > max_y:
            max_y = y
        if min_x is None or x < min_x:
            min_x = x
        if max_x is None or x > max_x:
            max_x = x
    row = max_y
    column = max_x
    matrix = np.empty(shape=(row, column), dtype=np.object)

    for cell_term in cell_term_list:
        matrix[
            cell_term.args[1].value - 1, cell_term.args[2].value - 1
        ] = cell_term.args[3].value

    return matrix
