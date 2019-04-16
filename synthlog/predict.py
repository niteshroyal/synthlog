from __future__ import print_function

from problog.extern import problog_export_nondet

from itertools import product

from problog.util import init_logger
from synthlog.keywords import init_cell_pred

from synthlog.predictors.predictors_classes import *

logger = init_logger()


@problog_export_nondet("+term", "+term", "+list", "+list", "-term")
def sklearn_predictor(scope, predictor_name, source_columns, target_columns, **kwargs):
    clf = SKLearnPredictor(
        predictor_name,
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clf.fit()
    return clf.output_terms()


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
    clf = DecisionTree(
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clf.fit()
    return clf.output_terms()


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
    clf = RandomForest(
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clf.fit()
    return clf.output_terms()


@problog_export_nondet("+term", "+list", "-term")
def mercs(scope, source_columns, **kwargs):
    """
    Learn a random forest predictor on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the scikit-learn predictor object.
    mercs(<predictor> is created, with <predictor> the scikit-learn predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    """
    clf = MERCSPredictor(
        scope, source_columns, database=kwargs["database"], engine=kwargs["engine"]
    )
    clf.fit()
    return clf.output_terms()


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

    clf = predictor.functor.model
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
