from __future__ import print_function

from problog.extern import problog_export_nondet

from itertools import product

import importlib

import numpy as np

from problog.util import init_logger
from synthlog.keywords import init_cell_pred

from problog.logic import Term, Object, term2str, unquote

logger = init_logger()


class Predictor:
    def __init__(
        self,
        scope=None,
        source_columns=None,
        target_columns=None,
        database=None,
        engine=None,
    ):
        self.scope = scope
        self.source_columns = source_columns
        self.target_columns = target_columns

        self.database = database
        self.engine = engine

        self.query_term = Term("predictor_object", None, None, None, None)

        query_obj = self.get_object_from_db()
        if query_obj:
            self.problog_obj = query_obj
            self.object_from_db = True
        else:
            self.problog_obj = Object(self)
            self.object_from_db = False

    def get_db_result(self):
        if not self.database or not self.engine:
            logger.warning(
                "Could not try to retrieve predictor from db. database and engine should be filled."
            )
            return None

        # We try to retrieve the model trained with the same parameters
        res_predictor_object = [
            t for t in self.engine.query(self.database, self.query_term, subcall=True)
        ]

        # If we succeed, we retrieve the previously trained object.
        # If not, we train a new one
        for r in res_predictor_object:
            if self.match_query_res(r):
                return r

    def match_query_res(self, r):
        return (
            r[0].functor == term2str(self.scope)
            and r[1].functor == self.source_columns
            and r[2].functor == self.target_columns
        )

    def get_object_from_db(self):
        res = self.get_db_result()
        return res[3] if res else None

    def to_term(self):
        return Term(
            "predictor_object",
            self.scope,
            Object(self.source_columns),
            Object(self.target_columns),
            self.problog_obj,
        )

    def fit(self):
        raise NotImplementedError

    def output_terms(self):
        predictor_term = Term("predictor", self.problog_obj)
        target_terms = [
            Term("target", self.problog_obj, t) for t in self.target_columns
        ]
        source_terms = [
            Term("source", self.problog_obj, s) for s in self.source_columns
        ]
        return [predictor_term] + source_terms + target_terms

    def __repr__(self):
        return "Predictor({})".format(id(self))

    def __str__(self):
        return "Predictor({})".format(id(self))


class SKLearnPredictor(Predictor):
    def __init__(
        self,
        modelname,
        scope,
        source_columns,
        target_columns,
        database=None,
        engine=None,
        parameters=None,
    ):
        super().__init__(
            scope=scope,
            source_columns=source_columns,
            target_columns=target_columns,
            database=database,
            engine=engine,
        )
        if parameters is None:
            parameters = {}
        self.modelname = "sklearn.%s" % unquote(modelname)
        self.parameters = parameters

        modulename, classname = self.modelname.rsplit(".", 1)
        modelclass = getattr(importlib.import_module(modulename), classname)
        self.model = modelclass(**self.parameters)

    def fit(self):
        """
        Learn scikit learn predictor clf on scope. It uses source_columns to predict target_columns
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param kwargs:
        :return: A tuple: list of Terms, classifier_object
        List of Terms is:
            predictor(<predictor>) is created, with <predictor> the scikit-learn predictor object.
            target(<predictor>, <column>) are created for each target column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
            source(<predictor>, <column>) are created for each source column. <predictor> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
        classifier_object is the classifier, as a Problog object
        """
        # If the object was not retrieved from db, we train the model
        if not self.object_from_db:
            table_cell_term_list = [
                t[1]
                for t in self.engine.query(
                    self.database, Term("':'", self.scope, None), subcall=True
                )
                if t[1].functor == "table_cell"
            ]

            relevant_table = [
                t
                for t in table_cell_term_list
                if t.args[0] == self.target_columns[0].args[0]
            ]

            matrix = cells_to_matrix(relevant_table)

            src_cols = [s.args[1].value for s in self.source_columns]
            tgt_cols = [s.args[1].value for s in self.target_columns]

            self.model.fit(matrix[:, src_cols], matrix[:, tgt_cols])

            # We add the new predictor in the database to be able to retrieve it in future calls
            self.database.add_fact(self.to_term())

    def output_terms(self):
        super_terms = super().output_terms()
        return super_terms + [Term("sklearn_predictor", self.problog_obj)]


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


class DecisionTree(SKLearnPredictor):
    def __init__(
        self,
        scope,
        source_columns,
        target_columns,
        database=None,
        engine=None,
        parameters=None,
    ):
        super().__init__(
            "tree.DecisionTreeClassifier",
            scope,
            source_columns,
            target_columns,
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        return super().output_terms() + [Term("decision_tree", self.problog_obj)]

    def __str__(self):
        return "DT({})".format(id(self))

    def __repr__(self):
        return "DT({})".format(id(self))


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


class RandomForest(SKLearnPredictor):
    def __init__(
        self,
        scope,
        source_columns,
        target_columns,
        database=None,
        engine=None,
        parameters=None,
    ):
        super().__init__(
            "ensemble.RandomForestClassifier",
            scope,
            source_columns,
            target_columns,
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        return super().output_terms() + [Term("random_forest", self.problog_obj)]

    def __str__(self):
        return "RF({})".format(id(self))

    def __repr__(self):
        return "RF({})".format(id(self))


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
