from __future__ import print_function

import importlib

import numpy as np

from problog.util import init_logger

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


class FitPredictor(Predictor):
    def __init__(
        self,
        modelclass,
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
        self.parameters = parameters
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


class SKLearnPredictor(FitPredictor):
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
        self.modelname = "sklearn.%s" % unquote(modelname)
        modulename, classname = self.modelname.rsplit(".", 1)
        modelclass = getattr(importlib.import_module(modulename), classname)

        super().__init__(
            modelclass,
            scope=scope,
            source_columns=source_columns,
            target_columns=target_columns,
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        super_terms = super().output_terms()
        return super_terms + [Term("sklearn_predictor", self.problog_obj)]


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
