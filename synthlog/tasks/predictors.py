from __future__ import print_function

import importlib
from abc import abstractmethod

import numpy as np
import pandas as pd

from problog.util import init_logger
from problog.logic import Term, Object, Constant, term2str, unquote

from synthlog.mercs.core.MERCS import MERCS
from synthlog.tasks.base_stored_object import StoredObject, cells_to_matrix

logger = init_logger()


class Predictor(StoredObject):
    def __init__(
        self,
        scope=None,
        source_columns=None,
        target_columns=None,
        database=None,
        engine=None,
    ):
        """

        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        """
        self.source_columns = source_columns
        self.target_columns = target_columns

        self.confidence = 0.5

        super().__init__(scope, database, engine)

    def match_query_res(self, r):
        """
        Function matching a result from the database to the current Predictor object
        :param r:
        :return:
        """
        return (
            term2str(r[0]) == term2str(self.scope)
            and r[1].functor == self.source_columns
            and r[2].functor == self.target_columns
        )

    def get_object_from_db(self):
        res = self.get_db_result()
        return res[3] if res else None

    def to_term(self):
        """
        Term representation of the current Predictor object
        :return:
        """
        return Term(
            "predictor_object",
            self.scope,
            Object(self.source_columns),
            Object(self.target_columns),
            self.problog_obj,
        )

    @abstractmethod
    def fit(self, table_cell_term_list):
        return NotImplemented

    @abstractmethod
    def predict(self, X):
        return NotImplemented

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
        """
        :param modelclass: A class (not an instance, but the class itself) of a predictor implementing the fit(x,y) method. Scikit-learn classifiers satisfiy this definition.
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of modelclass. This is a dictionary.
        """
        if parameters is None:
            parameters = {}
        self.parameters = parameters
        self.model = modelclass(**self.parameters)
        self.modelclass = modelclass

        super().__init__(
            scope=scope,
            source_columns=source_columns,
            target_columns=target_columns,
            database=database,
            engine=engine,
        )

    def fit(self, table_cell_term_list):
        """
        If a predictor object is matched on the database, does nothing.
        Else, learn the predictor model on scope. It uses source_columns to predict target_columns and stores the model in Problog database.
        """
        # If the object was not retrieved from db, we train the model
        if not self.object_from_db:
            relevant_table = [
                t
                for t in table_cell_term_list
                if t.args[0] == self.target_columns[0].args[0]
            ]

            matrix = cells_to_matrix(relevant_table)

            src_cols = [s.args[1].value for s in self.source_columns]
            tgt_cols = [s.args[1].value for s in self.target_columns]

            # If target is an object, we try to convert it to different types
            fit_target = matrix[:, tgt_cols]
            if matrix[:, tgt_cols].dtype == np.object:
                try:
                    fit_target = matrix[:, tgt_cols].astype(int)
                except:
                    try:
                        fit_target = matrix[:, tgt_cols].astype(float)
                    except:
                        try:
                            fit_target = matrix[:, tgt_cols].astype(str)
                        except:
                            fit_target = matrix[:, tgt_cols].astype(np.object)
            self.model.fit(matrix[:, src_cols], fit_target)

            # We add the new predictor in the database to be able to retrieve it in future calls
            self.database.add_fact(self.to_term())

    # def fit(self):
    #     """
    #     If a predictor object is matched on the database, does nothing.
    #     Else, learn the predictor model on scope. It uses source_columns to predict target_columns and stores the model in Problog database.
    #     """
    #     # If the object was not retrieved from db, we train the model
    #     if not self.object_from_db:
    #         table_cell_term_list = [
    #             t[1]
    #             for t in self.engine.query(
    #                 self.database, Term("':'", self.scope, None), subcall=True
    #             )
    #             if t[1].functor == "table_cell"
    #         ]
    #
    #         relevant_table = [
    #             t
    #             for t in table_cell_term_list
    #             if t.args[0] == self.target_columns[0].args[0]
    #         ]
    #
    #         matrix = cells_to_matrix(relevant_table)
    #
    #         src_cols = [s.args[1].value for s in self.source_columns]
    #         tgt_cols = [s.args[1].value for s in self.target_columns]
    #
    #         # If target is an object, we try to convert it to different types
    #         fit_target = matrix[:, tgt_cols]
    #         if matrix[:, tgt_cols].dtype == np.object:
    #             try:
    #                 fit_target = matrix[:, tgt_cols].astype(int)
    #             except:
    #                 try:
    #                     fit_target = matrix[:, tgt_cols].astype(float)
    #                 except:
    #                     try:
    #                         fit_target = matrix[:, tgt_cols].astype(str)
    #                     except:
    #                         fit_target = matrix[:, tgt_cols].astype(np.object)
    #         self.model.fit(matrix[:, src_cols], fit_target)
    #
    #         # We add the new predictor in the database to be able to retrieve it in future calls
    #         self.database.add_fact(self.to_term())

    def predict(self, X):
        return self.model.predict(X)

    def predict_proba(self, X):
        return self.model.predict_proba(X)

    def to_term(self):
        """
        Term representation of the current Predictor object
        :return:
        """
        return Term(
            "predictor_object",
            self.scope,
            Object(self.modelclass),
            Object(self.source_columns),
            Object(self.target_columns),
            self.problog_obj,
        )

    def match_query_res(self, r):
        """
        Function matching a result from the database to the current Predictor object
        :param r:
        :return:
        """
        return (
            term2str(r[0]) == term2str(self.scope)
            and r[1].functor == self.modelclass
            and r[2].functor == self.source_columns
            and r[3].functor == self.target_columns
        )

    def get_object_from_db(self):
        res = self.get_db_result()
        return res[4] if res else None


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
        """
        :param modelname: The name of the scikit-learn classifier to use. Name is the package name, without the sklearn part. For example, the name of a decision tree is "tree.DecisionTreeClassifier"
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of modelname. This is a dictionary.
        """
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
        """
        Creates a decision tree based on the scikit-learn implementation in tree.DecisionTreeClassifier
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of the decision tree. This is a dictionary. Parameters are the same as tree.DecisionTreeClassifier.
        """
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
        """
        Creates a random forest based on the scikit-learn implementation in ensemble.RandomForestClassifier
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of the decision tree. This is a dictionary. Parameters are the same as ensemble.RandomForestClassifier.
        """
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


class MERCSPredictor(Predictor):
    def __init__(
        self, scope, source_columns, database=None, engine=None, parameters=None
    ):
        """
        Creates a MERCS predictor.
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of MERCS.
        """
        super().__init__(
            scope=scope,
            source_columns=source_columns,
            target_columns=source_columns,
            database=database,
            engine=engine,
        )
        # So far, we do nothing with parameters
        self.parameters = parameters
        self.model = MERCS()

    def fit(self):
        """
        If a predictor object is matched on the database, does nothing.
        Else, learn the predictor model on scope. It uses source_columns to predict source_columns (MERCS can predict any column from its sources) and stores the model in Problog database.
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

            # Filter data
            matrix = cells_to_matrix(relevant_table)
            src_cols = [s.args[1].value for s in self.source_columns]
            matrix = matrix[:, src_cols]

            # Train a MERCS model

            data = pd.DataFrame(matrix)  # MERCS still needs this (elia: I'm so sorry)
            self.model.fit(data)

            # We add the new predictor in the database to be able to retrieve it in future calls
            self.database.add_fact(self.to_term())

    def __str__(self):
        return "MERCS({})".format(id(self))

    def __repr__(self):
        return "MERCS({})".format(id(self))

    def output_terms(self):
        return super().output_terms() + [Term("mercs", self.problog_obj)]

    def predict(self, X):
        return self.model.predict(X)

    def predict_proba(self, X):
        return self.model.predict_proba(X)


class MERCSWhiteBoxPredictor(MERCSPredictor):
    def __init__(
        self, scope, source_columns, database=None, engine=None, parameters=None
    ):
        """
        Creates a MERCS predictor, exposing its internal trees. It is a white box model
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of MERCS.
        """
        super().__init__(
            scope,
            source_columns,
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        """
        Exposes internal trees as decision tree predicates
        :return:
        """
        dt_terms = []
        for dt, dt_code in zip(self.model.m_list, self.model.m_codes):
            dt_source_columns = [
                x for i, x in enumerate(self.source_columns) if dt_code[i] == 0
            ]
            dt_target_columns = [
                x for i, x in enumerate(self.source_columns) if dt_code[i] == 1
            ]

            # We create decision tree objects (is it DTClassifier or Regressor, check that!)
            dt_object = DecisionTree(
                self.scope,
                dt_source_columns,
                dt_target_columns,
                self.database,
                self.engine,
            )
            # We set the model to the actual decision tree
            dt_object.model = dt
            self.database.add_fact(dt_object.to_term())

            dt_terms.extend(dt_object.output_terms())

        return super().output_terms() + dt_terms
