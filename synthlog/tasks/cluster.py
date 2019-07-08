from __future__ import print_function

import importlib
from abc import abstractmethod

import numpy as np
import pandas as pd

from problog.util import init_logger
from problog.logic import Term, Object, Constant, term2str, unquote

from synthlog.tasks.base_stored_object import StoredObject, cells_to_matrix

logger = init_logger()


class Cluster(StoredObject):
    def __init__(self, scope=None, source_columns=None, database=None, engine=None):
        """

        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the cluster.
        :param database: The database of Problog
        :param engine: The engine of Problog
        """
        self.source_columns = source_columns

        self.confidence = 0.5

        super().__init__(scope, database, engine)

    def match_query_res(self, r):
        """
        Function matching a result from the database to the current Cluster object
        :param r:
        :return:
        """
        return (
            term2str(r[0]) == term2str(self.scope)
            and r[1].functor == self.source_columns
        )

    def get_query_term(self):
        """
        Return the Term that is used to query the database to retrieve the current Cluster object.
        It is based on the to_term() function and replaces each argument by None (the _ in problog).
        :return:
        """
        own_term = self.to_term()
        return Term(own_term.functor, *[None] * len(own_term.args))

    def get_object_from_db(self):
        res = self.get_db_result()
        return res[2] if res else None

    def to_term(self):
        """
        Term representation of the current Cluster  object
        :return:
        """
        return Term(
            "cluster_object", self.scope, Object(self.source_columns), self.problog_obj
        )

    @abstractmethod
    def fit(self, table_cell_term_list):
        return NotImplemented

    @abstractmethod
    def predict(self, X):
        return NotImplemented

    def output_terms(self):
        predictor_term = Term("clustering", self.problog_obj)
        source_terms = [
            Term("source", self.problog_obj, s) for s in self.source_columns
        ]
        return [predictor_term] + source_terms

    def __repr__(self):
        return "Cluster({})".format(id(self))

    def __str__(self):
        return "Cluster({})".format(id(self))


class FitCluster(Cluster):
    def __init__(
        self,
        modelclass,
        scope,
        source_columns,
        database=None,
        engine=None,
        parameters=None,
    ):
        """
        :param modelclass: A class (not an instance, but the class itself) of a cluster implementing the fit(x) method. Scikit-learn classifiers satisfiy this definition.
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the cluster.
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
            scope=scope, source_columns=source_columns, database=database, engine=engine
        )

    def fit(self, table_cell_term_list):
        """
        If a cluster object is matched on the database, does nothing.
        Else, learn the cluster model on scope. It uses source_columns to predict target_columns and stores the model in Problog database.
        """
        # If the object was not retrieved from db, we train the model
        if not self.object_from_db:
            matrix = cells_to_matrix(table_cell_term_list)

            src_cols = [s.args[1].value for s in self.source_columns]

            self.model.fit(matrix[:, src_cols])

            # We add the new predictor in the database to be able to retrieve it in future calls
            self.database.add_fact(self.to_term())

    def predict(self, X):
        return self.model.predict(X)

    def predict_proba(self, X):
        return self.model.predict_proba(X)

    def to_term(self):
        """
        Term representation of the current Cluster object
        :return:
        """
        return Term(
            "cluster_object",
            self.scope,
            Object(self.modelclass),
            Object(self.source_columns),
            self.problog_obj,
        )

    def match_query_res(self, r):
        """
        Function matching a result from the database to the current Cluster object
        :param r:
        :return:
        """
        return (
            term2str(r[0]) == term2str(self.scope)
            and r[1].functor == self.modelclass
            and r[2].functor == self.source_columns
        )

    def get_object_from_db(self):
        res = self.get_db_result()
        return res[3] if res else None


class SKLearnCluster(FitCluster):
    def __init__(
        self,
        modelname,
        scope,
        source_columns,
        database=None,
        engine=None,
        parameters=None,
    ):
        """
        :param modelname: The name of the scikit-learn cluster method to use. Name is the package name, without the sklearn part. For example, the name of KMeans is "cluster.KMeans"
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the clustering.
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
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        super_terms = super().output_terms()
        return super_terms + [Term("sklearn_clustering", self.problog_obj)]


class KMeans(SKLearnCluster):
    def __init__(
        self, scope, source_columns, database=None, engine=None, parameters=None
    ):
        """
        Creates a KMeans based on the scikit-learn implementation in cluster.KMeans
        :param scope: A scope, containing table_cell predicates describing a table content.
        :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the clustering.
         :param database: The database of Problog
        :param engine: The engine of Problog
        :param parameters: Parameters to pass to the constructor of the KMEans. This is a dictionary. Parameters are the same as cluster.KMeans.
        """
        super().__init__(
            "cluster.KMeans",
            scope,
            source_columns,
            database=database,
            engine=engine,
            parameters=parameters,
        )

    def output_terms(self):
        return super().output_terms() + [Term("kmeans", self.problog_obj)]

    def __str__(self):
        return "kmeans({})".format(id(self))

    def __repr__(self):
        return "kmeans({})".format(id(self))
