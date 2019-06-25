# This file contains the base class for storing a Python object in a ClauseDB
# This object typically correspond to an object implementing the fit/predict ok sklearn

from __future__ import print_function

from abc import ABC, abstractmethod

from problog.util import init_logger
from problog.logic import Term, Object, Constant

import numpy as np


logger = init_logger()


class StoredObject(ABC):
    def __init__(self, scope=None, database=None, engine=None):
        """

        :param scope: A scope, containing table_cell predicates describing a table content.
        :param database: The database of Problog
        :param engine: The engine of Problog
        """
        self.scope = scope

        self.database = database
        self.engine = engine

        self.problog_obj = None
        query_obj = self.get_object_from_db()
        if query_obj:
            self.problog_obj = query_obj
            self.object_from_db = True
        else:
            self.problog_obj = Constant(self)
            self.object_from_db = False

    def get_db_result(self):
        if not self.database or not self.engine:
            logger.warning(
                "Could not try to retrieve predictor from db. database and engine should be filled."
            )
            return None

        # We try to retrieve the model trained with the same parameters
        res_predictor_object = [
            t
            for t in self.engine.query(
                self.database, self.get_query_term(), subcall=True
            )
        ]

        # If we succeed, we retrieve the previously trained object.
        # If not, we train a new one
        for r in res_predictor_object:
            if self.match_query_res(r):
                return r

    @abstractmethod
    def match_query_res(self, r):
        """
        Function matching a result from the database to the current Object object
        :param r:
        :return:
        """
        raise NotImplemented

    def get_query_term(self):
        """
        Return the Term that is used to query the database to retrieve the current Object object.
        It is based on the to_term() function and replaces each argument by None (the _ in problog).
        :return:
        """
        own_term = self.to_term()
        return Term(own_term.functor, *[None] * len(own_term.args))

    @abstractmethod
    def get_object_from_db(self):
        raise NotImplemented

    def to_term(self):
        """
        Term representation of the current Object object
        :return:
        """
        return Term("object", self.scope, self.problog_obj)

    def output_terms(self):
        predictor_term = Term("object", self.problog_obj)
        return [predictor_term]

    def __repr__(self):
        return "Object({})".format(id(self))

    def __str__(self):
        return "Object({})".format(id(self))


def cells_to_matrix(cell_term_list):
    min_y, max_y, min_x, max_x = [None, None, None, None]
    col_types = {}
    for cell_term in cell_term_list:
        y, x = cell_term.args[1].value, cell_term.args[2].value
        if not x in col_types:
            col_types[x] = []
        col_types[x].append(type(cell_term.args[3].value))

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

    # Ideally, we would like the right datatype for each cell instead of np.object.
    # This is not possible with numpy currently
    matrix = np.empty(shape=(row, column), dtype=np.object)

    for cell_term in cell_term_list:
        matrix[
            cell_term.args[1].value - 1, cell_term.args[2].value - 1
        ] = cell_term.args[3].value

    # matrix = np.array(
    #     matrix,
    #     dtype=[
    #         (str(i), np.object)
    #         if len(set(col_types[i])) != 1
    #         else (str(i), np.dtype(list(set(col_types[i]))[0]))
    #         for i in range(min_x, max_x + 1)
    #     ],
    # )

    return matrix
