from __future__ import print_function

from problog.extern import (
    problog_export,
    problog_export_class,
    problog_export_raw,
    problog_export_nondet,
)
import sklearn
from sklearn.tree import DecisionTreeClassifier
from sklearn.preprocessing import LabelEncoder

import importlib
import sys
import ast
import numpy as np

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


@problog_export_nondet("+term", "+list", "+list", "-term")
def decision_tree(scope, source_columns, target_columns, **kwargs):
    engine = kwargs["engine"]
    database = kwargs["database"]
    table_cell_term_list = [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == "table_cell"
    ]

    # raise RuntimeError(target_columns, type(target_columns), type(target_columns[0]))
    # raise RuntimeWarning([t for t in table_cell_term_list if t.args[0] == target_columns[0].args[0]])
    # raise RuntimeError(table_cell_term_list, table_cell_term_list[0].value, type(table_cell_term_list[0].value))

    relevant_table = [
        t for t in table_cell_term_list if t.args[0] == target_columns[0].args[0]
    ]

    matrix = cells_to_matrix(relevant_table)

    clf = DecisionTreeClassifier()

    src_cols = [s.args[1].value for s in source_columns]
    tgt_cols = [s.args[1].value for s in target_columns]

    print(type(matrix), matrix.shape)
    clf.fit(matrix[:, src_cols], matrix[:, tgt_cols])

    print(clf, clf.get_params())
    quit()

    # raise RuntimeError(matrix)

    return []


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


class ClassifierWrapper:
    def __init__(self):
        self.clf = None
        self.encoders = None
        self.parameters = {}

    def parse_parameters(self, params):
        for p in term2list(params):
            try:
                value = ast.literal_eval(term2str(p).split("=")[1])
            # If we can not convert the argument value to a standard one, we keep it as a string
            except:
                value = str(term2str(p).split("=")[1])
            finally:
                self.parameters[term2str(p).split("=")[0]] = value

    def encode(self, matrix):
        nmatrix = matrix.copy()
        for i in range(nmatrix.shape[1]):
            if self.encoders[i]:
                nmatrix[:, i] = self.encoders[i].transform(nmatrix[:, i])
        return nmatrix.astype(float)

    def fit(self, matrix, targ_ids):
        # Encoders are computed at train time. Unseen label at test time will lead to an error

        nmatrix = self.encode(matrix)
        attr_ids = set(range(nmatrix.shape[1]))
        desc_ids = list(attr_ids - set(targ_ids))

        self.encoders = np.apply_along_axis(
            ClassifierWrapper.fit_encode, axis=0, arr=matrix
        )
        self.clf.fit(X=nmatrix[:, desc_ids], y=nmatrix[:, targ_ids])

    def predict(self, matrix, predict_column):
        pred_column = self.get_predict_column(predict_column)
        nmatrix = self.encode(matrix)

        pred = []
        if type(pred_column) is int:
            train_indices = [i for i in range(nmatrix.shape[1]) if i != pred_column]
            pred = self.clf.predict(nmatrix[:, train_indices])
        elif type(pred_column) is list:
            raise ValueError("List type not supported for prediction indices")

        if self.encoders[pred_column]:
            pred = self.encoders[pred_column].inverse_transform(pred.astype(int))
        return pred

    def get_predict_column(self, predict_column):
        pred_column = None
        if type(predict_column) is Term:
            pred_column = [i - 1 for i in term2list(predict_column)]
        elif type(predict_column) is Constant:
            pred_column = predict_column.value - 1
        return pred_column

    #######################
    #                     #
    #        Class        #
    #       Methods       #
    #                     #
    #######################

    @staticmethod
    def fit_encode(array):
        try:
            array.astype(float, casting="safe")
        except:
            try:
                label = LabelEncoder()
                label.fit(array)
                return label
            except:
                raise UserError("Cannot fit matrices containing variables")

    @staticmethod
    def drop_quotes(s):
        s = s.functor
        if s.startswith("'") and s.endswith("'"):
            return s[1:-1]
        else:
            return s


class ScikitLearnClassifierWrapper(ClassifierWrapper):
    def __init__(self, model_name, params=None):
        """

        :param model_name: Corresponds to the name of the classifier in sklearn. Example for random forest: ensemble.RandomForestClassifier
        :param params: Parameters of the classifier (same as the one in sklearn). List of strings in the form key=value.
        """
        super().__init__()
        self.model_name = "sklearn.%s" % self.drop_quotes(model_name)
        module_name, classname = self.model_name.rsplit(".", 1)
        model_class = getattr(importlib.import_module(module_name), classname)

        self.parse_parameters(params)

        self.clf = model_class(**self.parameters)
