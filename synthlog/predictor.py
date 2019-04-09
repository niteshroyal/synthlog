import sys
import importlib

import numpy as np
import pandas as pd

import ast

from problog.logic import term2list, term2str, Term, Constant
from problog.errors import UserError

from sklearn.preprocessing import LabelEncoder
import sklearn.tree as tree

sys.path.append(".")
from mercs_repo.src.mercs.core import MERCS


#######################
#                     #
#       Classes       #
#                     #
#######################


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

    def fit(self, matrix, train_column):
        # Encoders are computed at train time. Unseen label at test time will lead to an error
        class_index = train_column.value - 1
        self.encoders = np.apply_along_axis(
            ClassifierWrapper.fit_encode, axis=0, arr=matrix
        )
        nmatrix = self.encode(matrix)
        train_indices = [
            i for i in range(nmatrix.shape[1]) if i != class_index
        ]
        self.clf.fit(X=nmatrix[:, train_indices], y=nmatrix[:, class_index])

    def predict(self, matrix, predict_column):
        pred_column = self.get_predict_column(predict_column)
        nmatrix = self.encode(matrix)

        pred = []
        if type(pred_column) is int:
            train_indices = [
                i for i in range(nmatrix.shape[1]) if i != pred_column
            ]
            pred = self.clf.predict(nmatrix[:, train_indices])
        elif type(pred_column) is list:
            raise ValueError("List type not supported for prediction indices")

        if self.encoders[pred_column]:
            pred = self.encoders[pred_column].inverse_transform(
                pred.astype(int)
            )
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


class MERCSClassifierWrapper(ClassifierWrapper):
    def __init__(self, params):
        """

        :param params: List of strings in the form key=value. Parameters are identical to the one in MERCS.
        Additional parameters: columns_prediction: list of columns to predict. Either column name or column index (0-based) in the fitted dataset.
        Default for columns_prediction is the last column
        """
        super().__init__()
        self.clf = MERCS()

        self.parse_parameters(params)

    def fit(self, matrix, train_column):
        self.encoders = np.apply_along_axis(
            ClassifierWrapper.fit_encode, axis=0, arr=matrix
        )
        nmatrix = self.encode(matrix)
        self.clf.fit(pd.DataFrame(nmatrix), **self.parameters)

    def predict(self, matrix, predict_column):
        # We build the code, so far no missing values are considered (the -1 code)
        pred_column = self.get_predict_column(predict_column)
        single_column = False
        if type(pred_column) is int:
            pred_column = [pred_column]
            single_column = True

        code = [0] * matrix.shape[1]
        for index in pred_column:
            code[index] = 1

        nmatrix = self.encode(matrix)

        pred = self.clf.predict(
            pd.DataFrame(nmatrix), **self.parameters, qry_code=code
        )

        processed_pred = []
        for index, col_index in enumerate(pred_column):
            if self.encoders[col_index]:
                processed_pred.append(
                    self.encoders[col_index].inverse_transform(
                        pred[:, index].astype(int)
                    )
                )
        if single_column:
            processed_pred = processed_pred[0]
        return np.array(processed_pred)
