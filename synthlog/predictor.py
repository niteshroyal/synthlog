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

        pred = self.clf.predict(pd.DataFrame(nmatrix), **self.parameters, qry_code=code)

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
