from __future__ import print_function

from problog.extern import (
    problog_export,
    problog_export_class,
    problog_export_raw,
    problog_export_nondet,
)

import sys


from problog.logic import Term, Object, term2list, Constant, is_list, term2str, Var
from problog.engine_unify import unify_value, UnifyError

sys.path.append(".")
import predictor


#######################
#                     #
#       ProbLog       #
#       Exports       #
#                     #
#######################


@problog_export("+term", "-term")
def get_mercs_classifier(params):
    return Object(predictors.MERCSClassifierWrapper(params))


@problog_export("+term", "+term", "-term")
def sklearn_classifier(classifier_name, params):
    return Object(predictors.ScikitLearnClassifierWrapper(classifier_name, params))


@problog_export("+term", "+term", "-term")
def decision_tree(matrix, tgt_columns):

    m = matrix.functor.matrix
    classifier = predictors.ScikitLearnClassifierWrapper("DecisionTreeClassifier")

    clf = classifier.functor
    clf.fit(m, tgt_columns)

    return Object(clf)


@problog_export("+term", "+term", "+term", "-term")
def fit(matrix, classifier, src_columns):
    """
    Fit a classifier on the given matrix to predict train_column
    :param matrix: Matrix containing both the train features and the column to predict
    :param classifier: Classifier that will be used to fit the data
    :param src_columns: Index (1-based index) of the column matrix that will be predicted. It is automatically excluded from train features.
    :return: The trained classifier
    """
    m = matrix.functor.matrix
    clf = classifier.functor
    clf.fit(m, src_columns)
    return Object(clf)


@problog_export("+term", "+term", "+term", "-list")
def predict(matrix, classifier, tgt_columns):
    """
    Predicts
    :param matrix: The matrix containing data for prediction. Column(s) in predict_column is (are) removed from the matrix before prediction.
    :param classifier: A classifier trained on the right columns.
    :param tgt_columns: Index (or list of indices for some classifiers) of columns to predict in matrix.
    :return: A list of predictions (or list of list of predictions if predict_column is a list).
    """
    m = matrix.functor.matrix
    clf = classifier.functor
    pred = clf.predict(m, tgt_columns)
    return pred.tolist()
