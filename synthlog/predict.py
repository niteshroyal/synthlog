from __future__ import print_function

from problog.extern import problog_export_nondet

from itertools import product

from problog.util import init_logger
from synthlog.keywords import init_cell_pred

from synthlog.tasks.predictors import *
from synthlog.tasks.cluster import *

logger = init_logger()


@problog_export_nondet("+term", "+term", "+list", "+list", "+list", "-term")
def sklearn_predictor(
    scope, predictor_name, source_columns, target_columns, term_list, **kwargs
):
    """
    Learn a scikit-learn predictor on scope. It uses source_columns to predict target_columns
    :param predictor_name: Name of the sklearn predictor to use. For example, "tree.DecisionTreeClassifier" is the name of a decision tree in sklearn.
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the predictor object.
    decision_tree(<predictor> is created, with <predictor> the predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    """
    clf = SKLearnPredictor(
        term2str(predictor_name),
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clf.fit(term_list)
    return clf.output_terms()


@problog_export_nondet("+term", "+list", "+list", "+list", "-term")
def decision_tree(scope, source_columns, target_columns, term_list, **kwargs):
    """
    Learn a decision tree predictor on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the predictor object.
    decision_tree(<predictor> is created, with <predictor> the predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    """
    clf = DecisionTree(
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clf.fit(term_list)
    return clf.output_terms()


@problog_export_nondet("+term", "+list", "+list", "+list", "-term")
def random_forest(scope, source_columns, target_columns, term_list, **kwargs):
    """
    Learn a random forest predictor on scope. It uses source_columns to predict target_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param target_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as columns to predict for the predictor.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the predictor object.
    random_forest(<predictor> is created, with <predictor> the predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the predictor object and <column> is column(<table_name>, <col_number>)
    """
    clf = RandomForest(
        scope,
        source_columns,
        target_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
        parameters={"random_state": 1},  # TODO: Remove this after tests
    )
    clf.fit(term_list)
    return clf.output_terms()


#################################################
########## Clustering ###########################
#################################################
# TODO: Use separate files (makes more sense I think)


@problog_export_nondet("+term", "+term", "+list", "+list", "-term")
def sklearn_clustering(scope, clustering_name, source_columns, term_list, **kwargs):
    """
    Learn a scikit-learn clustering on scope. It uses source_columns
    :param clustering_name: Name of the sklearn predictor to use. For example, "tree.DecisionTreeClassifier" is the name of a decision tree in sklearn.
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the clustering.
        Source column should have numeric values.
    :param kwargs:
    :return: A list of Terms.
    clustering(<clustering>) is created, with <clustering> the clustering object.
    sklearn_clustering(<clustering> is created, with <clustering> the clustering object.
    source(<clustering>, <column>) are created for each source column. <clustering> is the predictor object and <column> is column(<table_name>, <col_number>)
    """
    clust = SKLearnCluster(
        term2str(clustering_name),
        scope,
        source_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
    )
    clust.fit(term_list)
    return clust.output_terms()


@problog_export_nondet("+term", "+list", "+list", "-term")
def kmeans(scope, source_columns, term_list, **kwargs):
    clus = KMeans(
        scope,
        source_columns,
        database=kwargs["database"],
        engine=kwargs["engine"],
        parameters={"n_clusters": 2},
    )
    clus.fit(term_list)
    return clus.output_terms()


#################################################
########## TODOs ###########################
#################################################
# TODO: make MERCS use the subquery (should be fairly easy)
@problog_export_nondet("+term", "+list", "-term")
def mercs(scope, source_columns, **kwargs):
    """
    Learn a MERCS predictor on scope. It uses source_columns to predict any source_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param kwargs:
    :return: A list of Terms.
    predictor(<predictor>) is created, with <predictor> the MERCS predictor object.
    mercs(<predictor> is created, with <predictor> the MERCS predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the MERCS predictor object and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the MERCS predictor object and <column> is column(<table_name>, <col_number>)
    """
    clf = MERCSPredictor(
        scope, source_columns, database=kwargs["database"], engine=kwargs["engine"]
    )
    clf.fit()
    return clf.output_terms()


@problog_export_nondet("+term", "+list", "-term")
def white_mercs(scope, source_columns, **kwargs):
    """
    Learn a MERCS predictor on scope. It exposes its internal trees. It uses source_columns to predict any source_columns
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
        Source column should have numeric values.
    :param kwargs:
    :return: A list of Terms.
    decision_tree(<predictor>), with <predictor> the decision tree predictor internal to MERCS.
    predictor(<predictor>) is created, with <predictor> the predictor (MERCS or internal decision tree) object.
    mercs(<predictor> is created, with <predictor> the MERCS predictor object.
    target(<predictor>, <column>) are created for each target column. <predictor> is the predictor object (MERCS or internal decision tree) and <column> is column(<table_name>, <col_number>)
    source(<predictor>, <column>) are created for each source column. <predictor> is the predictor object (MERCS or internal decision tree) and <column> is column(<table_name>, <col_number>)
    """
    clf = MERCSWhiteBoxPredictor(
        scope, source_columns, database=kwargs["database"], engine=kwargs["engine"]
    )
    clf.fit()
    return clf.output_terms()


# THIS IS AN OLD VERSION, NON PROBABILISTIC ONE!
# @problog_export_nondet("+term", "+term", "+list", "-term")
def predict(scope, predictor, source_columns, **kwargs):
    """
    Predict values using a predictor that was fitted on data. It uses source_columns of scope to predict the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param predictor: A predictor, stored as a Problog Object (accessible through predictor(<predictor>) for example).
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

    clf = predictor.functor
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


@problog_export_nondet("+term", "+term", "+list", "+list", "-term", "-term")
def predict(scope, predictor, source_columns, table_cell_term_list, **kwargs):
    """
    Predict values using a predictor that was fitted on data. It uses source_columns of scope to predict the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param predictor: A predictor, stored as a Problog Object (accessible through predictor(<predictor>) for example).
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
    :param table_cell_term_list: List of cell terms, representing the data that will be used for the prediction
    :param pred_name: Name of the predicate that will be used as output predicate
    :param kwargs:
    :return: Predictions from predictor using source_columns of scope, as well as predictions metadata.
    prediction(<scope>, <predictor>, <source_columns>) is created. <scope> is the scope parameter, as a Problog object, <predictor> is the predictor parameter, as a Problog object and <source_columns> are the source_columns parameter as a Problog object.
        This whole prediction/3 is used as a key for the prediction object. In the future, it might be better to use a unique identifier or something else!
    cell_pred(<row_id>, <col_id>, <value>, <predictor>) are created for each prediction. <row_id> and <col_id> are (1,1) indexed, NOT from the table_cell row and column ids. cell_pred have an associated probability (1 if no probabilistic prediction is available)
        The <col_id> corresponds to the index of the target column of predictor. <value> is the predicted value. <predictor> is predictor defined above.
    source(<predictor>, <source_column>) are created for each source_column. <predictor> is predictor defined above, <source_column> is column(<table_name>, <col_number>)
    All these tasks have a probability attached to it. This probability is available through the second returned column.
    """
    clf = predictor.functor
    pred_name = "prediction"
    if isinstance(clf, Cluster):
        pred_name = "clustering"

    prediction_term_3 = Term(
        pred_name, Object(scope), predictor, Object(source_columns)
    )

    prediction_term_1 = Term(pred_name, prediction_term_3)

    relevant_table = [
        t for t in table_cell_term_list if t.args[0] == source_columns[0].args[0]
    ]

    matrix = cells_to_matrix(relevant_table)

    src_cols = [s.args[1].value for s in source_columns]

    clf = predictor.functor

    # We first try to use probabilistic prediction
    try:
        y_prob = clf.predict_proba(matrix[:, src_cols])

        if len(y_prob.shape) > 2:
            n_rows, n_classes, n_cols = y_prob.shape
        else:
            n_rows, n_classes = y_prob.shape
            n_cols = 1

        cell_pred_terms = []
        for r, cl, c in product(range(n_rows), range(n_classes), range(n_cols)):
            if n_cols > 1:
                proba = y_prob[r, cl, c]
            else:
                proba = y_prob[r, cl]
            cell_pred_terms.append(
                (
                    init_cell_pred(
                        r + 1, c + 1, clf.model.classes_[cl].item(), predictor
                    ),
                    # init_cell_pred(r + 1, c + 1, clf.model.classes_[cl], prediction_term_3),
                    Constant(proba),
                )
            )
    # If predict_proba is not found, we use the non probabilistic prediction
    except AttributeError:
        y_pred = clf.predict(matrix[:, src_cols])

        if len(y_pred.shape) == 1:
            y_pred = np.atleast_2d(y_pred).T

        n_rows, n_cols = y_pred.shape

        cell_pred_terms = []
        for r, c in product(range(n_rows), range(n_cols)):
            cell_pred_terms.append(
                (
                    init_cell_pred(r + 1, c + 1, y_pred[r, c].item(), predictor),
                    Constant(1),
                )
            )

    # predictor_term = [(Term("predictor", prediction_term_3, predictor), Constant(1))]
    source_terms = [
        # (Term("source", prediction_term_3, s), Constant(1)) for s in source_columns
        (Term("source", predictor, s), Constant(1))
        for s in source_columns
    ]

    # TODO: Add a real confidence, based on some metrics?
    # TODO: Add doc for confidence predicate, when it is finalized
    confidence_term = (
        Term("confidence", predictor, Constant(clf.confidence)),
        Constant(1),
    )

    return (
        [(prediction_term_1, Constant(1)), (prediction_term_3, Constant(1))]
        + cell_pred_terms
        # + predictor_term
        + source_terms
        + [confidence_term]
    )
