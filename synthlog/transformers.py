from problog.extern import (
    problog_export,
    problog_export_class,
    problog_export_raw,
    problog_export_nondet,
)

from sklearn.preprocessing import OrdinalEncoder

import importlib
import sys
import ast

import numpy as np

from itertools import product

from synthlog.keywords import init_table_cell, init_cell_transform
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

from synthlog.predict import cells_to_matrix


@problog_export_nondet("+term", "+list", "-term")
def ordinal_encoder(scope, source_columns, **kwargs):
    """
    Fit OrdinalEncoder transformer on scope. It uses source_columns to learn the transformation
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
    :param kwargs:
    :return: A list of Terms.
    transformer(<transformer>) is created, with <transformer> the scikit-learn transformer object.
    ordinal_encoder(<transformer>) is created, with <transformer> the scikit-learn transformer object.
    source(<transformer>, <column>) are created for each source column. <transformer> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    """

    def short_str(_self):
        return "OE({})".format(id(_self))

    OrdinalEncoder.__repr__ = short_str
    OrdinalEncoder.__str__ = short_str

    transformer = OrdinalEncoder()
    problog_obj = Object(transformer)
    sklearn_res, problog_obj_back = scikit_learn_transformer(
        scope, source_columns, problog_obj, **kwargs
    )
    ordinal_encoder_term = Term("ordinal_encoder", problog_obj_back)

    return sklearn_res + [ordinal_encoder_term]


def scikit_learn_transformer(scope, source_columns, problog_obj, **kwargs):
    """
    Fit scikit learn transformer on scope. It uses source_columns to learn the transformation
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the predictor.
    :param transformer: The transformer to use
    :param kwargs:
    :return: A tuple list of Terms, problog_object.
    List of Terms is
        transformer(<transformer>) is created, with <transformer> the scikit-learn transformer object.
        source(<transformer>, <column>) are created for each source column. <transformer> is the scikit-learn predictor object and <column> is column(<table_name>, <col_number>)
    problog_object is the transformation object, as a problog object
    """
    engine = kwargs["engine"]
    database = kwargs["database"]

    transformer = problog_obj.functor

    # We try to retrieve the model trained with the same parameters
    res_predictor_object = [
        t
        for t in engine.query(
            database, Term("transformer_object", None, None, None), subcall=True
        )
    ]
    # TODO: Handle probabilistic terms in transformers!
    # If we succeed, we retrieve the previously trained object.
    # If not, we train a new one
    for r in res_predictor_object:
        if term2str(scope) == r[0].functor and r[1].functor == source_columns:
            problog_obj = r[2]
            source_columns = r[1].functor

            transformer_term = Term("transformer", problog_obj)
            source_terms = [Term("source", problog_obj, s) for s in source_columns]
            return [transformer_term] + source_terms, problog_obj

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

    transformer.fit(matrix[:, src_cols])

    # We add the new predictor in the database to be able to retrieve it in future calls
    database.add_fact(
        Term("transformer_object", scope, Object(source_columns), problog_obj)
    )

    transformer_term = Term("transformer", problog_obj)
    source_terms = [Term("source", problog_obj, s) for s in source_columns]

    return [transformer_term] + source_terms, problog_obj


@problog_export_nondet("+term", "+list", "+term", "+list", "-term")
def trans(scope, term_list, transformer, source_columns, **kwargs):
    """
    Transform values using a transformer that was fitted on data. It uses source_columns of scope to transform the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param transformer: A scikit-learn transformer, stored as a Problog Object (accessible through transformer(<transformer>) of the ordinal_encoder function for example).
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the transformer.
    :param kwargs:
    :return: Transformations from transformer using source_columns of scope, as well as transformation metadata.
    transformation(<scope>, <transformer>, <source_columns>) is created. <scope> is the scope parameter, as a Problog object, <transformer> is the transformer parameter, as a Problog object and <source_columns> are the source_columns parameter as a Problog object.
        This whole transformation/3 is used as a key for the transformation object. In the future, it might be better to use a unique identifier or something else!
    cell_transform(<row_id>, <col_id>, <value>, <transformation_term>) are created for each transformation. <row_id> and <col_id> are (1,1) indexed, NOT indexed from the table_cell row and column ids.
        The <col_id> corresponds to the index of the target column of transformer. <value> is the transformed value. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above.
    transformer(<transformation_term>, <transformer>) is created. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <transformer> is the transformer parameter, as a Problog object
    source(<transformation_term>, <source_column>) are created for each source_column. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <source_column> is column(<table_name>, <col_number>)
    """
    return transformation(
        scope,
        term_list,
        transformer,
        source_columns,
        transformer.functor.transform,
        **kwargs
    )


@problog_export_nondet("+term", "+list", "+term", "+list", "-term")
def inverse_trans(scope, term_list, transformer, source_columns, **kwargs):
    """
    Transform values using a transformer that was fitted on data. It uses source_columns of scope to transform the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param transformer: A scikit-learn transformer, stored as a Problog Object (accessible through transformer(<transformer>) of the ordinal_encoder function for example).
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the transformer.
    :param kwargs:
    :return: Transformations from transformer using source_columns of scope, as well as transformation metadata.
    transformation(<scope>, <transformer>, <source_columns>) is created. <scope> is the scope parameter, as a Problog object, <transformer> is the transformer parameter, as a Problog object and <source_columns> are the source_columns parameter as a Problog object.
        This whole transformation/3 is used as a key for the transformation object. In the future, it might be better to use a unique identifier or something else!
    cell_transform(<row_id>, <col_id>, <value>, <transformation_term>) are created for each transformation. <row_id> and <col_id> are (1,1) indexed, NOT indexed from the table_cell row and column ids.
        The <col_id> corresponds to the index of the target column of transformer. <value> is the transformed value. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above.
    transformer(<transformation_term>, <transformer>) is created. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <transformer> is the transformer parameter, as a Problog object
    source(<transformation_term>, <source_column>) are created for each source_column. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <source_column> is column(<table_name>, <col_number>)
    """
    return transformation(
        scope,
        term_list,
        transformer,
        source_columns,
        transformer.functor.inverse_transform,
        **kwargs
    )


def get_most_probable_world(table_cell_list):
    most_probable_cells = {}
    for proba, cell in table_cell_list:
        coord = (cell.args[1], cell.args[2])
        if not coord in most_probable_cells:
            most_probable_cells[coord] = cell.with_probability(proba)
        else:
            if most_probable_cells[coord].probability < proba:
                most_probable_cells[coord] = cell.with_probability(proba)
    return most_probable_cells.values()


# @problog_export_nondet("+term", "+term", "+list", "-term")
def transformation(scope, term_list, transformer, source_columns, function, **kwargs):
    """
    Transform values using a transformer that was fitted on data. It uses source_columns of scope to transform the data
    :param scope: A scope, containing table_cell predicates describing a table content.
    :param transformer: A scikit-learn transformer, stored as a Problog Object (accessible through transformer(<transformer>) of the ordinal_encoder function for example).
    :param source_columns: A list of columns, where column is: column(<table_name>, <col_number>). <table_name> is a table name present in table_cell. These columns will be used as input columns for the transformer.
    :param function: The function to use for the transformation (either transform or inverse transform)
    :param kwargs:
    :return: Transformations from transformer using source_columns of scope, as well as transformation metadata.
    transformation(<scope>, <transformer>, <source_columns>) is created. <scope> is the scope parameter, as a Problog object, <transformer> is the transformer parameter, as a Problog object and <source_columns> are the source_columns parameter as a Problog object.
        This whole transformation/3 is used as a key for the transformation object. In the future, it might be better to use a unique identifier or something else!
    cell_transform(<row_id>, <col_id>, <value>, <transformation_term>) are created for each transformation. <row_id> and <col_id> are (1,1) indexed, NOT indexed from the table_cell row and column ids.
        The <col_id> corresponds to the index of the target column of transformer. <value> is the transformed value. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above.
    transformer(<transformation_term>, <transformer>) is created. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <transformer> is the transformer parameter, as a Problog object
    source(<transformation_term>, <source_column>) are created for each source_column. <transformation_term> is whole transformation(<scope>, <transformer>, <source_columns>) defined above, <source_column> is column(<table_name>, <col_number>)
    """
    transformation_term_3 = Term(
        "transformation", Object(scope), transformer, Object(source_columns)
    )

    transformation_term_1 = Term("transformation", transformation_term_3)

    # TODO: Handle probabilistic terms in transformers!
    relevant_table = [t for t in term_list if t[1].args[0] == source_columns[0].args[0]]

    most_probable_cells = get_most_probable_world(relevant_table)

    matrix = cells_to_matrix(most_probable_cells)

    src_cols = [s.args[1].value for s in source_columns]

    y_transform = function(matrix[:, src_cols])

    if len(y_transform.shape) == 1:
        y_transform = np.atleast_2d(y_transform).T

    n_rows, n_cols = y_transform.shape

    cell_transform_cells = []
    for r, c in product(range(n_rows), range(n_cols)):
        cell_transform_cells.append(
            init_cell_transform(r + 1, c + 1, y_transform[r, c], transformation_term_3)
        )

    transformer_term = [Term("transformer", transformation_term_3, transformer)]
    source_terms = [Term("source", transformation_term_3, s) for s in source_columns]

    return (
        [transformation_term_1, transformation_term_3]
        + cell_transform_cells
        + transformer_term
        + source_terms
    )
