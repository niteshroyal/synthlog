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

    encoder = OrdinalEncoder()

    encoder.fit(matrix[:, src_cols])

    transformer_term = Term("transformer", Object(encoder))
    ordinal_encoder_term = Term("ordinal_encoder", Object(encoder))
    col_transformation_terms = [
        Term("source", Object(encoder), s) for s in source_columns
    ]

    return [transformer_term, ordinal_encoder_term] + col_transformation_terms


@problog_export_nondet("+term", "+term", "+list", "-term")
def transform(scope, transformer, source_columns, **kwargs):
    """
    Transfomr values using a transformer that was fitted on data. It uses source_columns of scope to transform the data
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
    transformation_term_3 = Term(
        "transformation", Object(scope), Object(transformer), Object(source_columns)
    )

    transformation_term_1 = Term("transformation", transformation_term_3)

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

    transfomer_instance = transformer.functor
    y_transform = transfomer_instance.transform(matrix[:, src_cols])

    if len(y_transform.shape) == 1:
        y_transform = np.atleast_2d(y_transform).T

    n_rows, n_cols = y_transform.shape

    cell_transform_cells = []
    for r, c in product(range(n_rows), range(n_cols)):
        cell_transform_cells.append(
            init_cell_transform(r + 1, c + 1, y_transform[r, c], transformation_term_3)
        )

    transformer_term = [Term("transformer", transformation_term_3, Object(transformer))]
    source_terms = [Term("source", transformation_term_3, s) for s in source_columns]

    return (
        [transformation_term_1, transformation_term_3]
        + cell_transform_cells
        + transformer_term
        + source_terms
    )
