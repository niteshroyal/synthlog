from __future__ import print_function

import ast
import importlib
import numpy as np
import pandas as pd
import sys

from problog.engine_unify import unify_value, UnifyError
from problog.errors import UserError
from problog.extern import (
    problog_export,
    problog_export_class,
    problog_export_raw,
    problog_export_nondet,
)
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

from synthlog.mercs.core.MERCS import MERCS
from synthlog.predict import cells_to_matrix
from synthlog.keywords import init_cell_pred


@problog_export_nondet("+term", "+list", "-term")
def mercs(scope, source_columns, **kwargs):

    # Preliminaries
    engine = kwargs["engine"]
    database = kwargs["database"]

    def short_str(_self):
        return "MERCS({})".format(id(_self))

    MERCS.__repr__ = short_str
    MERCS.__str__ = short_str

    # Verify whether or not a MERCS model already exists with these exact same parameters
    res_predictor_object = [
        t
        for t in engine.query(
            database, Term("predictor_object", None, None, None), subcall=True
        )
    ]

    # If found, return the existing object. If not, create a predictor.
    for r in res_predictor_object:
        if (
            term2str(scope) == r[0].functor
            and r[1].functor == source_columns
        ):
            problog_obj = r[2]
            source_columns = r[1].functor

            predictor_term = Term("predictor", problog_obj)
            mercs_term = Term("mercs", problog_obj)
            target_terms = [Term("target", problog_obj, t) for t in source_columns]
            source_terms = [Term("source", problog_obj, s) for s in source_columns]
            return [predictor_term] + [mercs_term] + source_terms + target_terms

    # Getting input data
    table_cell_term_list = [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == "table_cell"
    ]

    relevant_table = [
        t for t in table_cell_term_list if t.args[0] == source_columns[0].args[0]
    ]

    # Filter data
    matrix = cells_to_matrix(relevant_table)
    src_cols = [s.args[1].value for s in source_columns]
    matrix = matrix[:, src_cols]

    # Train a MERCS model
    clf = MERCS()

    data = pd.DataFrame(matrix)  # MERCS still needs this (elia: I'm so sorry)
    clf.fit(data)

    problog_obj = Object(clf)

    # We add the new predictor in the database to be able to retrieve it in future calls
    database.add_fact(
        Term(
            "predictor_object",
            scope,
            Object(source_columns),
            problog_obj,
        )
    )

    predictor_term = Term("predictor", problog_obj)
    mercs_term = Term("mercs", problog_obj)
    target_terms = [Term("target", problog_obj, t) for t in source_columns]
    source_terms = [Term("source", problog_obj, s) for s in source_columns]

    return [predictor_term] + source_terms + target_terms + [mercs_term]
