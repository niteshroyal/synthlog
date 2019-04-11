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

    # Getting input data
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

    # Filter data
    matrix = cells_to_matrix(relevant_table)
    src_cols = [s.args[1].value for s in source_columns]
    matrix = matrix[:, src_cols]

    # Train a MERCS model
    clf = MERCS()

    data = pd.DataFrame(matrix)  # MERCS still needs this (elia: I'm so sorry)
    clf.fit(data)

    predictor_term = Term("predictor", Object(clf))
    mercs_term = [Term("mercs", Object(clf))]
    source_terms = [Term("source", Object(clf), s) for s in source_columns]
    target_terms = [
        Term("target", Object(clf), s) for s in source_columns
    ]  # MERCS can predict everything it had as input

    # Whitebox

    # print(source_terms[0].args[0])

    return [predictor_term] + mercs_term + source_terms + target_terms
