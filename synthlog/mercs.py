from __future__ import print_function

import ast
import importlib
import numpy as np
import pandas as pd
import sys

from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor

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
            mercs_problog_object = r[2]
            source_columns = r[1].functor

            predictor_term = Term("predictor", mercs_problog_object)
            mercs_term = Term("mercs", mercs_problog_object)
            target_terms = [Term("target", mercs_problog_object, t) for t in source_columns]
            source_terms = [Term("source", mercs_problog_object, s) for s in source_columns]
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

    mercs_problog_object = Object(clf)

    # We add the new predictor in the database to be able to retrieve it in future calls
    database.add_fact(
        Term(
            "predictor_object",
            scope,
            Object(source_columns),
            mercs_problog_object,
        )
    )

    predictor_term = Term("predictor", mercs_problog_object)
    mercs_term = Term("mercs", mercs_problog_object)
    target_terms = [Term("target", mercs_problog_object, t) for t in source_columns]
    source_terms = [Term("source", mercs_problog_object, s) for s in source_columns]

    # Whitebox
    dt_terms = []
    for dt, dt_code in zip(clf.m_list, clf.m_codes):

        def short_str(_self):
            return "DT({})".format(id(_self))

        DecisionTreeRegressor.__str__ = short_str
        DecisionTreeRegressor.__repr__ = short_str
        DecisionTreeClassifier.__str__ = short_str
        DecisionTreeClassifier.__repr__ = short_str

        # dt.__str__ = short_str
        # dt.__repr__ = short_str

        dt_problog_object = Object(dt)
        dt_predictor_term = Term("predictor", dt_problog_object)
        decision_tree_term = Term("decision_tree", dt_problog_object)

        dt_source_columns = [x for i, x in enumerate(source_columns)
                             if dt_code[i] == 0]
        dt_target_columns = [x for i, x in enumerate(source_columns)
                             if dt_code[i] == 1]

        dt_target_terms = [Term("target", dt_problog_object, t) for t in dt_target_columns]
        dt_source_terms = [Term("source", dt_problog_object, s) for s in dt_source_columns]

        dt_terms.append(dt_predictor_term)
        dt_terms.append(decision_tree_term)
        dt_terms.extend(dt_target_terms)
        dt_terms.extend(dt_source_terms)

        database.add_fact(
            Term(
                "predictor_object",
                scope,
                Object(dt_source_terms),
                Object(dt_target_terms),
                dt_problog_object,
            )
        )

    return [predictor_term] + source_terms + target_terms + [mercs_term] + dt_terms
