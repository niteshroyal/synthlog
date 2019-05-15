from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Clause, Constant
from copy import deepcopy


@problog_export_nondet("+term", "-term", "-term", "-term")
def unify_clause(clause):
    if isinstance(clause, Clause):
        head = deepcopy(clause.head)
        proba = head.probability
        if proba is None:
            proba = 1
        head.probability = None
        if not isinstance(proba, Constant):
            proba = Constant(proba)
        return [(head, Constant(proba), clause.body)]
    return []
