from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Clause, Constant


@problog_export_nondet("+term", "-term", "-term", "-term")
def unify_clause(clause):
    if isinstance(clause, Clause):
        head = clause.head
        proba = head.probability
        if proba is None:
            proba = 1
        if not isinstance(proba, Constant):
            proba = Constant(proba)
        head.probability = None
        return [(head, proba, clause.body)]
    return []
