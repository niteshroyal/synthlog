from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Clause


@problog_export_nondet("+term", "-term", "-term")
def unify_clause(clause):
    if isinstance(clause, Clause):
        return [(clause.head, clause.body)]
    return []
