from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Clause, Constant, Term
from copy import deepcopy

existing_terms = set()


@problog_export_raw("+term")
def store_existing_term(term, *args, **kwargs):
    global existing_terms
    existing_terms.add(term)
    return []


@problog_export_raw("+term")
def is_not_existing_term(term, *args, **kwargs):
    global existing_terms
    res = []
    if term not in existing_terms:
        res.append((term,))
    return res


@problog_export_raw("+term")
def standard_term(term, *args, **kwargs):
    if term.functor in ("\\+", "not"):
        return []
    return [(term,)]


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


@problog_export("+term", "+term")
def apply_clause(scope, clause, database=None, *args, **kwargs):
    if isinstance(clause, Clause):
        database.add_clause(
            Clause(
                Term(
                    "':'",
                    scope,
                    clause.head.with_probability(None),
                    p=clause.head.probability,
                ),
                Term("evaluate", scope, clause.body),
            )
        )
    return ()
