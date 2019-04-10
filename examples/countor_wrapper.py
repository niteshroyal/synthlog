# from extended_db import save
import dill as pickle
import sys

from problog.extern import problog_export, problog_export_raw
from problog.logic import Constant, Object, Term, Var, unquote, term2list
from problog.errors import UserError

from countor.count_or import learnConstraints


@problog_export("+term", "-term")
def learn_count_or_constraints(matrix, *args, **kwargs):
    print(matrix, type(matrix), dir(matrix))
    print(matrix.functor)
    print(matrix.functor.matrix)
    quit()
    m = matrix.functor.matrix
    constraints = co.learnConstraints(m, m.shape)
    return Object(constraints)


@problog_export_raw("+term", "+term", "+term")
def load_count_or_constraints(workbook_id, worksheet_id, *args, **kwargs):
    engine = kwargs["engine"]
    database = kwargs["database"]
    rows = engine.query(
        database,
        Term("countor_constraint", workbook_id, worksheet_id, None),
        subcall=True,
    )
    return [
        (workbook_id, worksheet_id, Object(pickle.loads(row[2].value))) for row in rows
    ]


@problog_export("+term")
def print_count_or_constraint(constraint):
    print(constraint.functor)
    return ()


@problog_export("+str", "+term", "+term", "+term")
def save_count_or_constraint(filename, workbook_id, worksheet_id, constraint):
    pickled_constraint = pickle.dumps(constraint.functor)
    types = ["INT", "INT", "BLOB"]
    save(
        filename,
        "countor_constraint",
        types,
        workbook_id.value,
        worksheet_id.value,
        pickled_constraint,
    )
    return ()
