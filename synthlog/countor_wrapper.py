# from extended_db import save
import dill as pickle
from problog.extern import problog_export, problog_export_raw, problog_export_nondet
from problog.logic import Object, Term
from countor.count_or import learnConstraints
from countor.count_or import cleanData
import numpy as np
import synthlog.spreadsheet as spreadsheet


@problog_export_nondet("+term",  "-term")
def learn_count_or_constraints(scope,  **kwargs):
    table_cell_term_list=spreadsheet.get_terms_from_scope(scope,"table_cell",kwargs)
    matrix = spreadsheet.table_cells_to_matrix(table_cell_term_list)
    dataTensor,variables=cleanData(matrix)
    constraints = learnConstraints(dataTensor,dataTensor.shape)
    print(constraints)
    return constraints

@problog_export("+term", "-term")
def get_count_or_constraints(matrix, *args, **kwargs):
#    print(matrix, type(matrix), dir(matrix))
#    print(matrix.functor)
    data=np.asarray(matrix.functor.matrix)
    dataTensor,variables=cleanData(data)
#    print(dataTensor,variables)
#    quit()
#    m = matrix.functor.matrix
    constraints = learnConstraints(dataTensor,dataTensor.shape)
    print(constraints)
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
