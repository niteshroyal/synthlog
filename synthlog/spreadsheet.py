from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, unquote, term2str, Clause
from problog.errors import UserError, InvalidValue

import os
import openpyxl as xls
import csv
import numpy as np

from tacle import tables_from_cells, learn_from_csv, learn_from_cells, Constraint
from tacle.core.template import AllDifferent
from tacle.indexing import Orientation, Table, Range

from synthlog.keywords import (
    init_cell,
    init_table,
    init_table_cell,
    init_table_cell_type,
    init_table_header,
    init_constraint,
)


#######################
#                     #
#       ProbLog       #
#       Exports       #
#                     #
#######################


@problog_export_nondet("+str", "-term")
def load_spreadsheet(filename):
    """
    Load a excel spreadsheet into Synthlog.
    Output is a list of cell terms, containing the values of the first excel spreadsheet.
    For example, if on the second row and third column, there is a cell having value 6, the term cell(1, 2, 3, 6). will be created.
    Usage:
    .. code-block:: prolog
        magic:X :- load_spreadsheet("magic.xlsx", X).

    where X grounds to all non-empty cells in the spreadsheet.
    :param filename: Filename of the xlsx file to load. If relative, the base directory is ...
    :type filename: string
    :return A list of cell terms. A cell term is cell(<sheet_id>, <row_id>, <column_id>, <value>)
    :rtype list of Term
    """
    # Resolve the filename with respect to the main Prolog file location.
    workbook = problog_export.database.resolve_filename(filename)
    if not os.path.isfile(workbook):
        raise UserError("Can't find spreadsheet '%s'" % workbook)

    # Load the Excel workbook.
    wb = xls.load_workbook(workbook)

    res = []
    for row in wb.active.iter_rows():
        for cell in row:
            if cell.value is not None:
                res.append(init_cell(cell.row, cell.col_idx, cell.value))
    return res


@problog_export_nondet("+str", "-term")
def load_csv(filename):
    # Resolve the filename with respect to the main Prolog file location.
    csv_file = problog_export.database.resolve_filename(filename)
    if not os.path.isfile(csv_file):
        raise UserError("Can't find CSV file '%s'" % csv_file)

    with open(csv_file) as csv_ref:
        csv_reader = csv.reader(csv_ref, delimiter=",")
        result = []
        for i, row in enumerate(csv_reader):
            for j, cell in enumerate(row):
                if cell is not None:
                    result.append(init_cell(i + 1, j + 1, cell))
    return result


@problog_export_nondet("+term", "-term")
def detect_tables(scope, **kwargs):
    """
    Query the cells of a scope and return table, table_cell and table_cell_type predicates
    :param scope: The scope
    :type scope: Problog Term
    :param kwargs: The keyword arguments used by Problog to give the reference on the database and the engine
    :return: list of Terms that is used in Problog unification (one unification by Term)
    """
    engine = kwargs["engine"]
    database = kwargs["database"]
    cell_term_list = [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == "cell"
    ]
    matrix = cells_to_matrix(cell_term_list)
    for i in range(matrix.shape[0]):
        for j in range(matrix.shape[1]):
            if matrix[i, j] is None:
                matrix[i, j] = ""
    tables = tables_from_cells(matrix, Orientation.vertical)
    result = []
    for table in tables:
        result.append(
            init_table(
                table.name,
                table.range.row + 1,
                table.range.column + 1,
                table.range.height,
                table.range.width,
            )
        )

        for j in range(table.range.width):
            result.append(
                init_table_header(table.name, j + 1, matrix[table.range.row - 1, j])
            )
            for i in range(table.range.height):
                result.append(
                    init_table_cell(table.name, i + 1, j + 1, table.data[i, j])
                )
                result.append(
                    init_table_cell_type(
                        table.name, i + 1, j + 1, table.type_data[i, j]
                    )
                )
    return result


@problog_export_nondet("+term", "-term")
def tacle(scope, **kwargs):
    tables = scope_to_tables(scope, kwargs)
    data = cells_to_matrix(get_terms_from_scope(scope, "cell", kwargs))
    constraints = learn_from_cells(data, tables=tables)
    return [init_constraint(c) for c in constraints]


def translate_constraint(constraint: Constraint):
    if isinstance(constraint.template, AllDifferent):
        # ensure_false :- table_cell('T1', R1, 4, V), table_cell('T1', R2, 4, V), R1 \= R2.
        return Clause(Term("ensure_false"), Term("'{}'".format()))


@problog_export_nondet("+term", "+term", "+term", "+term", "+term", "-term")
def matrix_to_atoms(table_name, header, cell_row, cell_type, cell_value, **kwargs):
    table_name = unquote(str(table_name)).lower()
    header = unquote(str(header)).lower()
    cell_row = unquote(str(cell_row)).lower()
    cell_type = unquote(str(cell_type)).lower()
    cell_value = unquote(str(cell_value)).lower()

    row_id = table_name + "_r" + cell_row

    result = []
    result.append(Term(header, Constant(row_id), Constant(cell_value)))
    if cell_type == "string":
        result.append(Term(header + "_" + cell_value, Constant(row_id)))

    return result


#######################
#                     #
#      Functions      #
#                     #
#######################


def cells_to_matrix(cell_terms):
    return convert_to_matrix(
        cell_terms,
        lambda t: t.args[0].value,
        lambda t: t.args[1].value,
        lambda t: t.args[2].value,
    )


def table_cell_types_to_matrix(table_cell_type_terms):
    return table_cells_to_matrix(table_cell_type_terms)


def table_cells_to_matrix(table_cell_terms):
    return convert_to_matrix(
        table_cell_terms,
        lambda t: t.args[1].value,
        lambda t: t.args[2].value,
        lambda t: t.args[3].value,
    )


def scope_to_tables(scope, kwargs):
    tables = get_terms_from_scope(scope, "table", kwargs)
    table_cells = get_terms_from_scope(scope, "table_cell", kwargs)
    table_types = get_terms_from_scope(scope, "table_cell_type", kwargs)

    tacle_tables = []
    for table in tables:
        table_name = unquote(term2str(table.args[0].value))
        data = table_cells_to_matrix(
            [c for c in table_cells if c.args[0] == table.args[0]]
        )
        type_data = table_cell_types_to_matrix(
            [c for c in table_types if c.args[0] == table.args[0]]
        )
        t_range = Range(
            table.args[2].value - 1,
            table.args[1].value - 1,
            table.args[4].value,
            table.args[3].value,
        )

        table = Table(
            data,
            type_data,
            t_range,
            name=table_name,
            orientations=[Orientation.vertical],
        )
        tacle_tables.append(table)

    return tacle_tables


def convert_to_matrix(terms, extract_y_f, extract_x_f, extract_val_f):
    min_y, max_y, min_x, max_x = [None, None, None, None]
    for term in terms:
        y, x = extract_y_f(term), extract_x_f(term)
        if min_y is None or y < min_y:
            min_y = y
        if max_y is None or y > max_y:
            max_y = y
        if min_x is None or x < min_x:
            min_x = x
        if max_x is None or x > max_x:
            max_x = x
    row = max_y
    column = max_x
    matrix = np.empty(shape=(row, column), dtype=np.object)

    for term in terms:
        matrix[extract_y_f(term) - 1, extract_x_f(term) - 1] = extract_val_f(term)

    return matrix


def get_terms_from_scope(scope, term_name, kwargs):
    engine = kwargs["engine"]
    database = kwargs["database"]
    return [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if t[1].functor == term_name
    ]
