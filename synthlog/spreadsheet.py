from __future__ import print_function

from problog import get_evaluatable
from problog.extern import (
    problog_export,
    problog_export_nondet,
    problog_export_raw,
)

from problog.logic import Term, term2list, Constant, unquote
from problog.errors import UserError, InvalidValue

import os
import openpyxl as xls
import csv
import numpy as np

from problog.program import PrologString

from tacle import tables_from_cells
from tacle.indexing import Orientation


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
    print(tables)
    result = []
    for table in tables:
        result.append(
            Term(
                "table",
                Constant(table.name),
                Constant(table.range.row + 1),
                Constant(table.range.column + 1),
                Constant(table.range.height),
                Constant(table.range.width),
            )
        )

        for j in range(table.range.width):
            result.append(
                Term(
                    "table_header",
                    Constant(table.name),
                    Constant(table.range.row),
                    Constant(j + 1),
                    Constant(matrix[table.range.row - 1, j]),
                )
            )
            for i in range(table.range.height):
                result.append(
                    Term(
                        "table_cell",
                        Constant(table.name),
                        Constant(i + 1),
                        Constant(j + 1),
                        Constant(table.data[i, j]),
                    )
                )
                result.append(
                    Term(
                        "table_cell_type",
                        Constant(table.name),
                        Constant(i + 1),
                        Constant(j + 1),
                        Constant(table.type_data[i, j]),
                    )
                )
    return result


#######################
#                     #
#      Functions      #
#                     #
#######################


def cells_to_matrix(cell_term_list):
    min_y, max_y, min_x, max_x = [None, None, None, None]
    for cell_term in cell_term_list:
        y, x = cell_term.args[0].value, cell_term.args[1].value
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

    for cell_term in cell_term_list:
        matrix[
            cell_term.args[0].value - 1, cell_term.args[1].value - 1
        ] = cell_term.args[2].value

    return matrix


def init_cell(row_id, col_id, value, p=None):
    """
    Initialize a cell predicate
    :param row_id: The cell row ID
    :type row_id: int

    :param col_id: The cell column ID
    :type col_id: int

    :param value: The cell value
    :type value: str

    :param p: The cell probability (optional)
    :type p: float

    :return: The cell Term
    :rtype: Problog Term
    """
    return Term(
        "cell", Constant(row_id), Constant(col_id), Constant(value), p=p
    )
