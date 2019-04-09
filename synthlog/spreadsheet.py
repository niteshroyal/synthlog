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
            if cell.value:
                res.append(
                    Term(
                        "cell",
                        Constant(cell.row),
                        Constant(cell.col_idx),
                        Constant(cell.value),
                    )
                )
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
                if cell:
                    result.append(
                        Term(
                            "cell",
                            Constant(i),
                            Constant(j),
                            Constant(cell),
                        )
                    )
    return result


@problog_export_nondet("+list", "-term")
def detect_tables(cell_terms):
    raise RuntimeError(repr(cell_terms))


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
    row = max_y - 1
    column = max_x - 1
    matrix = np.empty(shape=(row, column))

    for cell_term in cell_term_list:
        matrix[cell_term.args[0].value-1, cell_term.args[1].value-1] = cell_term.args[2].value

    return matrix
