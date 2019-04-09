from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, unquote
from problog.errors import UserError, InvalidValue

import os
import openpyxl as xls


@problog_export_nondet('+str', '-term')
def load_spreadsheet(filename):
    # Resolve the filename with respect to the main Prolog file location.
    workbook = problog_export.database.resolve_filename(filename)
    if not os.path.isfile(workbook):
        raise UserError('Can\'t find spreadsheet \'%s\'' % workbook)

    # Load the Excel workbook.
    wb = xls.load_workbook(workbook)

    res = []
    for row in wb.active.iter_rows():
        for cell in row:
            if cell.value:
                res.append(Term("cell", Constant(1), Constant(cell.row),
                                Constant(cell.col_idx), Constant(cell.value)))
    return res
