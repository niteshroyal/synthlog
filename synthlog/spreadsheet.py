from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import (
    Term,
    term2list,
    Constant,
    unquote,
    term2str,
    Clause,
    Object,
    Var,
    list2term,
)
from problog.errors import UserError, InvalidValue

import os
import openpyxl as xls
import csv
import numpy as np

from tacle import tables_from_cells, learn_from_csv, learn_from_cells, Constraint
from tacle.core.template import AllDifferent
from tacle.indexing import Orientation, Table, Range

# This is for the excel add-in, to be able to use synthlog
# It works because spreadsheet.py is the first import
# TODO: Do something smarter and more general instead
import sys
parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
if not os.path.abspath(parent_dir) in sys.path:
	sys.path.append(os.path.abspath(parent_dir))

from synthlog.keywords import (
    init_cell,
    init_table,
    init_table_cell,
    init_table_cell_type,
    init_table_header,
    init_constraint,
)

# Problog keeps calling the loading/detect table functions, so we cache some of the results
detect_table_dict = {}
loaded_csv = {}
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
    if filename in loaded_csv:
        return loaded_csv[filename]

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
    loaded_csv[filename] = result
    return result


def convert(value, type):
    if type == "int":
        return int(value)
    if type == "string":
        return str(value)
    if type == "float":
        return float(value)
    return value


# @problog_export_nondet("+term", "-term")
def detect_tables(scope, **kwargs):
    """
    Query the cells of a scope and return table, table_cell and table_cell_type predicates
    :param scope: The scope
    :type cell_term_list: Problog Term
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

        table_header_type = []
        table_header_unique_values = []
        for j in range(table.range.width):
            table_header_type.append("int")
            table_header_unique_values.append(set())

            for i in range(table.range.height):
                result.append(
                    init_table_cell(
                        table.name,
                        i + 1,
                        j + 1,
                        convert(
                            table.data[i, j], table.type_data[i, j]
                        ),  # We convert to the right data type
                    )
                )

                result.append(
                    init_table_cell_type(
                        table.name, i + 1, j + 1, table.type_data[i, j]
                    )
                )

                if table.type_data[i, j] == "string":
                    table_header_type[j] = "string"

                table_header_unique_values[j].add(table.data[i, j])

        for j in range(table.range.width):
            if table_header_type[j] == "string":
                result.append(
                    init_table_header(
                        table.name,
                        j + 1,
                        matrix[table.range.row - 1, j],
                        "string",
                        list(table_header_unique_values[j]),
                    )
                )
            else:
                result.append(
                    init_table_header(
                        table.name,
                        j + 1,
                        matrix[table.range.row - 1, j],
                        table_header_type[j],
                        list(table_header_unique_values[j]),
                    )
                )
    return result


@problog_export_nondet("+list", "-term")
def detect_cell_tables(cell_term_list, **kwargs):
    """
    Query the cells of a scope and return table, table_cell and table_cell_type predicates
    :param scope: The scope
    :type cell_term_list: Problog Term
    :param kwargs: The keyword arguments used by Problog to give the reference on the database and the engine
    :return: list of Terms that is used in Problog unification (one unification by Term)
    """

    def hashfunc(l):
        return hash("".join([str(hash(e)) for e in l]))

    engine = kwargs["engine"]
    database = kwargs["database"]
    if hashfunc(cell_term_list) in detect_table_dict:
        return detect_table_dict[hashfunc(cell_term_list)]

    # # We try to retrieve the model trained with the same parameters
    # res_predictor_object = [
    #     t
    #     for t in engine.query(
    #         database, Term("detected_cell_tables", None, None), subcall=True
    #     )
    # ]
    #
    # # If we succeed, we retrieve the previously trained object.
    # # If not, we train a new one
    # for r in res_predictor_object:
    #     if r[0].functor == hashfunc(cell_term_list):
    #         return r[1].functor
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

        table_header_type = []
        table_header_unique_values = []
        for j in range(table.range.width):
            table_header_type.append("int")
            table_header_unique_values.append(set())

            for i in range(table.range.height):
                result.append(
                    init_table_cell(
                        table.name,
                        i + 1,
                        j + 1,
                        convert(
                            table.data[i, j], table.type_data[i, j]
                        ),  # We convert to the right data type
                    )
                )

                result.append(
                    init_table_cell_type(
                        table.name, i + 1, j + 1, table.type_data[i, j]
                    )
                )

                if table.type_data[i, j] == "string":
                    table_header_type[j] = "string"

                table_header_unique_values[j].add(table.data[i, j])

        for j in range(table.range.width):
            if table_header_type[j] == "string":
                result.append(
                    init_table_header(
                        table.name,
                        j + 1,
                        matrix[table.range.row - 1, j],
                        "string",
                        list(table_header_unique_values[j]),
                    )
                )
            else:
                result.append(
                    init_table_header(
                        table.name,
                        j + 1,
                        matrix[table.range.row - 1, j],
                        table_header_type[j],
                        list(table_header_unique_values[j]),
                    )
                )

    detect_table_dict[hashfunc(cell_term_list)] = result
    database.add_fact(
        Term("detected_cell_tables", Object(hashfunc(cell_term_list)), Object(result))
    )
    return result


@problog_export_nondet("+term", "-term")
def tacle(scope, **kwargs):
    tables = scope_to_tables(scope, kwargs)
    data = cells_to_matrix(get_terms_from_scope(scope, "cell", **kwargs))
    constraints = learn_from_cells(data, tables=tables)
    return [init_constraint(c) for c in constraints]


def translate_constraint(constraint: Constraint):
    if isinstance(constraint.template, AllDifferent):
        block = constraint[AllDifferent.x]
        print(block)
        # ensure_false :- table_cell('T1', R1, 4, V), table_cell('T1', R2, 4, V), R1 \= R2.
        return Clause(
            Term("ensure_false"), init_table_cell(block.table.name, "R1", block)
        )


@problog_export_nondet(
    "+term", "+term", "+term", "+term", "+term", "+term", "-term", "-float"
)
def cell_to_atoms(
    table_name,
    header,
    cell_row,
    column_type,
    column_unique_values,
    cell_value,
    **kwargs
):
    table_name = unquote(str(table_name)).lower()
    header = unquote(str(header)).lower()
    cell_row = unquote(str(cell_row)).lower()
    column_type = unquote(str(column_type)).lower()
    cell_value = unquote(str(cell_value)).lower()
    column_unique_values1 = term2list(column_unique_values)
    # print(column_unique_values1)

    row_id = table_name + "_r" + cell_row

    # result = [Term(header, Constant(row_id), Constant(cell_value))]
    result = []

    result.append((Term("row", Constant(row_id)), 1.0))

    if column_type == "string":
        for unique_value in column_unique_values1:
            if unquote(unique_value).lower() == cell_value:
                result.append((Term(header + "_" + cell_value, Constant(row_id)), 1.0))
            else:
                # WIth Probability 0
                result.append(
                    (
                        Term(
                            header + "_" + unquote(unique_value).lower(),
                            Constant(row_id),
                        ),
                        0.0,
                    )
                )
    return result


@problog_export_nondet("+term", "+term", "-term")
def scope_minus(scope1, scope2, **kwargs):
    terms1 = set(get_terms_from_scope(scope1, **kwargs))
    terms2 = set(get_terms_from_scope(scope2, **kwargs))
    return terms1 - terms2


#######################
#                     #
#      Functions      #
#                     #
#######################


def cells_to_matrix(cell_terms):
    return convert_to_matrix(
        cell_terms,
        lambda t: t.args[0].functor if isinstance(t, Term) else t.args[0],
        lambda t: t.args[1].functor if isinstance(t, Term) else t.args[1],
        lambda t: t.args[2].functor if isinstance(t, Term) else t.args[2],
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
    tables = get_terms_from_scope(scope, "table", **kwargs)
    table_cells = get_terms_from_scope(scope, "table_cell", **kwargs)
    table_types = get_terms_from_scope(scope, "table_cell_type", **kwargs)

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


def get_terms_from_scope(scope, term_name=None, **kwargs):
    engine = kwargs["engine"]
    database = kwargs["database"]
    return [
        t[1]
        for t in engine.query(database, Term("':'", scope, None), subcall=True)
        if term_name is None or t[1].functor == term_name
    ]
