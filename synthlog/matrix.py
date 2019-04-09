import numpy as np
import sqlite3

from problog.extern import problog_export, problog_export_raw
from problog.logic import Constant, Object, Term, Var, unquote, term2list
from problog.errors import UserError

import logging

logger = logging.getLogger("problog")


#######################
#                     #
#       ProbLog       #
#       Exports       #
#                     #
#######################


@problog_export("-term")
@problog_export("+str", "-term")
def init_matrix(sheet_format=None):
    sformat = StandardSpreadsheetFormat()
    # TODO pass the shifts as parameters
    if sheet_format == "nurse":
        sformat = NurseSpreadsheetFormat(3)
    return Object(SpreadsheetMatrix(sformat))


@problog_export("+term", "+str", "+term", "+term", "+term", "+term")
def load_blocks(matrix, filename, wid, sid, row_slice, col_slice):
    if type(matrix) != int:
        m = matrix.functor
        m.set_load_cells_parameters(
            unquote(filename), wid.functor, sid.functor, row_slice, col_slice
        )
    return ()


@problog_export_raw("+term", "+term", "+term", "+term")
def generate_axis(matrix, axis, index, *args, **kwargs):
    return generate_term_array(matrix, axis, index, None, **kwargs)


@problog_export_raw("+term", "+term", "+term")
def generate_cell(matrix, indices, *args, **kwargs):
    force_load(matrix, **kwargs)
    m = matrix.functor
    list_indices = term2list(indices)
    cell = m.get_cell(*list_indices)
    if cell:
        return [(matrix, indices, cell)]
    return []


@problog_export_raw("+term", "+term", "+term")
def generate_column(matrix, index, *args, **kwargs):
    return generate_term_array(matrix, 1, index, "column", **kwargs)


@problog_export_raw("+term", "+term")
def generate_columns(matrix, *args, **kwargs):
    return generate_terms_array(matrix, 1, "column", **kwargs)


@problog_export_raw("+term", "+term", "+term", "+term")
def generate_full_axis(matrix, axis, *args, **kwargs):
    return generate_terms_array(matrix, axis, None, **kwargs)


@problog_export_raw("+term", "+term", "+term")
def generate_row(matrix, index, *args, **kwargs):
    return generate_term_array(matrix, 0, index, "row", **kwargs)


@problog_export_raw("+term", "+term")
def generate_rows(matrix, *args, **kwargs):
    return generate_terms_array(matrix, 0, "row", **kwargs)


@problog_export("+term", "+term", "+term", "+str")
def insert_str_cell(matrix, indices, value):
    list_indices = term2list(indices)
    return insert_value(matrix, list_indices, unquote(value))


@problog_export("+term", "+term", "+term", "+term")
def insert_cell(matrix, indices, value):
    list_indices = term2list(indices)
    return insert_value(matrix, list_indices, value.value)


@problog_export("+term", "+term", "+term")
def insert_column(matrix, index, column):
    return insert_array(matrix, index, column, axis=1)


@problog_export("+term", "+term", "+term")
def insert_row(matrix, index, row):
    return insert_array(matrix, index, row, axis=0)


#######################
#                     #
#      Functions      #
#                     #
#######################


def force_load(matrix, **kwargs):
    engine = kwargs["engine"]
    database = kwargs["database"]
    # TODO: test if it is still needed
    engine.query(database, Term("init_matrix", matrix), subcall=True)


def generate_term_array(matrix, axis, index, term_name, **kwargs):
    force_load(matrix, **kwargs)
    m = matrix.functor
    if type(index) != int:
        term = m.get_axis(axis, index.value, term_name=term_name)
        if term:
            return [(matrix, index, term)]
    return []


def generate_terms_array(matrix, axis, term_name, **kwargs):
    force_load(matrix, **kwargs)
    m = matrix.functor
    terms = m.get_full_axis(axis, term_name=term_name)
    if terms:
        return [(matrix, term) for term in terms]
    return []


def insert_array(matrix, index, array, axis):
    m = matrix.functor
    m.insert_parameters.append(
        (SpreadsheetMatrix.insert, index.value, term2list(array), axis)
    )
    return ()


def insert_value(matrix, indices, value):
    m = matrix.functor
    m.insert_parameters.append((SpreadsheetMatrix.insert_cell, indices, value))
    return ()


##########################
# #                    # #
# #    Spreadsheet     # #
# #      Format        # #
# #                    # #
##########################


class SpreadsheetFormat:
    def format_cells(self, matrix, cells):
        raise NotImplementedError

    def output_matrix(self, matrix):
        raise NotImplementedError


class StandardSpreadsheetFormat(SpreadsheetFormat):
    def format_cells(self, matrix, cells):
        m = np.empty(
            [matrix.get_row_number(), matrix.get_column_number()], dtype=Term
        )
        m[
            cells[:, 0].astype(int) - matrix.rows[0],
            cells[:, 1].astype(int) - matrix.columns[0],
        ] = cells[:, 2]
        matrix.set_matrix(m)

    def output_matrix(self, matrix):
        return matrix.matrix


class NurseSpreadsheetFormat(SpreadsheetFormat):
    def __init__(self, shifts):
        self.shifts = shifts

    def format_cells(self, matrix, cells):
        m = np.empty(
            [
                matrix.get_row_number(matrix),
                matrix.get_column_number(matrix) / self.shifts,
                self.shifts,
            ],
            dtype=Term,
        )
        m[
            cells[:, 0].astype(int) - matrix.rows[0],
            (
                (cells[:, 1].astype(int) - matrix.columns[0]) / self.shifts
            ).astype(int),
            (
                (cells[:, 1].astype(int) - matrix.columns[0]) % self.shifts
            ).astype(int),
        ] = cells[:, 2]
        matrix.set_matrix(m)

    def output_matrix(self, matrix):
        original = matrix.matrix
        shape = original.shape()
        return original.reshape(shape[0], shape[1] * shape[2])


##########################
# #                    # #
# #    Spreadsheet     # #
# #      Matrix        # #
# #                    # #
##########################


class SpreadsheetMatrix:
    def __init__(self, sheet_format):
        self.__format = sheet_format
        self.__matrix = None

        self.__loaded = False
        self.__load_cells_parameters = None
        self.__insert_parameters = []

        self.__wid = None
        self.__sid = None
        self.__rows = (None, None)
        self.__columns = (None, None)

    @property
    def matrix(self):
        self.update_matrix()
        return self.__matrix

    @property
    def rows(self):
        return self.__rows

    @property
    def columns(self):
        return self.__columns

    #######################
    #                     #
    #       Methods       #
    #                     #
    #######################

    def fill_axis(self, index, axis=0):
        shape = list(self.__matrix.shape)
        if index >= shape[axis]:
            shape[axis] = index - shape[axis] + 1
            filler = np.empty(shape, dtype=Term)
            self.__matrix = np.append(self.__matrix, filler, axis=axis)

    def fill_until(self, indices):
        for i in range(len(indices)):
            if indices[i] is not None:
                self.fill_axis(indices[i], axis=i)

    def insert(self, index, array, axis=0):
        if self.__loaded and self.__matrix.any():
            shape = self.__matrix.shape()
            array = np.array(array)
            for i in range(len(shape)):
                if i != axis:
                    array_i = i if i < axis else i + 1
                    shape[i] = max(shape[i], array.shape[array_i])
                else:
                    shape[i] = max(shape[i], index + 1)
            self.fill_until(shape)

            self.update(index, array, axis=axis)

    def insert_cell(self, indices, value):
        self.fill_until(indices)
        self.update_cell(indices, value)

    def load_table(self, database, wid, sid, row_slice, col_slice):
        if not self.__loaded:
            database = problog_export.database.resolve_filename(database)
            conn = sqlite3.connect(database)
            cursor = conn.cursor()

            self.__wid = wid
            self.__sid = sid

            self.__rows = self.get_slice(row_slice)
            self.__columns = self.get_slice(col_slice)

            with conn:
                sql = """
                SELECT Row, Column, Value FROM cell
                WHERE WorkbookID=? and SheetID=? and 
                 Row between ? and ? and Column between ? and ? and 
                 Owner =?
                """

                cursor.execute(
                    sql,
                    [
                        self.__wid,
                        self.__sid,
                        self.__rows[0],
                        self.__rows[1],
                        self.__columns[0],
                        self.__columns[1],
                        "excel",
                    ],
                )
                cells = np.matrix(cursor.fetchall())

                self.__format.format_cells(self, cells)

            self.__loaded = True

    def output_matrix(self):
        return self.__format.output_matrix(self)

    def update(self, index, array, axis=0):
        matrix_range = (
            [
                range(0, array.shape[i])
                for i in range(0, min(axis, len(array.shape)))
            ]
            + [index]
            + [range(0, array.shape[i]) for i in range(axis, len(array.shape))]
        )
        self.__matrix[matrix_range] = array

    def update_cell(self, indices, value):
        self.__matrix[indices] = value

    def update_matrix(self):
        if self.__load_cells_parameters:
            self.load_table(*self.__load_cells_parameters)
            self.__load_cells_parameters = None
        for insert in self.__insert_parameters:
            insert[0](self, *insert[1:])
        self.__insert_parameters = []

    #######################
    #                     #
    #       Getter        #
    #      & Setter       #
    #                     #
    #######################

    def get_cell(self, *args):
        self.update_matrix()
        if self.__loaded:
            shape = self.__matrix.shape
            if len(args) == len(shape) and all(
                args[i] < shape[i] for i in range(len(args))
            ):
                return Term(
                    "cell",
                    *[Constant(a) for a in args],
                    Constant(self.__matrix[args])
                )

    def get_axis(self, axis, index, term_name=None):
        self.update_matrix()
        if not term_name:
            term_name = "axis_" + str(axis)
        if (
            self.__loaded
            and self.__matrix.any()
            and axis < len(self.__matrix.shape)
            and index < self.__matrix.shape[axis]
        ):
            return self.get_array_term(
                term_name, index, self.__matrix.take(index, axis=axis)
            )

    def get_column_number(self):
        return self.__columns[1] - self.__columns[0] + 1

    def get_full_axis(self, axis, term_name=None):
        self.update_matrix()
        if self.__loaded:
            return [
                self.get_axis(axis, index, term_name=term_name)
                for index in range(self.__matrix.shape[axis])
            ]

    """
    Return the matrix without update
    """

    def get_matrix(self):
        return self.__matrix

    def get_row_number(self):
        return self.__rows[1] - self.__rows[0] + 1

    def set_load_cells_parameters(self, *args):
        self.__load_cells_parameters = args
        self.__loaded = False

    def set_matrix(self, matrix):
        self.__matrix = matrix

    #######################
    #                     #
    #        Class        #
    #       Methods       #
    #                     #
    #######################

    @staticmethod
    def get_array_term(name, index, array):
        return Term(
            name,
            Constant(index),
            *[
                Constant(array[j])
                if array[j] is not None
                else Var("X" + str(j))
                for j in range(len(array))
            ]
        )

    @staticmethod
    def get_slice(term):
        if unquote(term.functor) == ":":
            return term.args[0].value, term.args[1].value
        raise UserError("Term is not a slice: " + str(term))
