from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, unquote
from problog.errors import UserError, InvalidValue

import sqlite3
import os
import logging
import re

import openpyxl as xls

logger = logging.getLogger("problog")

"""
    The `extended_db` module for Synthlog
    =====================================
    
    Use it to import predicates from an external database or save predicates into an external database.
    This file is mainly based on the standard `db.py` from Problog. 

"""

#######################
#                     #
#       ProbLog       #
#       Exports       #
#                     #
#######################


@problog_export("+str", "+str")
@problog_export("+str", "+str", "+str")
def excel_into_sqlite(workbook, database, workbook_name=None):
    """
    Problog predicate to import a workbook (xlsx file) into a database

    :param workbook: workbook path
    :type workbook: str

    :param database: database path
    :type database: str

    :param workbook_name: optional, a specific workbook name
        default: the filename without extension
    :type workbook_name: str

    :return: An empty tuple (No Problog unification)
    """

    # Resolve the filename with respect to the main Prolog file location.
    workbook = problog_export.database.resolve_filename(workbook)
    if not os.path.exists(workbook):
        raise UserError("Can't find spreadsheet '%s'" % workbook)

    # Load the Excel workbook.
    wb = xls.load_workbook(workbook)
    if not workbook_name:
        workbook_name = workbook.split("/")[-1].split(".")[0]

    filename = problog_export.database.resolve_filename(database)
    if filename:
        database = filename
    else:
        database = unquote(database)
    conn = sqlite3.connect(database)
    cursor = conn.cursor()

    with conn:
        cursor.execute(
            """CREATE TABLE IF NOT EXISTS 
            cell(WorkbookID INT, SheetID INT, Row INT, Column INT, Value TEXT, Owner TEXT, 
            UNIQUE(WorkbookID, SheetID, Row, Column) ON CONFLICT REPLACE);"""
        )
        cursor.execute("CREATE TABLE IF NOT EXISTS workbook(Name TEXT);")
        cursor.execute("CREATE TABLE IF NOT EXISTS sheet(WorkbookID INT, Name TEXT);")
        workbook_id = get_workbook(cursor, workbook_name)

        # TODO make a unique insert
        for sheetname in wb.sheetnames:
            sheet_id = get_sheet(cursor, workbook_id, sheetname)
            # Load the data from the Excel sheet and add non-empty cells to the database.
            for row in wb[sheetname].iter_rows():
                for cell in row:
                    if cell.value is not None:
                        insert_cell(
                            cursor,
                            workbook_id,
                            sheet_id,
                            cell.row,
                            cell.col_idx,
                            cell.value,
                            "excel",
                        )

    return ()


@problog_export("+str", "+term", "+term", "+term", "+term", "+term")
def save_column(filename, column, workbook_id, sheet_id, first_row, col):
    """
    Store a Term list in a database

    :param filename: The database filename
    :type filename: str

    :param column: The Term list
    :type column: Problog list

    :param workbook_id: The ID of the workbook for which the Term list represents a column
    :type workbook_id: Constant (containing an integer)

    :param sheet_id: The ID of the spreadsheet for which the Term list represents of column
    :type sheet_id:  Constant (containing an integer)

    :param first_row: The ID of the row for the first element of the list is the cell value
    :type first_row:  Constant (containing an integer)

    :param col: The ID of the column represented by the list
    :type col:  Constant (containing an integer)

    :return: An empty tuple (No Problog unification)
    """
    conn, cursor = connect(filename)

    m = term2list(column)
    wid = workbook_id.value
    sid = sheet_id.value

    with conn:
        for row in range(len(m)):
            rid = row + first_row.value
            value = pl2db(m[row])
            insert_cell(cursor, wid, sid, rid, col.value, value, "problog")
    cursor.close()

    return ()


@problog_export("+str", "+term", "+term", "+term", "+term", "+term")
def save_matrix(filename, matrix, workbook_id, sheet_id, first_row, first_col):
    """
    Store a Term matrix in a database

    :param filename: The database filename
    :type filename: str

    :param matrix: the Term matrix
    :type matrix: Problog list

    :param workbook_id: The ID of the workbook for which the Term list represents a column
    :type workbook_id: Constant (containing an integer)

    :param sheet_id: The ID of the spreadsheet for which the Term list represents of column
    :type sheet_id:  Constant (containing an integer)

    :param first_row: The ID of the row for the first element of the list is the cell value
    :type first_row:  Constant (containing an integer)

    :param first_col: The ID of the column for the first element of the list is the cell value
    :type first_col: Constaint (containing an integer)

    :return: An empty tuple (No Problog unification)
    """
    conn, cursor = connect(filename)

    m = matrix.functor.matrix
    wid = workbook_id.value
    sid = sheet_id.value

    with conn:
        for row in range(len(m)):
            rid = row + first_row.value
            for col in range(len(m[row])):
                value = m[row][col]
                insert_cell(
                    cursor, wid, sid, rid, col + first_col.value, value, "problog"
                )
    cursor.close()

    return ()


@problog_export("+str")
@problog_export("+str", "+str")
def sqlite_load(filename, pattern=None):
    """
    Load predicates from a database

    :param filename: The database filename
    :type filename: str

    :param pattern: optional, a pattern of the predicated to load
        TODO: give an example
    :type pattern: str

    :return: An empty tuple (No Problog unification)
    """
    if pattern:
        values = get_pattern_values(pattern)
    fin = problog_export.database.resolve_filename(filename)
    if fin:
        filename = fin
    conn = sqlite3.connect(filename)
    cursor = conn.cursor()

    sql = "SELECT name FROM sqlite_master WHERE type='table'"
    if pattern:
        sql += " AND name='" + values[0] + "pass'"
    sql += ";"
    cursor.execute(sql)
    tables = [x[0] for x in cursor.fetchall()]
    cursor.close()

    for table in tables:
        columns = get_colnames(conn, table)
        types = ["+term"] * len(columns)
        where = values[1] if pattern else None
        problog_export_raw(*types)(
            QueryFunc(conn, table, columns, where=where), funcname=table, modname=None
        )

    return ()


@problog_export("+str", "+str")
def sqlite_load_table(filename, table):
    """
    Load specific predicates from a database

    :param filename: The database filename
    :type filename: str

    :param table: The table name in the database corresponding to the predicates
    :type table: str

    :return: An empty tuple (No Problog unification)
    """
    conn, cursor = connect(filename)
    columns = get_colnames(conn, table)
    types = ["+term"] * len(columns)
    problog_export_raw(*types)(
        QueryFunc(conn, table, columns), funcname=table, modname=None
    )

    return ()


#######################
#                     #
#      Functions      #
#                     #
#######################


def connect(filename):
    filename = problog_export.database.resolve_filename(filename)
    if not os.path.exists(filename):
        raise UserError("Can't find database '%s'" % filename)
    conn = sqlite3.connect(filename)
    return conn, conn.cursor()


def create(filename, table, types, *args):
    conn, cursor = connect(filename)
    with conn:
        desc = []
        for i in range(len(args)):
            arg = args[i]
            desc.append("X" + str(i))
            if type(arg) == int:
                desc[-1] += " " + types[i]

        cursor.execute(
            "CREATE TABLE IF NOT EXISTS " + table + "(" + ",".join(desc) + ");"
        )


def db2pl(dbvalue):
    dbvalue = represents_constant(dbvalue)
    if type(dbvalue) == str:
        return Term("'" + dbvalue + "'")
    else:
        return Constant(dbvalue)


def get_colnames(conn, tablename):
    cur = conn.cursor()
    cur.execute("SELECT * FROM %s WHERE 0;" % tablename)
    res = [x[0] for x in cur.description]
    cur.close()
    return res


def get_pattern_values(pattern):
    rx = re.compile(r"^'([a-z]+)\((.+)\)'$")
    m = rx.match(pattern)
    if not m:
        raise UserError("String does not follow the fact format for matching table")
    table_name = m.group(1)
    values = m.group(2).split(",")
    return table_name, values


def get_sheet(cursor, workbook_id, name):
    sql = (
        "SELECT rowid FROM sheet WHERE WorkbookID="
        + str(workbook_id)
        + " AND name='"
        + str(name)
        + "';"
    )
    cursor.execute(sql)
    results = [x[0] for x in cursor.fetchall()]
    if len(results) > 0:
        return results[0]

    cursor.execute(
        "INSERT INTO sheet(workbookid, name) VALUES("
        + str(workbook_id)
        + ", '"
        + str(name)
        + "');"
    )
    cursor.execute(sql)
    results = [x[0] for x in cursor.fetchall()]
    if len(results) > 0:
        return results[0]

    raise UserError("Can't insert new worksheet in the database '%s'" % database)


def get_workbook(cursor, name):
    sql = "SELECT rowid FROM workbook WHERE name='" + str(name) + "';"
    cursor.execute(sql)
    results = [x[0] for x in cursor.fetchall()]
    if len(results) > 0:
        return results[0]
    cursor.execute("INSERT INTO workbook(name) VALUES('" + str(name) + "');")
    cursor.execute(sql)
    results = [x[0] for x in cursor.fetchall()]
    if len(results) > 0:
        return results[0]
    raise UserError("Can't insert new workbook in the database '%s'" % database)


def insert(filename, table, *args):
    conn, cursor = connect(filename)
    with conn:
        cursor.execute(
            "INSERT INTO " + table + " VALUES(" + ",".join(["?"] * len(args)) + ")",
            args,
        )


def insert_cell(cursor, workbook_id, sheet_id, row, col, value, owner):
    sql = "INSERT INTO cell(WorkbookID, SheetID, Row, Column, Value, Owner) "
    sql += "VALUES(" + str(workbook_id) + ", " + str(sheet_id) + ", " + str(row) + ", "
    sql += str(col) + ", '" + str(value) + "', '" + str(owner) + "')"
    cursor.execute(sql)


def pl2db(t):
    if isinstance(t, Constant):
        return t.value
    else:
        return str(t).strip("'")


def represents_constant(s):
    try:
        return int(s)
    except ValueError:
        try:
            return float(s)
        except ValueError:
            return s


def save(filename, table, types, *args):
    create(filename, table, types, *args)
    insert(filename, table, *args)


#######################
#                     #
#       Classes       #
#                     #
#######################


class QueryFunc(object):
    def __init__(self, db, tablename, columns, where=None):
        self.db = db
        self.tablename = tablename
        self.columns = columns
        self.where_ = where

    def __call__(self, *args, **kwargs):
        where, values = self.parse_where_args(args, extern_args=True)
        if self.where_:
            w, v = self.parse_where_args(self.where_)
            where += w
            values += v

        where = " AND ".join(where)
        if where:
            where = " WHERE " + where

        query = "SELECT %s FROM %s%s" % (", ".join(self.columns), self.tablename, where)
        cur = self.db.cursor()

        cur.execute(query, values)
        res = [tuple(map(db2pl, r)) for r in cur.fetchall()]
        cur.close()
        return res

    def parse_where_args(self, where_list, extern_args=False):
        where = []
        values = []

        for c, a in zip(self.columns, where_list):
            if a is not None and a is not "_":
                m = None
                if not extern_args:
                    slice_regexp = re.compile("^([0-9]*):([0-9]*)$")
                    m = slice_regexp.match(a)
                if m:
                    if m.group(1):
                        where.append("%s >= ?" % c)
                        values.append(pl2db(int(m.group(1))))
                    if m.group(2):
                        where.append("%s < ?" % c)
                        values.append(pl2db(int(m.group(2))))
                else:
                    where.append("%s = ?" % c)
                    values.append(pl2db(a))
            else:
                where.append("%s IS NOT NULL" % c)
                where.append("%s <> ?" % c)
                values.append(pl2db("NULL"))

        return where, values
