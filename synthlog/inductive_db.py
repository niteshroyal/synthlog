from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, Object, unquote
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


@problog_export("+str", "-term")
def load_inductive_db(filename):
    """
    Load predicates from a database

    :param filename: The database filename
    :type filename: str

    :return: The database Term
    :rtype: Problog Object
    """

    fin = problog_export.database.resolve_filename(filename)
    if fin:
        filename = fin

    idb = InductiveDBWrapper(filename)

    # Say to Problog engine that if scopes are queried, the inductive database has to be called first
    problog_export_raw(["+term", "+term"])(idb(), funcname="':'", modname=None)

    return Object(idb)


#######################
#                     #
#       Classes       #
#                     #
#######################


class InductiveDBWrapper:
    def __init__(self, filename):
        self.filename = filename

    def __call__(self, *args, **kwargs):
        return []


#######################
#                     #
#         Old         #
#        Stuff        #
#                     #
#######################


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
