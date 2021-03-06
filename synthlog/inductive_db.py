from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, Object, unquote
from problog.errors import UserError, InvalidValue

import sqlite3
import os
import logging
import re

import dill as pickle
import numpy as np

import openpyxl as xls
from copy import deepcopy

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


@problog_export("+str")
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
    problog_export_raw("+term", "+term")(idb, funcname="':'", modname=None)

    return ()


@problog_export("+str")
def load_prob_inductive_db(filename):
    """
    Load predicates from a database (with probabilities on terms)

    :param filename: The database filename
    :type filename: str

    :return: The database Term
    :rtype: Problog Object
    """
    fin = problog_export.database.resolve_filename(filename)
    if fin:
        filename = fin

    idb = InductiveDBWrapper(filename, export_proba=True)

    # Say to Problog engine that if scopes are queried, the inductive database has to be called first
    problog_export_raw("+term", "+term", "+term")(
        idb, funcname="'scope_in_database'", modname=None
    )

    return ()


@problog_export("+term", "+term", "+str")
def save_term(scope, term, inductive_database):

    pickled_term = pickle.dumps(term)
    pickled_scope = pickle.dumps(scope)
    InductiveDBWrapper(inductive_database).save_term(
        scope.functor,
        scope.arity,
        pickled_scope,
        term.functor,
        term.arity,
        pickled_term,
    )

    return ()


@problog_export("+term", "+term", "+term", "+str")
def save_term(scope, term, proba, inductive_database):

    if not proba.is_constant():
        raise InvalidValue("The probability needs to be a number")

    saved_term = deepcopy(term)
    if proba.value == 1:
        saved_term.probability = None
    else:
        saved_term.probability = proba
    print("{}:{}:\t{}".format(str(scope), str(term), str(proba.value)))

    pickled_term = pickle.dumps(saved_term)
    pickled_scope = pickle.dumps(scope)

    InductiveDBWrapper(inductive_database, True).save_term(
        scope.functor,
        scope.arity,
        pickled_scope,
        saved_term.functor,
        saved_term.arity,
        pickled_term,
    )

    return ()


#######################
#                     #
#       Classes       #
#                     #
#######################


class InductiveDBWrapper:
    def __init__(self, filename, export_proba=False):
        self.filename = unquote(filename)
        self.connection = sqlite3.connect(self.filename)
        self.cursor = self.connection.cursor()
        self.proba = export_proba
        self.__init_tables()
        self.__previous_insert = set()

    def __call__(self, *args, **kwargs):
        scope_args = ["%"] * 2
        term_args = ["%"] * 3

        if args:
            if args[0]:
                scope_args[0] = args[0].functor
                scope_args[1] = args[0].arity
            if args[1]:
                term_args[1] = args[1].functor
                term_args[2] = args[1].arity

        res = []

        with self.connection:
            scopes = self.__get_scopes(*scope_args)
            for scope_id, scope in scopes:
                term_args[0] = scope_id
                terms = self.__get_terms(*term_args)

                unpickled_scope = pickle.loads(scope)
                for term_id, term in terms:
                    if term_id not in self.__previous_insert:
                        unpickled_term = pickle.loads(term)

                        if not self.proba:
                            res.append((unpickled_scope, unpickled_term))
                        else:
                            proba = unpickled_term.probability
                            if proba is None:
                                proba = 1
                            unpickled_term.probability = None
                            if not isinstance(proba, Constant):
                                proba = Constant(proba)
                            res.append((unpickled_scope, unpickled_term, proba))

        return res

    def save_term(self, scope_name, scope_arity, scope, term_name, term_arity, term):
        scope_id = self.__insert_scope(scope_name, scope_arity, scope)
        self.__previous_insert.add(
            self.__insert_term(scope_id, term_name, term_arity, term)
        )

    # Internal methods #

    def __create_table(self, name, *args):
        sql_query = (
            "CREATE TABLE IF NOT EXISTS "
            + name
            + "("
            + ",".join([arg[0] + " " + arg[1] for arg in args])
            + ")"
        )
        self.cursor.execute(sql_query)

    def __get_terms(self, scope_id, name, arity):
        with self.connection:
            self.cursor.execute(
                """
               SELECT rowid, data 
               FROM term 
               WHERE scope LIKE ?
               AND name LIKE ?
               AND arity LIKE ?; 
               """,
                (scope_id, name, arity),
            )
            return self.cursor.fetchall()

    def __get_scopes(self, name, arity):
        with self.connection:
            self.cursor.execute(
                """
               SELECT rowid, data 
               FROM scope 
               WHERE name LIKE ?
               AND arity LIKE ?; 
               """,
                (name, arity),
            )
            return self.cursor.fetchall()

    def __init_tables(self):
        self.__create_table(
            "scope", ("name", "TEXT"), ("arity", "INT"), ("data", "BLOB")
        )

        self.__create_table(
            "term",
            ("scope", "INT"),
            ("name", "TEXT"),
            ("arity", "INT"),
            ("data", "BLOB"),
        )

    def __insert_scope(self, name, arity, scope):
        scopes = self.__get_scopes(name, arity)
        unpickled_scope = pickle.loads(scope)

        for scope_id, similar_scope in scopes:
            unpickled_similar = pickle.loads(similar_scope)
            if str(unpickled_scope) == str(unpickled_similar):
                return scope_id

        with self.connection:
            self.cursor.execute(
                """
                INSERT INTO scope(name, arity, data)
                VALUES (?, ?, ?);
                """,
                (name, arity, scope),
            )
        return self.cursor.lastrowid

    def __insert_term(self, scope_id, name, arity, term):
        terms = self.__get_terms(scope_id, name, arity)
        unpickled_term = pickle.loads(term)

        for term_id, similar_term in terms:
            unpickled_similar = pickle.loads(similar_term)
            if str(unpickled_term) == str(unpickled_similar):
                return term_id

        with self.connection:
            self.cursor.execute(
                """
                INSERT INTO term(scope, name, arity, data) 
                VALUES (?, ?, ?, ?);
                """,
                (scope_id, name, arity, term),
            )
        return self.cursor.lastrowid
