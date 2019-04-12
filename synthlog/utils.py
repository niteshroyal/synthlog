from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, unquote, term2str
from problog.errors import UserError, InvalidValue
from problog.parser import PrologParser
from problog.program import ExtendedPrologFactory


@problog_export("+str")
def parse_clause(term_string, engine=None, database=None, **kwargs):
    parser = PrologParser(ExtendedPrologFactory())
    res = parser.parseString(unquote(term_string))
    database.add_clause(res[0])
    return ()


@problog_export("+str", "-term")
def str2term(term_string):
    return Term(unquote(term_string))
