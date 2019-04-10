from __future__ import print_function

from problog import get_evaluatable
from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, unquote
from problog.errors import UserError, InvalidValue


@problog_export("+str", "-term")
def str2term(term_string):
    return Term(unquote(term_string))
