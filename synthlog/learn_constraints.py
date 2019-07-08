from __future__ import print_function

from problog.extern import problog_export_nondet

from itertools import product

from problog.util import init_logger
from problog.logic import Term, Constant, Clause, Var
from synthlog.keywords import init_cell_pred

from problog.parser import PrologParser
from problog.program import ExtendedPrologFactory

from synthlog.tasks.predictors import *

logger = init_logger()


@problog_export_nondet("-term")
def learn_dummy_constraint(**kwargs):
    """

    """
    term_string2 = "0.3::wrong:- final_pred(8, 1, V), V < 600."
    term_string = "wrong :- final_pred(8,1, V), V < 310."

    parser = PrologParser(ExtendedPrologFactory())
    res = parser.parseString(unquote(term_string))

    res.extend(parser.parseString(unquote(term_string2)))
    return res
