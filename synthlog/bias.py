from __future__ import print_function

from problog.extern import problog_export, problog_export_nondet, problog_export_raw

from problog.logic import Term, term2list, Constant, Var, unquote
from problog.errors import UserError, InvalidValue

from operator import itemgetter
import logging

logger = logging.getLogger("problog")


@problog_export("+term", "+term", "-term")
def foreign_keys(a, b):
    args = list(a.args)
    bargs = list(b.args)

    lgg_args = [None] * len(args)
    lgg_bargs = [None] * len(bargs)

    x = 0
    for i in range(len(args)):
        match = False
        for j in range(len(bargs)):
            if not isinstance(bargs[j], Var):
                if args[i] == bargs[j]:
                    v = Var("X" + str(x))
                    x += 1
                    lgg_args[i] = v
                    lgg_bargs[j] = v
                    match = True
                    break
        if not match:
            lgg_args[i] = Var("X" + str(x))
            x += 1

    for i in range(len(lgg_bargs)):
        if lgg_bargs[i] is None:
            lgg_bargs[i] = Var("X" + str(x))
            x += 1

    return Term("join", a.with_args(*lgg_args), b.with_args(*lgg_bargs))


@problog_export("+term", "+term", "+term", "-term")
def lgg(a, b, template):
    return rec_lgg(a, b, template)


def rec_lgg(a, b, template):
    if isinstance(a, Constant):
        if isinstance(b, Constant) and a.value == b.value:
            return a
        return template
    elif isinstance(a, Var):
        return template
    elif isinstance(a, Term):
        if (
            not isinstance(b, Term)
            or a.functor != b.functor
            or len(a.args) != len(b.args)
        ):
            return template
        args = []
        for x in range(len(a.args)):
            res = rec_lgg(a.args[x], b.args[x], template.args[x])
            args.append(res)
        return a.with_args(*args)
