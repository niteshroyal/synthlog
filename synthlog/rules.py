from problog.program import PrologFile, PrologString
from problog.core import ProbLog
from problog import get_evaluatable

# from probfoil.data import DataFile
from problog.extern import problog_export, problog_export_nondet, problog_export_raw
from problog.logic import Term, term2list, Constant, unquote, term2str, Object
import os

from synthlog.probfoil.probfoil import ProbFOIL2
from synthlog.probfoil.data import DataFile


@problog_export_nondet("+term", "+term", "-term")
def probfoil(scope, target_predicate, **kwargs):
    t = unquote(term2str(target_predicate))
    len_target = len(t)

    engine = kwargs["engine"]
    database = kwargs["database"]
    input_facts = engine.query(database, Term("':'", scope, None), subcall=True)

    inputFile = os.path.join(os.path.dirname(__file__), "../data/probfoil_temp.pl")
    inputf = open(inputFile, "w+")

    inputf.write("% Target Predicate\n")
    inputf.write("learn(" + t + "/1).\n\n")

    num_facts = len(input_facts)
    base_list = []
    base_facts = []
    mode_list = []
    mode_facts = []

    inputf.write("\n% Background Facts\n")
    for i in range(0, num_facts):
        fact = input_facts[i][1]
        args = ["'" + term2str(val) + "'" for val in fact.args]

        if (
            len(fact.functor) > len_target + 1
            and fact.functor[: len_target + 1] == t + "_"
        ):
            continue

        if fact.functor == t:
            if unquote(str(args[1])) == "yes":
                inputf.write(fact.functor + "(" + args[0] + ").\n")
            else:
                inputf.write("0::" + fact.functor + "(" + args[0] + ").\n")

        else:
            inputf.write(fact.functor + "(" + ",".join(args) + ").\n")

        if fact.functor not in base_list:
            base_list.append(fact.functor)

            if fact.functor == t or len(args) == 1:
                base_facts.append("base(" + fact.functor + "(row_id)).")
            elif len(args) == 2:
                base_facts.append(
                    "base(" + fact.functor + "(row_id, " + fact.functor + "_constant))."
                )

        if fact.functor not in mode_list:
            mode_list.append(fact.functor)

            if len(args) == 1 and fact.functor != t:
                mode_facts.append("mode(" + fact.functor + "(+)).")
            elif len(args) == 2 and fact.functor != t:
                mode_facts.append("mode(" + fact.functor + "(+, +)).")
                mode_facts.append("mode(" + fact.functor + "(-, +)).")
                mode_facts.append("mode(" + fact.functor + "(+, -)).")

    inputf.write("\n% Typing of Predicates\n")
    for fact in base_facts:
        inputf.write(fact + "\n")

    inputf.write("\n%Declarative Bias\n")
    for fact in mode_facts:
        inputf.write(fact + "\n")

    inputf.close()

    data = DataFile(PrologFile(inputFile))
    # print(data._database._ClauseDB__nodes)
    hypothesis = ProbFOIL2(data, beam_size=5).learn()
    result = [Term("predictor", Object(hypothesis))]

    rules = hypothesis.to_clauses(hypothesis.target.functor)

    # First rule is failing rule: don't print it if there are other rules.

    if len(rules) > 1:
        rule = hypothesis
        count = 0
        for rule_str in rules[1:]:
            result.append(Term("blackbox_rule", Constant(count), Object(rule)))
            result.append(Term("whitebox_rule", Constant(count), *rule.get_literals()))
            rule = rule.previous
            count += 1

    else:
        result.append(Term("blackbox_rule", Constant(0), Object(hypothesis)))
        result.append(
            Term("whitebox_rule", Constant(count), *hypothesis.get_literals())
        )

    return result
