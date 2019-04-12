from problog.program import PrologFile, PrologString
from problog.core import ProbLog
from problog import get_evaluatable

# from probfoil.data import DataFile
from problog.extern import problog_export, problog_export_nondet, problog_export_raw
from problog.logic import Term, term2list, Constant, unquote, term2str, Object
import os
from copy import copy

from synthlog.probfoil.probfoil import ProbFOIL2
from synthlog.probfoil.data import DataFile
from synthlog.probfoil.score import accuracy, precision, recall


def evaluate_probfoil_rules(hypothesis):

    if len(hypothesis) == 0:
        print("Length of hypothesis is 0 while calling evaluate_probfoil_rules")
        return []

    rule = hypothesis
    rule_list = []
    count = 0
    if len(hypothesis.to_clauses(hypothesis.target.functor)) > 1:
        while rule.previous is not None:
            rule_list.append(copy(rule))
            rule_list[count].previous = None
            rule = rule.previous
            count += 1

    elif len(hypothesis.to_clauses(rule.target.functor)) == 1:
        rule_list = [hypothesis]

    result = []
    for i, rule in enumerate(rule_list):
        result.append(
            Term(
                "accuracy_rule",
                Constant(rule.target.functor + "_" + str(i)),
                Constant(accuracy(rule)),
            )
        )
        result.append(
            Term(
                "precision_rule",
                Constant(rule.target.functor + "_" + str(i)),
                Constant(precision(rule)),
            )
        )
        result.append(
            Term(
                "recall_rule",
                Constant(rule.target.functor + "_" + str(i)),
                Constant(recall(rule)),
            )
        )

    return result


def rules2scope(hypothesis):
    result = [Term("predictor", Constant(str(hypothesis) + "."))]
    rules = hypothesis.to_clauses(hypothesis.target.functor)

    # First rule is failing rule: don't print it if there are other rules.

    if len(rules) > 1:
        rule = hypothesis
        count = 0
        for rule_str in rules[1:]:
            result.append(
                Term(
                    "blackbox_rule",
                    Constant(rule.target.functor + "_" + str(count)),
                    Object(rule),
                )
            )
            result.append(
                Term(
                    "whitebox_rule",
                    Constant(rule.target.functor + "_" + str(count)),
                    *rule.get_literals()
                )
            )
            rule = rule.previous
            count += 1

    else:
        result.append(
            Term(
                "blackbox_rule",
                Constant(hypothesis.target.functor + "_0"),
                Object(hypothesis),
            )
        )
        result.append(
            Term(
                "whitebox_rule",
                Constant(hypothesis.target.functor + "_0"),
                *hypothesis.get_literals()
            )
        )

    return result


def create_temp_probfoil_file(
    base, mode, target, background_facts, pos_examples, neg_examples
):
    outfile = os.path.join(os.path.dirname(__file__), "../data/probfoil_temp.pl")
    count = 1
    while os.path.isfile(outfile):
        outfile = os.path.join(
            os.path.dirname(__file__), "../data/probfoil_temp_" + str(count) + ".pl"
        )
        count += 1

    outf = open(outfile, "w+")

    outf.write("% Typing of Predicates\n")
    outf.write("base(" + target + "(row_id)).\n")
    for fact in base:
        outf.write(fact + "\n")

    outf.write("\n%Declarative Bias\n")
    for fact in mode:
        outf.write(fact + "\n")

    outf.write("\n% Target Predicate\n")
    outf.write("learn(" + target + "/1).\n\n")

    outf.write("\n% Background Facts\n")
    for fact in background_facts:
        outf.write(fact + "\n")

    outf.write("\n% Positive Target Examples\n")
    for fact in pos_examples:
        outf.write(fact + "\n")

    outf.write("\n% Negative Target Examples\n")
    for fact in neg_examples:
        outf.write("0::" + target + "(" + "(".join(fact.split("(")[1:]) + "\n")

    outf.close()

    return outfile


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

    # print(DataFile(PrologFile(inputFile))._database._ClauseDB__nodes)
    hypothesis = ProbFOIL2(DataFile(PrologFile(inputFile)), beam_size=5, l=3).learn()
    os.remove(inputFile)

    result = rules2scope(hypothesis) + evaluate_probfoil_rules(hypothesis)
    return result


@problog_export_nondet("+term", "+term", "-term")
def probfoil_loop(scope, target_predicate, **kwargs):
    t = unquote(term2str(target_predicate))
    len_target = len(t)

    engine = kwargs["engine"]
    database = kwargs["database"]
    input_facts = engine.query(database, Term("':'", scope, None), subcall=True)

    num_facts = len(input_facts)
    background_facts = []
    target_facts = {}
    base_list = []
    base_facts = []
    mode_list = []
    mode_facts = []

    for i in range(0, num_facts):
        fact = input_facts[i][1]
        args = ["'" + term2str(val) + "'" for val in fact.args]

        if (
            len(fact.functor) > len_target + 1
            and fact.functor[: len_target + 1] == t + "_"
        ):
            target_constant = fact.functor[len_target + 1 :]
            if target_constant not in target_facts:
                target_facts[target_constant] = []
            target_facts[target_constant].append(
                fact.functor + "(" + ",".join(args) + ")."
            )
        elif fact.functor == t:
            continue
        else:
            background_facts.append(fact.functor + "(" + ",".join(args) + ").")

            if fact.functor not in base_list:
                base_list.append(fact.functor)

                if fact.functor == t or len(args) == 1:
                    base_facts.append("base(" + fact.functor + "(row_id)).")
                elif len(args) == 2:
                    base_facts.append(
                        "base("
                        + fact.functor
                        + "(row_id, "
                        + fact.functor
                        + "_constant))."
                    )

            if fact.functor not in mode_list:
                mode_list.append(fact.functor)

                if len(args) == 1 and fact.functor != t:
                    mode_facts.append("mode(" + fact.functor + "(+)).")
                elif len(args) == 2 and fact.functor != t:
                    mode_facts.append("mode(" + fact.functor + "(+, +)).")
                    mode_facts.append("mode(" + fact.functor + "(-, +)).")
                    mode_facts.append("mode(" + fact.functor + "(+, -)).")

    result = []

    count = 0
    for target_constant in target_facts.keys():
        pos_examples = target_facts[target_constant]
        neg_examples = []
        for key, value in target_facts.items():
            if key != target_constant:
                neg_examples += value

        file = create_temp_probfoil_file(
            base_facts,
            mode_facts,
            t + "_" + target_constant,
            background_facts,
            pos_examples,
            neg_examples,
        )

        hypothesis = ProbFOIL2(DataFile(PrologFile(file)), beam_size=10, l=4).learn()
        os.remove(file)

        num_rules = len(hypothesis.to_clauses(hypothesis.target.functor)) - 1
        if num_rules < 1:
            num_rules = 1

        result += rules2scope(hypothesis) + evaluate_probfoil_rules(hypothesis)
        count += num_rules

    return result
