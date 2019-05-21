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
                Constant(rule.target.functor),
                Constant(i),
                Constant(accuracy(rule)),
            )
        )
        result.append(
            Term(
                "precision_rule",
                Constant(rule.target.functor),
                Constant(i),
                Constant(precision(rule)),
            )
        )
        result.append(
            Term(
                "recall_rule",
                Constant(rule.target.functor),
                Constant(i),
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
            if "true" in str(rule):
                text = str(rule).replace("true", "row(A)")
            else:
                text = str(rule) + ",row(A)"
            result.append(
                Term(
                    "blackbox_rule",
                    Constant(rule.target.functor),
                    Constant(count),
                    Term("'" + text + ".'"),
                )
            )
            result.append(
                Term(
                    "whitebox_rule",
                    Constant(rule.target.functor),
                    Constant(count),
                    *rule.get_literals()
                )
            )
            rule = rule.previous
            count += 1

    else:
        if "true" in str(hypothesis):
            text = str(hypothesis).replace("true", "row(A)")
        else:
            text = str(hypothesis) + ",row(A)"
        result.append(
            Term(
                "blackbox_rule",
                Constant(hypothesis.target.functor),
                Constant(0),
                Term("'" + text + ".'"),
            )
        )
        result.append(
            Term(
                "whitebox_rule",
                Constant(hypothesis.target.functor),
                Constant(0),
                *hypothesis.get_literals()
            )
        )

    return result


def create_probfoil_inputfile(
    base, mode, target, background_facts, pos_examples, neg_examples
):
    # Typing of Predicates
    probfoil_input = "base(" + target + "(row_id)).\n"
    for fact in base:
        probfoil_input += fact + "\n"

    # Declarative Bias
    for fact in mode:
        probfoil_input += fact + "\n"

    # Target Predicate
    probfoil_input += "learn(" + target + "/1).\n\n"

    # Background Facts
    for fact in background_facts:
        probfoil_input += fact + "\n"

    # Positive Target Examples
    for fact in pos_examples:
        probfoil_input += fact + "\n"

    # Negative Target Examples
    for fact in neg_examples:
        probfoil_input += "0::" + target + "(" + "(".join(fact.split("(")[1:]) + "\n"

    return probfoil_input


@problog_export_nondet("+term", "+term", "-term")
def probfoil(scope, target_predicate, **kwargs):
    t = unquote(term2str(target_predicate))
    len_target = len(t)

    engine = kwargs["engine"]
    database = kwargs["database"]
    input_facts = engine.query(database, Term("':'", scope, None), subcall=True)

    probfoil_input = "learn(" + t + "/1).\n"

    num_facts = len(input_facts)
    base_list = []
    # base_facts = []
    mode_list = []
    # mode_facts = []

    for i in range(0, num_facts):
        fact = input_facts[i][1]
        args = ["'" + term2str(val) + "'" for val in fact.args]

        # Ignore propositionalized facts of target predicate
        if (
            len(fact.functor) > len_target + 1
            and fact.functor[: len_target + 1] == t + "_"
        ):
            if fact.functor.endswith("yes"):
                probfoil_input += t + "(" + args[0] + ").\n"
            else:
                probfoil_input += "0::" + t + "(" + args[0] + ").\n"

        else:
            probfoil_input += fact.functor + "(" + ",".join(args) + ").\n"

        # Typing of Predicates
        if fact.functor not in base_list:
            base_list.append(fact.functor)
            if fact.functor.startswith(t + "_"):
                probfoil_input += "base(" + t + "(row_id)).\n"
            elif len(args) == 1:
                probfoil_input += "base(" + fact.functor + "(row_id)).\n"
            elif len(args) == 2:
                probfoil_input += (
                    "base("
                    + fact.functor
                    + "(row_id, "
                    + fact.functor
                    + "_constant)).\n"
                )

        # Declarative Bias
        if fact.functor not in mode_list:
            mode_list.append(fact.functor)
            if len(args) == 1 and not fact.functor.startswith(t + "_"):
                probfoil_input += "mode(" + fact.functor + "(+)).\n"
            elif len(args) == 2 and not fact.functor.startswith(t + "_"):
                probfoil_input += "mode(" + fact.functor + "(+, -)).\n"
                probfoil_input += "mode(" + fact.functor + "(-, +)).\n"
                probfoil_input += "mode(" + fact.functor + "(+, +)).\n"

    # # Typing of Predicates
    # for fact in base_facts:
    #     probfoil_input += fact + "\n"
    #
    # # Declarative Bias
    # for fact in mode_facts:
    #     probfoil_input += fact + "\n"

    # Run ProbFOIL+
    hypothesis = ProbFOIL2(
        DataFile(PrologString(probfoil_input)), beam_size=10, l=4
    ).learn()

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

            # Typing of Predicates
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

            # Declarative Bias
            if fact.functor not in mode_list:
                mode_list.append(fact.functor)
                if len(args) == 1 and fact.functor != t:
                    mode_facts.append("mode(" + fact.functor + "(+)).")
                elif len(args) == 2 and fact.functor != t:
                    mode_facts.append("mode(" + fact.functor + "(+, +)).")
                    mode_facts.append("mode(" + fact.functor + "(-, +)).")
                    mode_facts.append("mode(" + fact.functor + "(+, -)).")

    result = []

    for target_constant in target_facts.keys():
        pos_examples = target_facts[target_constant]
        neg_examples = []
        for key, value in target_facts.items():
            if key != target_constant:
                neg_examples += value

        # Create ProbFOIL Input
        probfoil_input = create_probfoil_inputfile(
            base_facts,
            mode_facts,
            t + "_" + target_constant,
            background_facts,
            pos_examples,
            neg_examples,
        )

        # Run ProbFOIL+
        hypothesis = ProbFOIL2(
            DataFile(PrologString(probfoil_input)), beam_size=10, l=4
        ).learn()

        result += rules2scope(hypothesis) + evaluate_probfoil_rules(hypothesis)

    return result
