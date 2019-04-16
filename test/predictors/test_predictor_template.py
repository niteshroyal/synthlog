import unittest

from problog.program import PrologString
from problog import get_evaluatable
from problog.logic import term2str

import os
from pathlib import Path


class TestPredictor(object):
    @classmethod
    def setUpClass(cls):
        cls.predicate_name = ""
        cls.input_predicate = ""
        cls.output_predicate = ""

    def setUp(self):
        self.saved_wd = Path.cwd()
        os.chdir(Path(__file__).resolve().parent)
        self.magic_ice_cream_path = "../../data/magic_ice_cream.csv"
        self.predict_lib_path = "../../synthlog/predict.py"
        self.spreadsheet_lib_path = "../../synthlog/spreadsheet.py"

        self.module_import = """
        :- use_module('{}').
        :- use_module('{}').
        """.format(
            self.spreadsheet_lib_path, self.predict_lib_path
        )
        self.load_csv = """        
        magic_cells:X :- load_csv('{}', X).
        magic_tables:X :- detect_tables(magic_cells, X).
        """.format(
            self.magic_ice_cream_path
        )
        if self.predicate_name:
            self.input_predicate = self.predicate_name
            self.output_predicate = self.predicate_name

        if not self.input_predicate or not self.output_predicate:
            raise AttributeError(
                "Empty predicate_name, can not test non defined scikit-learn predictor."
            )

    def test_predictor_predicate(self):
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                               [column('T1', 2), column('T1', 5)],
                               [column('T1', 3)],
                               X).
        query(magic_models:predictor(_)).
        """.format(
                self.input_predicate
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), 1)

    def test_implementation_predicate(self):
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                               [column('T1', 2), column('T1', 5)],
                               [column('T1', 3)],
                               X).
        query(magic_models:{}(_)).
        """.format(
                self.input_predicate, self.output_predicate
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), 1)

    def test_predicates_objects(self):
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                               [column('T1', 2), column('T1', 5)],
                               [column('T1', 3)],
                               X).
                same_objects :- magic_models:predictor(T), magic_models:{}(T).
        query(same_objects).
        """.format(
                self.input_predicate, self.output_predicate
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), 1)
        for term, proba in result.items():
            self.assertEqual(proba, 1)

    def test_source_predicates(self):
        expected_columns = ["column('T1',5)", "column('T1',2)"]
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                                       [{}],
                                       [column('T1', 3)],
                                       X).

                query(magic_models:source(_, _)).
                """.format(
                self.input_predicate, ",".join(expected_columns)
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), len(expected_columns))

        for term, proba in result.items():
            self.assertEqual(len(term.args), 2)
            self.assertIn(term2str(term.args[1].args[1]), expected_columns)
            expected_columns.remove(term2str(term.args[1].args[1]))

    def test_source_predicate_objects(self):
        expected_columns = ["column('T1',5)", "column('T1',3)"]
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                               [{}],
                               [column('T1', 2)],
                               X).
                same_objects(C) :- magic_models:source(T, C), magic_models:{}(T).
        query(same_objects(C)).
        """.format(
                self.input_predicate, ",".join(expected_columns), self.output_predicate
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), len(expected_columns))
        for term, proba in result.items():
            self.assertEqual(proba, 1)

    def test_target_predicates(self):
        expected_columns = ["column('T1',3)", "column('T1',2)"]
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                                       [column('T1', 5)],
                                       [{}],
                                       X).

                query(magic_models:target(_, _)).
                """.format(
                self.input_predicate, ",".join(expected_columns)
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), len(expected_columns))

        for term, proba in result.items():
            self.assertEqual(len(term.args), 2)
            self.assertIn(term2str(term.args[1].args[1]), expected_columns)
            expected_columns.remove(term2str(term.args[1].args[1]))

    def test_target_predicate_objects(self):
        expected_columns = ["column('T1',5)", "column('T1',3)"]
        model = (
            self.module_import
            + self.load_csv
            + """magic_models:X :-{}(magic_tables,
                               [column('T1', 2)],
                               [{}],
                               X).
                same_objects(C) :- magic_models:target(T, C), magic_models:{}(T).
        query(same_objects(C)).
        """.format(
                self.input_predicate, ",".join(expected_columns), self.output_predicate
            )
        )
        result = get_evaluatable().create_from(PrologString(model)).evaluate()
        self.assertEqual(len(result), len(expected_columns))
        for term, proba in result.items():
            self.assertEqual(proba, 1)

    def tearDown(self):
        os.chdir(self.saved_wd)
