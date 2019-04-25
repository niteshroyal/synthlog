import unittest

import test_predictor_template


class TestDecisionTree(test_predictor_template.TestPredictor, unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.predicate_name = "decision_tree"


if __name__ == "__main__":
    unittest.main()
