import unittest

import test_predictor_template


class TestRandomForest(test_predictor_template.TestPredictor, unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.predicate_name = "random_forest"


if __name__ == "__main__":
    unittest.main()
