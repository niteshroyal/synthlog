import logging
from typing import List
from uuid import uuid4

import tacle
import numpy as np
from psyche.predictor_strategy import (
    FormulaPredictorStrategy,
    DefaultSingleChainingPredictorStrategy,
)
from psyche.psyche import PsycheContext
from psyche.spreadsheet import Spreadsheet, CellAddress

from state_manager import State, Prediction, Coordinate
from tacle_state import ConstraintConverter
from .task import BaseTask


class PsycheTask(BaseTask):
    predictor_strategies = [
        DefaultSingleChainingPredictorStrategy(),
        FormulaPredictorStrategy(),
    ]

    def psyche_context(self) -> PsycheContext:
        spreadsheet = Spreadsheet(None)
        spreadsheet.tables = [
            t["tacle_table"] for t in self.state.tables if "tacle_table" in t
        ]
        return PsycheContext(
            spreadsheet, ConstraintConverter.get_constraints(self.state)
        )

    def find_missing_cells(self) -> List[CellAddress]:
        spreadsheet = self.psyche_context().spreadsheet
        data = np.array(tacle.parse_csv(self.state.filepath), dtype=object)
        cell_addresses = []
        for table in spreadsheet.tables:
            tacle_range = table.range
            table_data = tacle_range.get_data(data)
            table_indices = np.argwhere(table_data == "")  # (y, x)
            for i in range(table_indices.shape[0]):
                cell_addresses.append(
                    spreadsheet.address(
                        table, table_indices[i, 0].item(), table_indices[i, 1].item()
                    )
                )
        return cell_addresses

    def is_available(self):
        return len(self.find_missing_cells()) > 0

    def do(self) -> State:
        # logging.basicConfig(level=logging.DEBUG)
        spreadsheet = Spreadsheet(None)
        spreadsheet.tables = [
            t["tacle_table"] for t in self.state.tables if "tacle_table" in t
        ]
        context = PsycheContext(
            spreadsheet,
            ConstraintConverter.get_constraints(self.state),
            predictor_strategies=PsycheTask.predictor_strategies,
        )
        missing_cells = self.find_missing_cells()
        best_predictions = context.get_best_predictions(missing_cells)

        predictions = []
        for confidence, values in best_predictions:
            if values is not None:
                provenance = ("psyche", str(uuid4()))
                for cell, value in values.items():
                    predictions.append(
                        Prediction(
                            Coordinate(cell.abs_x, cell.abs_y),
                            value,
                            confidence,
                            provenance,
                        )
                    )
        return self.state.add_objects(predictions)

    def description(self):
        return "Autocomplete"
