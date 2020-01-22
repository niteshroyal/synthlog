import logging
from typing import List
from uuid import uuid4

import tacle
import numpy as np
from psyche.predictor_strategy import (
    FormulaPredictorStrategy,
    DefaultSingleChainingPredictorStrategy,
)
from psyche.psyche import PsycheContext, PredictorPruningStrategy
from psyche.spreadsheet import Spreadsheet, CellAddress
from tacle import get_type_data
from tacle.convert import orientation_compatible
from tacle.indexing import Table, Range

from state_manager import State, Prediction, Coordinate, Table as StateTable
from tacle_state import ConstraintConverter
from .task import BaseTask


class PsycheTask(BaseTask):
    predictor_strategies = [
        DefaultSingleChainingPredictorStrategy(),
        FormulaPredictorStrategy(),
    ]

    def __init__(self, state: State, context: dict):
        super().__init__(state, context)
        self.tables = self.get_tables()

    def get_tables(self):
        state_tables = self.state.tables
        data = tacle.parse_csv(self.state.filepath)
        data_array = np.array(data, dtype=object)
        type_data = get_type_data(data_array)

        tables = []
        for state_table in state_tables:
            if "tacle_range" in state_table:
                table_range = state_table["tacle_range"]  # type: Range
            else:
                continue

            table_data = table_range.get_data(data_array)
            supported_orientation = [
                o
                for o in tacle.indexing.Orientation.all()
                if orientation_compatible(type_data, table_range, o)
            ]
            if len(supported_orientation) > 0:
                tables.append(
                    tacle.indexing.Table(
                        table_data,
                        table_range.get_data(type_data),
                        table_range,
                        state_table.name,
                        supported_orientation,
                    )
                )
        return tables

    def psyche_context(self) -> PsycheContext:
        spreadsheet = Spreadsheet(None)
        spreadsheet.tables = self.tables
        return PsycheContext(
            spreadsheet, ConstraintConverter.get_constraints(self.state)
        )

    def find_missing_cells(self) -> List[CellAddress]:
        spreadsheet = self.psyche_context().spreadsheet
        cell_addresses = []
        for table in self.tables:
            table_indices = np.argwhere(table.data == "")  # (y, x)
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
        spreadsheet.tables = self.tables
        context = PsycheContext(
            spreadsheet,
            ConstraintConverter.get_constraints(self.state),
            predictor_strategies=PsycheTask.predictor_strategies,
            predictor_pruning=PredictorPruningStrategy.prune_empty_strategy(),
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
