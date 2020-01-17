import tacle
import numpy as np
import tacle.indexing
from tacle import get_type_data
from tacle.convert import orientation_compatible

from tacle_state import tacle_range_to_state_range
from .task import BaseTask
from state_manager import Table as StateTable, State


class DetectTablesTask(BaseTask):
    def do(self):
        table_ranges = tacle.ranges_from_csv(self.state.filepath)
        new_state = self.state
        for i, tacle_range in enumerate(table_ranges):
            table = StateTable(
                tacle_range_to_state_range(tacle_range), f"Table {i + 1}"
            )
            table["tacle_range"] = tacle_range
            new_state = new_state.add_table(table)
        return new_state

    def undo(self):
        pass

    def description(self):
        return "Detect Tables"


class DetectBlocksTask(BaseTask):
    def is_available(self):
        return len([t for t in self.state.tables if "tacle_table" not in t]) > 0

    def do(self):
        data = tacle.parse_csv(self.state.filepath)
        data_array = np.array(data, dtype=object)
        type_data = get_type_data(data_array)

        state = self.state.copy()
        for table in state.tables:
            if "tacle_range" in table:
                tacle_range = table["tacle_range"]  # type: tacle.indexing.Range
                table_data = tacle_range.get_data(data_array)
                supported_orientation = [
                    o
                    for o in tacle.indexing.Orientation.all()
                    if orientation_compatible(type_data, tacle_range, o)
                ]
                if len(supported_orientation) > 0:
                    tacle_table = tacle.indexing.Table(
                        table_data,
                        tacle_range.get_data(type_data),
                        tacle_range,
                        table.name,
                        supported_orientation,
                    )
                    table["tacle_table"] = tacle_table
        return state

    def undo(self):
        pass

    def description(self):
        return "Detect Blocks"


class TacleTask(BaseTask):
    def is_available(self):
        return len([t for t in self.state.tables if "tacle_table" in t]) > 0

    def do(self):
        tables = [
            table["tacle_table"]
            for table in self.state.tables
            if "tacle_table" in table
        ]
        constraints = tacle.learn_from_csv(self.state.filepath, tables=tables)
        state = self.state
        for constraint in constraints:
            constraint_dict = {"object_type": "constraint"}
            constraint_dict.update(constraint.to_dict())
            state = state.add_object(constraint_dict)
        return state

    def undo(self):
        pass

    def description(self):
        return "Learn Constraints"
