import tacle
import numpy as np

from state_manager import State
from .task import BaseTask


class PsycheTask(BaseTask):
    def find_missing_cells(self):
        data = np.array(tacle.parse_csv(self.state.filepath), dtype=object)
        indices = []
        for table in self.state.tables:
            tacle_range = table.range.tacle_range
            table_data = tacle_range.get_data(data)
            table_indices = np.argwhere(table_data == '')  # (y, x)
            for i in range(table_indices.shape[0]):
                indices.append((tacle_range.x0 + table_indices[i, 1], tacle_range.y0 + table_indices[i, 0]))
        return indices

    def is_available(self):
        # TODO Check if tables exist and there are missing values
        return True

    def do(self) -> State:
        print(self.find_missing_cells())

        exit(0)
        return self.state

    def description(self):
        return "Autocomplete"
