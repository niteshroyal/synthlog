import shelve
import argparse
import os
import json
import copy
from typing import List, Dict

from abc import ABC, abstractmethod


class Jsonifyable(ABC):
    """
    Class for a metadata. It needs to be json serializable
    """

    @abstractmethod
    def jsonify(self):
        pass


class MetadataPropObject(ABC):
    """
    Class for an object that contains metadata
    """

    @abstractmethod
    def __init__(self, metadata):
        self.metadata = metadata

    @abstractmethod
    def jsonify(self):
        return {"metadata": [m.jsonify() for m in self.metadata]}


class FillFormatting:
    def __init__(self, color):
        self.color = color

    def jsonify(self):
        return self.__dict__


class FontFormatting:
    def __init__(self, bold, color, italic, name, size, underline):
        self.bold = bold
        self.color = color
        self.italic = italic
        self.name = name
        self.size = size
        self.underline = underline

    def jsonify(self):
        return self.__dict__


class BorderFormatting:
    def __init__(self, color, style, weight):
        self.color = color
        self.style = style
        self.weight = weight

    def jsonify(self):
        return self.__dict__


class ObjectFormatting:
    def __init__(
        self,
        fill: FillFormatting,
        font: FontFormatting,
        borders: Dict[str, BorderFormatting],
    ):
        self.fill = fill
        self.borders = borders
        self.font = font

    def jsonify(self):
        return {
            "fill": self.fill.jsonify(),
            "font": self.font.jsonify(),
            "borders": {k: v.jsonify() for k, v in self.borders.items()},
        }


class Cell(MetadataPropObject):
    def __init__(
        self,
        cell_address: str,
        value,
        formatting: ObjectFormatting,
        metadata: List[Jsonifyable],
    ):
        super().__init__(metadata)
        self.cell_address = cell_address
        self.formatting = formatting
        self.value = value

    def jsonify(self):
        prop_dict = {
            "cell_address": self.cell_address,
            "value": self.value,
            "formatting": self.formatting.jsonify(),
        }
        prop_dict.update(super().jsonify())
        return prop_dict


class Range(MetadataPropObject):
    def __init__(
        self,
        range_address: str,
        values,
        formatting: ObjectFormatting,
        metadata: List[Jsonifyable],
    ):
        super().__init__(metadata)
        self.range_address = range_address # Current selection in the spreadsheet, represented as an Excel range (a string): A2:B4 for example, or A2 if only 1 cell is selected
        self.formatting = formatting
        self.values = values

    def jsonify(self):
        prop_dict = {
            "range_address": self.range_address,
            "values": self.values,
            "formatting": self.formatting.jsonify(),
        }
        prop_dict.update(super().jsonify())
        return


class Table(MetadataPropObject):
    def __init__(
        self, range: Range, name: str, header: Range, metadata: List[Jsonifyable]
    ):
        super().__init__(metadata)
        self.range = range
        self.name = name
        self.header = header

    def jsonify(self):
        prop_dict = {
            "range": self.range.jsonify(),
            "name": self.name,
            "header": self.header.jsonify(),
        }
        prop_dict.update(super().jsonify())
        return prop_dict

class Selection:
    def __init__(self, range: Range):
        self.range = range

    def jsonify(self):
        return {"range": self.range.jsonify()}


class State(MetadataPropObject):
    def __init__(self, filepath: str, selection: Selection, tables: List[Table], objects: List[Jsonifyable], metadata: List[Jsonifyable]):
        self.filepath = filepath
        self.selection = selection
        self.tables = tables
        self.objects = objects
        # Add more attributes here

    def get_filepath(self):
        return self.filepath

    def add_table(self, table):
        new_state = copy.deepcopy(self)
        new_state.tables.append(table)

        return new_state

    def add_object(self, object):
        new_state = copy.deepcopy(self)
        new_state.objects.append(object)

        return new_state

    def add_selection(self, selection):
        new_state = copy.deepcopy(self)
        new_state.selection = selection

        return new_state
    # Add many operations to move from one state to another

    def jsonify(self):
        """
        Converts the current state to a json string. This string is passed to the Excel client.
        """
        return {
            "filepath": self.filepath,
            "selection": self.selection.jsonify(),
            "tables": [t.jsonify() for t in self.tables],
        }

    def save_state(self, db_path=""):
        manager = StateManager(db_path=db_path)
        res = manager.add_state(self)
        manager.close_db()

class StateManager:
    def __init__(self, db_path=""):
        if db_path:
            self.state_db_path = db_path
        else:
            self.state_db_path = os.path.join(os.getcwd(), "states_db")

        self.db = None
        self.load_db()

        self.latest_state = None

    def load_db(self):
        self.db = shelve.open(self.state_db_path, writeback=True)

    def close_db(self):
        self.db.close()

    def get_latest_state(self):
        return self.latest_state

    def get_state(self, state):
        if self.db:
            return self.db[str(state)]
        else:
            return None

    def load_latest_state(self):
        if self.db:
            if "latest" in self.db:
                self.latest_state = self.db[str(self.db["latest"])]

    def add_state(self, state):
        self.latest_state = state
        state_id = str(len(self.db) + 1)
        self.db[state_id] = state
        self.db["latest"] = state_id
        return int(state_id)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--create", help="Create a new state", action="store_true")
    parser.add_argument("--filepath", help="Path to the spreadsheet file", type=str)
    parser.add_argument(
        "--selection", help="Current selection, as a range (string)", type=str
    )
    parser.add_argument(
        "--tables",
        help="Tables in the spreadsheet, as a list of ranges. Ranges should be separated with a space",
        type=lambda s: [r for r in s.split(" ")],
    )
    args = parser.parse_args()

    if args.create:
        # filepath = args.filepath if args.filepath else ""
        # selection = args.selection if args.selection else ""
        # tables = args.tables if args.tables else []
        # state = State(filepath=filepath, selection=selection, tables=tables)
        filepath = args.filepath if args.filepath else ""
        state = State(filepath=filepath, selection=None, tables=[], objects=[], metadata=[])
        manager = StateManager()
        res = manager.add_state(state)
        print(res)
        manager.close_db()

