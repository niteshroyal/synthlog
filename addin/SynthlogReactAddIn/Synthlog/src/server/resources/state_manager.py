import shelve
import argparse
import os
import json
import copy
import traceback
from typing import List, Dict, Optional, Any

from abc import ABC, abstractmethod

from tacle import Constraint
from tacle.indexing import Range as TacleRange


class Jsonifyable(ABC):
    """
    Class for a metadata. It needs to be json serializable
    """

    @abstractmethod
    def jsonify(self):
        pass


def jsonify(object_to_jsonify: Optional[Any]):
    try:
        return object_to_jsonify.jsonify() if object_to_jsonify is not None else None
    except AttributeError:
        return None


class MetadataPropObject(ABC):
    """
    Class for an object that contains metadata
    """

    def __init__(self, metadata):
        self.metadata = metadata if metadata is not None else []
        self.attributes = dict()

    def __setitem__(self, key, value):
        self.attributes[key] = value

    def __getitem__(self, item):
        return self.attributes[item]

    def __contains__(self, item):
        return item in self.attributes

    @abstractmethod
    def jsonify(self):
        result = []
        for m in self.metadata:
            converted = jsonify(m)
            if converted is not None:
                result.append(converted)
        attributes = dict()
        for k, v in self.attributes.items():
            converted = jsonify(v)
            if converted is not None:
                attributes[k] = converted
        return {"metadata": result, "attributes": attributes}


class Coordinate(MetadataPropObject):
    def __init__(self, x, y):
        super().__init__(None)
        self.x = x
        self.y = y
        self.address = Coordinate.pos_to_address(x, y)

    def jsonify(self):
        return {"x": self.x, "y": self.y, "address": self.address}

    @staticmethod
    def col_to_letter(col):
        return "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[col]

    @staticmethod
    def pos_to_address(x, y):
        return f"{Coordinate.col_to_letter(x)}{y + 1}"


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
            "fill": jsonify(self.fill),
            "font": jsonify(self.font),
            "borders": {k: jsonify(v) for k, v in self.borders.items()},
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
        tacle_range: TacleRange,
        values,
        formatting: ObjectFormatting,
        metadata: List[Jsonifyable],
    ):
        super().__init__(metadata)
        self.range_address = range_address  # Current selection in the spreadsheet, represented as an Excel range (a string): A2:B4 for example, or A2 if only 1 cell is selected
        self.tacle_range = tacle_range
        self.formatting = formatting
        self.values = values

    def jsonify(self):
        prop_dict = {
            "range_address": self.range_address,
            "values": self.values,
            "formatting": jsonify(self.formatting),
        }
        prop_dict.update(super().jsonify())
        return prop_dict

    @staticmethod
    def from_tacle_range(tacle_range: TacleRange):
        start = Coordinate(tacle_range.x0, tacle_range.y0)
        end = Coordinate(tacle_range.x1 - 1, tacle_range.y1 - 1)
        range_address = f"{start.address}:{end.address}"
        return Range(range_address, tacle_range, None, None, [])


class Table(MetadataPropObject):
    def __init__(
        self,
        table_range: Range,
        name: str,
        header: Optional[Range] = None,
        metadata: List[Jsonifyable] = None,
    ):
        super().__init__(metadata or [])
        self.range = table_range
        self.name = name
        self.header = header

    def jsonify(self):
        prop_dict = {
            "range": jsonify(self.range),
            "name": self.name,
            "header": jsonify(self.header),
        }
        prop_dict.update(super().jsonify())
        return prop_dict


class State(MetadataPropObject):
    def __init__(self, filepath: str, tables: List[Table], objects: List[Jsonifyable]):
        self.filepath = filepath
        self.tables = tables
        self.objects = objects
        self.id = None
        self.previous_state_id = None
        # Add more attributes here

    def get_filepath(self):
        return self.filepath

    def add_table(self, table):
        new_state = self.copy()
        new_state.tables.append(table)

        return new_state

    def add_object(self, an_object):
        new_state = self.copy()
        new_state.objects.append(an_object)

        return new_state

    def add_objects(self, objects):
        new_state = self.copy()
        new_state.objects += objects
        return new_state

    # Add many operations to move from one state to another

    def copy(self):
        new_state = copy.deepcopy(self)
        new_state.id = None
        return new_state

    def jsonify(self):
        """
        Converts the current state to a json string. This string is passed to the Excel client.
        """
        return {
            "id": self.id,
            "previous_id": self.previous_state_id,
            "filepath": self.filepath,
            # "tables": [t.jsonify() for t in self.tables],
        }

    def save_state(self, db_path=""):
        manager = StateManager(db_path=db_path)
        res = manager.add_state(self)
        manager.close_db()


class Prediction(MetadataPropObject):
    def __init__(self, coordinate, value, confidence, provenance, metadata=None):
        super().__init__(metadata)
        self.coordinate = coordinate
        self.value = value
        self.confidence = confidence
        self.provenance = provenance

    def jsonify(self):
        return {
            "coordinate": jsonify(self.coordinate),
            "value": self.value,
            "confidence": self.confidence,
            "provenance": self.provenance,
        }


class StateConverter:
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        raise NotImplementedError()


class TableConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        result = {"tables": [t.jsonify() for t in state.tables]}
        result.update(json_dict)
        return result


class BlockConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        blocks = []
        for table in state.tables:
            if "tacle_table" in table:
                tacle_table = table["tacle_table"]
                for block in tacle_table.blocks:
                    absolute_range = tacle_table.range.relative_to_absolute(
                        block.relative_range
                    )
                    blocks.append(
                        {
                            "table": table.name,
                            "range": Range.from_tacle_range(absolute_range).jsonify(),
                        }
                    )
        result = {"blocks": blocks}
        result.update(json_dict)
        return result


class ConstraintConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        constraints = []
        for o in state.objects:
            if isinstance(o, dict) and o.get("object_type", None) == "constraint":
                constraint = Constraint.from_dict(o)

                constraint_args = dict()
                for arg_name, arg_value in o["assignment"].items():
                    tacle_range = TacleRange.from_legacy_bounds(arg_value.bounds)
                    constraint_args[arg_name] = Range.from_tacle_range(tacle_range)

                constraints.append(
                    {
                        "template_name": constraint.template.name,
                        "name": constraint.template.to_string(
                            {k: v.range_address for k, v in constraint_args.items()}
                        ),
                        "args": {k: v.jsonify() for k, v in constraint_args.items()},
                        "is_formula": constraint.template.is_formula(),
                    }
                )
        result = {"constraints": constraints}
        result.update(json_dict)
        return result


class PredictionConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        result = {
            "predictions": [jsonify(o) for o in state.objects if type(o) == Prediction]
        }
        result.update(json_dict)
        return result


class StateManager:
    def __init__(self, db_path=""):
        if db_path:
            self.state_db_path = db_path
        else:
            self.state_db_path = os.path.join(os.getcwd(), "states_db")

        self.db = None
        self.load_db()

        self._latest_state = None
        self._latest_state_loaded = False

        self.converters = [
            TableConverter(),
            BlockConverter(),
            ConstraintConverter(),
            PredictionConverter(),
        ]  # type: List[StateConverter]

    def load_db(self):
        self.db = shelve.open(self.state_db_path, writeback=True)

    def close_db(self):
        self.db.close()

    def create_empty_state(self, filename):
        return State(filepath=filename, tables=[], objects=[])

    def get_latest_state(self) -> Optional[State]:
        if not self._latest_state_loaded:
            self.load_latest_state()
            self._latest_state_loaded = True
        return self._latest_state

    def get_state(self, state_id):
        if self.db:
            return self.db[str(state_id)]
        else:
            return None

    def load_latest_state(self):
        if self.db:
            if "latest" in self.db:
                latest_key = str(self.db["latest"])
                if latest_key in self.db:
                    try:
                        self._latest_state = self.db[latest_key]
                    except AttributeError:
                        raise
                else:
                    self._latest_state = None

    def set_latest(self, state):
        self._latest_state_loaded = True
        self._latest_state = state
        self.db["latest"] = str(state.id)

    def add_state(self, state):
        self._latest_state = state
        state.id = len(self.db) + 1
        state_id = str(state.id)
        self.db[state_id] = state
        self.db["latest"] = state.id
        return state.id

    def jsonify(self, state: State):
        json_dict = state.jsonify()
        for converter in self.converters:
            json_dict = converter.add_to_json(state, json_dict)
        return json_dict

    def print_state(self, state):
        print(json.dumps(self.jsonify(state)))
