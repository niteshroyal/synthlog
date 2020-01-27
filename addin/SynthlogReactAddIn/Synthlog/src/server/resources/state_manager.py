import shelve
import os
import json
import copy
from typing import List, Dict, Optional, Any

from abc import ABC, abstractmethod

# from tacle import Constraint
# from tacle.indexing import Range as TacleRange


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
    except AttributeError as e:
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
        # noinspection SpellCheckingInspection
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
        fill: Optional[FillFormatting],
        font: Optional[FontFormatting],
        borders: Optional[Dict[str, BorderFormatting]] = None,
    ):
        self.fill = fill
        self.borders = borders or {}
        self.font = font

    def jsonify(self):
        return {
            "fill": jsonify(self.fill),
            "font": jsonify(self.font),
            "borders": None
            if self.borders is None
            else {k: jsonify(v) for k, v in self.borders.items()},
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
        formatting: Optional[ObjectFormatting],
        metadata: List[Jsonifyable],
    ):
        super().__init__(metadata)
        # Current selection in the spreadsheet, represented as an Excel range (a string):
        # A2:B4 for example, or A2 if only 1 cell is selected
        self.range_address = range_address
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
    def __init__(
        self,
        filepath: str,
        sheet_name: str,
        tables: List[Table],
        objects: List[Jsonifyable],
        metadata,
    ):
        super().__init__(metadata)
        self.filepath = filepath
        self.sheet_name = sheet_name
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
        return res

    def empty_copy(self):
        return StateManager.create_empty_state(self.filepath, self.sheet_name)


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


class Selection(MetadataPropObject):
    def __init__(self, cell, provenance, metadata=None):
        super().__init__(metadata)
        self.cell = cell
        self.provenance = provenance

    def jsonify(self):
        return {"cell": jsonify(self.cell), "provenance": self.provenance}


class StateConverter:
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        """
        A state converter takes a state and a intermediate conversion (json-dict) of that state and manipulates the
        intermediate converted json-dict.  It returns the result of its manipulation and might modify the input
        json-dict.
        :param state:  The state to be converted
        :param json_dict:  The (intermediate) JSON representation of the state
        :return:  An updated JSON representation of the state
        """
        raise NotImplementedError()


class TableConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        result = {"tables": [t.jsonify() for t in state.tables]}
        result.update(json_dict)
        return result


class PredictionConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        result = {
            "predictions": [jsonify(o) for o in state.objects if type(o) == Prediction]
        }
        result.update(json_dict)
        return result


class SelectionConverter(StateConverter):
    def add_to_json(self, state: State, json_dict: dict) -> dict:
        result = {
            "selections": [jsonify(o) for o in state.objects if type(o) == Selection]
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
            PredictionConverter(),
            SelectionConverter(),
        ]  # type: List[StateConverter]

        try:
            import tacle_state

            self.converters.append(tacle_state.BlockConverter())
            self.converters.append(tacle_state.ConstraintConverter())
        except ImportError:
            pass

    def load_db(self):
        self.db = shelve.open(self.state_db_path, writeback=True)

    def close_db(self):
        self.db.close()

    @staticmethod
    def create_empty_state(filename, sheet_name):
        return State(
            filepath=filename,
            sheet_name=sheet_name,
            tables=[],
            objects=[],
            metadata=None,
        )

    def get_latest_state(self, filename, sheet_name) -> Optional[State]:
        if not self._latest_state_loaded:
            self.load_latest_state(filename, sheet_name)
            self._latest_state_loaded = True
        return self._latest_state

    def get_state(self, state_id):
        if self.db:
            return self.db[str(state_id)]
        else:
            return None

    @staticmethod
    def get_key(filename, sheet_name):
        return f"current*{filename}*{sheet_name}"

    def load_latest_state(self, filename, sheet_name):
        if self.db:
            key = StateManager.get_key(filename, sheet_name)
            if key in self.db:
                latest_key = str(self.db[key])
                if latest_key in self.db:
                    try:
                        self._latest_state = self.db[latest_key]
                    except AttributeError:
                        raise
                else:
                    self._latest_state = None

    def set_latest(self, state: State):
        self._latest_state_loaded = True
        self._latest_state = state
        self.db[StateManager.get_key(state.filepath, state.sheet_name)] = str(state.id)

    def add_state(self, state):
        self._latest_state = state
        state.id = len(self.db) + 1
        state_id = str(state.id)
        self.db[state_id] = state
        self.db[StateManager.get_key(state.filepath, state.sheet_name)] = state.id
        return state.id

    def jsonify(self, state: State):
        json_dict = state.jsonify()
        for converter in self.converters:
            json_dict = converter.add_to_json(state, json_dict)
        return json_dict

    def print_state(self, state):
        print(json.dumps(self.jsonify(state)))
