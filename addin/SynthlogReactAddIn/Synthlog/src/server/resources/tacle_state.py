from typing import List

from tacle import Constraint
from tacle.indexing import Range, Table

from state_manager import StateConverter, State, Range as StateRange, Coordinate


def tacle_range_to_state_range(tacle_range: Range) -> StateRange:
    start = Coordinate(tacle_range.x0, tacle_range.y0)
    end = Coordinate(tacle_range.x1 - 1, tacle_range.y1 - 1)
    range_address = f"{start.address}:{end.address}"
    state_range = StateRange(range_address, None, None, [])
    state_range["tacle_range"] = range
    return state_range


class ConstraintConverter(StateConverter):
    @staticmethod
    def get_constraints(state: State) -> List[Constraint]:
        constraints = []
        for o in state.objects:
            if isinstance(o, dict) and o.get("object_type", None) == "constraint":
                constraints.append(Constraint.from_dict(o))
        return constraints

    def add_to_json(self, state: State, json_dict: dict) -> dict:
        constraints = []

        tables_dict = {}
        for table in state.tables:
            if "tacle_table" in table:
                tacle_table = table["tacle_table"]  # type: Table
                tables_dict[tacle_table.name] = tacle_table

        for constraint in ConstraintConverter.get_constraints(state):
            constraint_args = dict()
            for arg_name, arg_value in constraint.assignment.items():
                table = tables_dict[arg_value.table.name]
                tacle_range = table.range.relative_to_absolute(
                    Range.from_legacy_bounds(arg_value.bounds)
                )
                constraint_args[arg_name] = tacle_range_to_state_range(tacle_range)

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

        if "constraints" in json_dict:
            json_dict["constraints"].append(constraints)
        else:
            json_dict["constraints"] = constraints
        return json_dict


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
                            "range": tacle_range_to_state_range(
                                absolute_range
                            ).jsonify(),
                        }
                    )
        result = {"blocks": blocks}
        result.update(json_dict)
        return result
