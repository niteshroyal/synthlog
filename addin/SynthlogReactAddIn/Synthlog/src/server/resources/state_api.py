import argparse
import json
import traceback

from state_manager import StateManager


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="action")

    action_initialize = "initialize"
    init_parser = subparsers.add_parser(action_initialize)
    init_parser.add_argument("filepath", help="Path to the spreadsheet file", type=str)

    action_load = "load"
    load_parser = subparsers.add_parser(action_load)
    load_parser.add_argument("state_id", help="The id of the state to be loaded", type=int)

    # parser.add_argument("--create", help="Create a new state", action="store_true")
    # parser.add_argument(
    #     "--selection", help="Current selection, as a range (string)", type=str
    # )
    # parser.add_argument(
    #     "--tables",
    #     help="Tables in the spreadsheet, as a list of ranges. Ranges should be separated with a space",
    #     type=lambda s: [r for r in s.split(" ")],
    # )
    args = parser.parse_args()
    manager = StateManager()

    try:
        if args.action == action_initialize:
            latest_state = manager.get_latest_state()  # TODO Make filename dependent

            if latest_state:
                manager.print_state(latest_state)
            else:
                state = manager.create_empty_state(args.filepath)
                manager.add_state(state)
                assert manager.get_latest_state() is not None
                manager.print_state(state)

        elif args.action == action_load:
            state = manager.get_state(args.state_id)
            manager.set_latest(state)
            manager.print_state(state)

    except Exception as e:
        print(json.dumps({"exception": traceback.format_exc()}))
    finally:
        manager.close_db()
