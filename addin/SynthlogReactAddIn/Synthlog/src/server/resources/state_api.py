import argparse
import json
import traceback

from state_manager import StateManager


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="action")

    action_initialize = "initialize"
    init_parser = subparsers.add_parser(action_initialize)
    init_parser.add_argument("file_path", help="Path to the spreadsheet file", type=str)

    action_load = "load"
    load_parser = subparsers.add_parser(action_load)
    load_parser.add_argument(
        "state_id", help="The id of the state to be loaded", type=int
    )

    args = parser.parse_args()
    manager = StateManager()

    # noinspection PyBroadException
    try:
        if args.action == action_initialize:
            state = manager.get_latest_state(
                args.file_path
            )  # TODO Make filename dependent

            if state is None:
                state = manager.create_empty_state(args.file_path)
                manager.add_state(state)
                assert state is not None

            manager.print_state(state)

        elif args.action == action_load:
            state = manager.get_state(args.state_id)
            manager.set_latest(state)
            manager.print_state(state)

    except Exception as e:
        print(json.dumps({"exception": traceback.format_exc()}))
    finally:
        manager.close_db()
