import argparse
import json
import traceback
import logging

from task_manager import TaskManager
from state_manager import StateManager

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    sub_parsers = parser.add_subparsers(dest="action")

    action_get = "get"
    get_parser = sub_parsers.add_parser(action_get)
    get_parser.add_argument("state_id", help="The state-id", type=int)
    get_parser.add_argument("context", help="The context (as JSON string)", type=str)

    action_execute = "execute"
    execute_parser = sub_parsers.add_parser(action_execute)
    execute_parser.add_argument("task_id", help="The task-id to be executed", type=int)

    parser.add_argument(
        "-d",
        "--dev",
        help="Run API in development mode (when executing from a CLI)",
        action="store_true",
    )

    args = parser.parse_args()

    task_manager = None
    state_manager = StateManager()

    if args.dev:
        logging.basicConfig(level=logging.DEBUG)

    # noinspection PyBroadException
    try:
        if args.action == action_get:
            if args.state_id:
                state = state_manager.get_state(args.state_id)
            else:
                state = state_manager.get_latest_state(args.file_path)

            context = json.loads(args.context) if args.context else None
            task_manager = TaskManager()
            tasks = task_manager.get_suggested_tasks(state, context)
            print(json.dumps(tasks))
            task_manager.close_db()

        elif args.action == action_execute:
            task_manager = TaskManager()
            new_state = task_manager.execute_task(args.task_id)
            state_manager.add_state(new_state)

            print(
                json.dumps(
                    state_manager.jsonify(new_state), indent=(3 if args.dev else None)
                )
            )
            task_manager.close_db()
    except Exception:
        if args.dev:
            raise
        else:
            print(json.dumps({"exception": traceback.format_exc()}))
    finally:
        state_manager.close_db()
