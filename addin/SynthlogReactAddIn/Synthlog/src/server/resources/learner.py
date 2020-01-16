import json
import traceback

from tasks import tacle_tasks, ResetTask, psyche
import shelve
import argparse
import os
from state_manager import StateManager, State


class TaskManager:
    def __init__(self, state, context={}):
        self.tasks_db_path = os.path.join(os.getcwd(), "tasks_db")
        self.actions = {}
        self.load_actions()
        self.state = state
        self.db = None
        self.context = context

        self.load_actions()

    def load_actions(self):
        self.db = shelve.open(self.tasks_db_path, writeback=True)

    def close_db(self):
        if self.db:
            self.db.close()

    # def load_state(self):
    #     manager = StateManager()
    #     if self.state_id:
    #         self.state = manager.get_state(self.state_id)
    #     else:
    #         self.state = manager.get_latest_state()
    #     manager.close_db()

    def get_task(self, task_id):
        if self.db:
            return self.db[str(task_id)]

    def get_suggested_tasks(self):
        task_pool = [
            tacle_tasks.DetectTablesTask(self.state),
            tacle_tasks.DetectBlocksTask(self.state),
            tacle_tasks.TacleTask(self.state),
            psyche.PsycheTask(self.state),
            ResetTask.ResetTask(self.state),
        ]  # TODO Add MERCS back
        available_tasks = [t for t in task_pool if t.is_available()]
        task_ids = [i + len(self.db) + 1 for i in range(len(available_tasks))]
        for task, task_id in zip(available_tasks, task_ids):
            self.db[str(task_id)] = task
        return [{"id": k, "name": v} for k, v in zip(task_ids, [t.description() for t in available_tasks])]

    def execute_task(self, task_id):
        task = self.get_task(task_id)
        task.set_context(self.context)
        task.state = self.state
        return task.do()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--get", help="Gets new tasks", action="store_true")
    parser.add_argument("--execute", help="Execute the task with the given task id")
    parser.add_argument("--context", help="JSON string representing context")
    parser.add_argument("--state", help="State id of the spreadsheet, retrieved from the state database")
    parser.add_argument("--dev", help="Execute in development mode (raise exception, pretty print json)",
                        action="store_true")
    args = parser.parse_args()

    learner = None
    context = {}
    state_manager = StateManager()

    try:
        if args.context:
            context = json.loads(args.context)
        if args.get:
            if args.state:
                learner = TaskManager(args.state, context)
            else:
                learner = TaskManager(state_manager.get_latest_state(), context)
            tasks = learner.get_suggested_tasks()
            print(json.dumps(tasks))
            learner.close_db()

        if args.execute:

            if args.state:
                learner = TaskManager(args.state, context)
            else:
                state = state_manager.get_latest_state()
                assert state is not None
                learner = TaskManager(state, context)

            new_state = learner.execute_task(args.execute)

            if new_state is None:
                new_state = state_manager.create_empty_state(learner.state.filepath)

            new_state.previous_state_id = learner.state.id
            state_manager.add_state(new_state)

            print(json.dumps(state_manager.jsonify(new_state), indent=(4 if args.dev else None)))
            learner.close_db()
    except Exception:
        if args.dev:
            raise
        else:
            print(json.dumps({"exception": traceback.format_exc()}))
    finally:
        state_manager.close_db()
