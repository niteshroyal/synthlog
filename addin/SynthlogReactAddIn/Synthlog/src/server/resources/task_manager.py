import os
import shelve

# Do not remove the State import, it is required for pickle to work correctly
# noinspection PyUnresolvedReferences
from state_manager import StateManager, State
from tasks import ResetTask
from tasks.task import BaseTask

try:
    from tasks import tacle_tasks
except ImportError:
    tacle_tasks = None

try:
    from tasks import psyche
except ImportError:
    psyche = None

try:
    from tasks import MERCSTask
except ImportError:
    MERCSTask = None

try:
    from tasks import selection_tasks
except ImportError:
    selection_tasks = None


class TaskManager:
    def __init__(self):
        self.tasks_db_path = os.path.join(os.getcwd(), "tasks_db")
        self.actions = {}
        self.load_actions()
        self.db = None
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

    def get_task(self, task_id) -> BaseTask:
        if self.db:
            return self.db[str(task_id)]
        else:
            raise RuntimeError("Task database not loaded")

    def get_suggested_tasks(self, state: State, context: dict):
        task_pool = []

        if tacle_tasks is not None:
            task_pool.append(tacle_tasks.DetectTablesTask(state, context))
            task_pool.append(tacle_tasks.DetectBlocksTask(state, context))
            task_pool.append(tacle_tasks.TacleTask(state, context))

        if psyche is not None:
            task_pool.append(psyche.PsycheTask(state, context))

        if MERCSTask is not None:
            task_pool.append(MERCSTask.MERCSTask(state, context))

        if selection_tasks is not None:
            task_pool.append(selection_tasks.BaseSelectionTask(state, context))

        task_pool.append(ResetTask.ResetTask(state, context))
        # TODO Add MERCS back

        available_tasks = [t for t in task_pool if t.is_available()]
        task_ids = [i + len(self.db) + 1 for i in range(len(available_tasks))]
        for task, task_id in zip(available_tasks, task_ids):
            self.db[str(task_id)] = task
        return [
            {"id": k, "name": v}
            for k, v in zip(task_ids, [t.description() for t in available_tasks])
        ]

    def execute_task(self, task_id):
        task = self.get_task(task_id)
        new_state = task.do()
        new_state.previous_state_id = task.state.id
        return new_state
