from abc import ABC, abstractmethod
from tasks import MERCSTask
import state_manager
import shelve
import argparse
import os
from state_manager import State

class BaseTask(ABC):

    @abstractmethod
    def __init__(self, state):
        self.state = state

    @abstractmethod
    def do(self):
        pass

    @abstractmethod
    def undo(self):
        pass

    @abstractmethod
    def description(self):
        pass


class Learner():
    def __init__(self, state_id=None):
        self.tasks_db_path = os.path.join(os.getcwd(), "tasks_db")
        self.actions = {}
        self.load_actions()
        self.state = None
        self.state_id = state_id
        self.db = None

        self.load_actions()
        self.load_state()

    def load_actions(self):
        self.db = shelve.open(self.tasks_db_path, writeback=True)

    def close_db(self):
        if self.db:
            self.db.close()

    def load_state(self):
        manager = state_manager.StateManager()
        if self.state_id:
            self.state = manager.get_state(self.state_id)
        else:
            self.state = manager.get_latest_state()
        manager.close_db()

    def get_task(self, task_id):
        if self.db:
            return self.db[str(task_id)]

    def get_suggested_tasks(self):
        # Given the state, find the right task
        # For now always a default merc task
        new_task = MERCSTask.MERCSTask()
        task_id = str(len(self.db)+1)
        self.db[task_id] = new_task

        return [(int(task_id), new_task.descr())]

    def execute_task(self, task_id):
        task = self.get_task(task_id)
        task.state = self.state
        task.do()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--get", help="Gets new tasks", action="store_true")
    parser.add_argument("--execute", help="Execute the task with the given task id")
    parser.add_argument("--state", help="State id of the spreadsheet, retrieved from the state database")
    args = parser.parse_args()

    learner = None
    if args.get:
        if args.state:
            learner = Learner(args.state)
        else:
            learner = Learner()
        tasks = learner.get_suggested_tasks()
        for t in tasks:
            print(t)
        learner.close_db()

    if args.execute:
        if args.state:
            learner = Learner(args.state)
        else:
            learner = Learner()
        learner.execute_task(args.execute)
        learner.close_db()


