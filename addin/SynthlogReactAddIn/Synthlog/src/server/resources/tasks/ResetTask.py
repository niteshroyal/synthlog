from tacle import ranges_from_csv

from .task import BaseTask
from state_manager import Table, Range, State


class ResetTask(BaseTask):
    def __init__(self, state: State):
        super().__init__(state)

    def do(self):
        return None

    def undo(self):
        pass

    def description(self):
        return "Reset"

