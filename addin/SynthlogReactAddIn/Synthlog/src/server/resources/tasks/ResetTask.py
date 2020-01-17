from .task import BaseTask
from state_manager import State


class ResetTask(BaseTask):
    def __init__(self, state: State):
        super().__init__(state)

    def do(self):
        return self.state.empty_copy()

    def undo(self):
        pass

    def description(self):
        return "Reset"
