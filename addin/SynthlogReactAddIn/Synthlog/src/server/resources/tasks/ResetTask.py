from .task import BaseTask


class ResetTask(BaseTask):
    def do(self):
        return self.state.empty_copy()

    def undo(self):
        pass

    def description(self):
        return "Reset"
