import sys
sys.path.append("..")

from learner import BaseTask

class MERCSTask(BaseTask):
    def __init__(self, state=None, model=None, train=[], query=None):
        super().__init__(state)
        self.mercs_path = ""
        self.model = model
        self.train = train
        self.query = query

    def do(self):
        # Do the required actions (train, query, storing model...)
        print("Executing MERCS")
        print("State file", self.state.filepath)
        print("State selection", self.state.selection)

    def undo(self):
        # Undo the action (might not always be relevant)
        pass

    def descr(self):
        return "MERCS prediction"
