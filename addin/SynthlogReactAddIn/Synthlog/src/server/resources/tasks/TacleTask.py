from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from addin.SynthlogReactAddIn.Synthlog.src.server.resources.learner import BaseTask


import tacle


class TacleTask(BaseTask):
    def __init__(self, state):
        super().__init__(state)

    def do(self):
        tacle.learn_from_cells()

    def undo(self):
        pass

    def description(self):
        return "TaCLe"
