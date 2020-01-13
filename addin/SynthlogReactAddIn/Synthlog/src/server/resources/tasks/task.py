from abc import ABC, abstractmethod


class BaseTask(ABC):
    @abstractmethod
    def __init__(self, state):
        self.state = state

    def is_available(self):
        return True

    @abstractmethod
    def do(self):
        pass

    @abstractmethod
    def undo(self):
        pass

    @abstractmethod
    def description(self):
        pass
