from abc import ABC, abstractmethod
from state_manager import State


class BaseTask(ABC):
    def __init__(self, state: State):
        self.state = state

    def is_available(self) -> bool:
        return True

    @abstractmethod
    def do(self) -> State:
        pass

    def undo(self) -> State:
        return self.state

    @abstractmethod
    def description(self) -> str:
        pass
