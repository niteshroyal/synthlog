import state_manager
from state_manager import State

if __name__ == "__main__":
    manager = state_manager.StateManager()
    state = manager.get_state(19)
    print(state)
    manager.close_db()