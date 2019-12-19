import shelve
import argparse
import os

class State:
    def __init__(self, filepath="", selection=""):
        self.filepath = filepath
        self.selection = selection # Current selection in the spreadsheet, represented as an Excel range (a string): A2:B4 for example, or A2 if only 1 celle is selected
        # Add more attributes here

    def get_filepath(self):
        return self.filepath

    # Add many operations to move from one state to another

class StateManager:
    def __init__(self, db_path=""):
        if db_path:
            self.state_db_path = db_path
        else:
            self.state_db_path = os.path.join(os.getcwd(), "states_db")

        self.db = None
        self.load_db()

        self.latest_state = None

    def load_db(self):
        self.db = shelve.open(self.state_db_path, writeback=True)

    def close_db(self):
        self.db.close()

    def get_latest_state(self):
        return self.latest_state

    def get_state(self, state):
        if self.db:
            return self.db[str(state)]
        else:
            return None

    def load_latest_state(self):
        if self.db:
            if "latest" in self.db:
                self.latest_state = self.db[str(self.db["latest"])]

    def add_state(self, state):
        self.latest_state = state
        state_id = str(len(self.db) + 1)
        self.db[state_id] = state
        self.db["latest"] = state_id
        return int(state_id)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--create", help="Create a new state", action="store_true")
    parser.add_argument("--filepath", help="Path to the spreadsheet file", type=str)
    parser.add_argument("--selection", help="Current selection, as a range (string)", type=str)
    args = parser.parse_args()

    if args.create:
        filepath = args.filepath if args.filepath else ""
        selection = args.selection if args.selection else ""
        state = State(filepath=filepath, selection=selection)
        manager = StateManager()
        res = manager.add_state(state)
        print(res)
        manager.close_db()

