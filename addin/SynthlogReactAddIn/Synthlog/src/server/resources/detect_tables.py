import sys
from tacle import ranges_from_csv

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("No argument given, or too many. 1 argument expected")
    elif not sys.argv[-1].endswith(".csv"):
        print("File should be a csv")
    else:
        print("\n".join([str(range) for range in ranges_from_csv(sys.argv[-1])]))

