import openpyxl
import csv
import json
import os
import sys

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("No argument given, or too many. 1 argument expected")
    elif not sys.argv[-1].endswith(".json"):
        print("File should be a json")
    else:
        with open(sys.argv[1]) as json_f:
            json_params = json.load(json_f)

        tables = json_params.tables
        filename = json_params.filename
        relevant_range = json_params.relevant_range
        unrelevant_range = json_params.unrelevant_range

        if filename.endswith(".csv"):
            filename = csv2xlsx(filename)

        # Do the conversion from strings of ranges to call to Yann code


def csv2xlsx(filename):
    wb = openpyxl.Workbook()
    ws = wb.active

    base, ext = os.path.splitext(filename)
    ws.title(os.path.splitext(os.path.basename(filename))[0])

    with open(filename) as f:
        reader = csv.reader(f, delimiter=',')
        for row in reader:
            ws.append(row)

    wb.save(base + '.xlsx')
    return base + ".xlsx"
