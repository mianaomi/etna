import re
import os
import json

DATA_PATH = './oc3/'
OUTPUT_FILE = './experiments/ocaml-experiments/stlc.json'
APPEND = False # if false, will override the contents in OUTPUT_FILE

def parse(filename):
    print(f"parsing {filename}")
    workload, strategy, mutant, prop = os.path.splitext(os.path.basename(filename))[0].split(',')

    file = ""
    with open(filename) as f:
        file = "".join(f.readlines())

    pattern_chunk = r'(?s)\[([.0-9]+) start\].*?\[([.0-9]+) exit (.*?)\]'
    matches = re.findall(pattern_chunk, file)
    data = []

    for start, end, code in matches:
        s = float(end) - float(start)
        passed = -1 # TODO: can we get this from crowbar?
        discards = -1
        # `code` can either be 1
        #                      0
        #                      "timeout"
        #                      "unexpectedly"
        foundbug = code != "timeout"
        if code == "unexpectedly": raise ValueError

        run = {
            "workload": workload,
            "discards": discards,
            "foundbug": foundbug,
            "strategy": strategy,
            "mutant": mutant,
            "passed": passed,
            "property": prop,
            "time": s, # NOTE: this time is in seconds.
        }
        data.append(run)

    return data



def main():
    filenames = os.listdir(DATA_PATH)
    filenames = list(map(lambda s: DATA_PATH + s, filenames))
    parsed = [run for filename in filenames for run in parse(filename)]


    if APPEND and os.path.exists(OUTPUT_FILE):
        with open(OUTPUT_FILE, 'r') as file:
            existing_data = json.load(file)
        print(f"Appending {len(existing_data)} items from existing file.")
    else:
        existing_data = []


    with open(OUTPUT_FILE, 'w') as f:
        json.dump(existing_data + parsed, f, indent=4)

    print(f"Successfully parsed through {DATA_PATH} directory")


if __name__ == '__main__':
    main()