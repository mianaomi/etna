from benchtool.Types import PBTGenerator
import os
import Collect
import Parse
import Analysis

# script to do the collection, parsing, and analysis all in one

WORKLOADS = [
    # 'BST',
    # 'RBT',
    'STLC',
]

STRATEGIES: list[PBTGenerator] = [
    # PBTGenerator('qcheck', 'bespoke'),
    # PBTGenerator('qcheck', 'type'),
    # PBTGenerator('crowbar', 'bespoke'),
    # PBTGenerator('crowbar', 'type'),
    # PBTGenerator('afl', 'bespoke'),
    # PBTGenerator('afl', 'type'),
    PBTGenerator('base', 'bespoke'),
    # PBTGenerator('base', 'type'),
]


def main():
    # initialize the main directories here
    cwd = os.path.dirname(os.path.realpath(__file__))
    raw = os.path.join(cwd, 'raw')
    parsed = os.path.join(cwd, 'parsed')
    analyzed = os.path.join(cwd, 'analyzed')
    for dir in [raw, parsed, analyzed]:
        os.makedirs(dir, exist_ok=True)
    print("[MAIN]: Created directories:", raw, parsed, analyzed, sep='\n')

    for workload in WORKLOADS:
        # initialize separate raw data directory for each workload
        raw_workload = os.path.join(raw, workload)
        os.makedirs(raw_workload, exist_ok=True)
        print("[MAIN]: Created directory:\n", raw_workload)

        # collect the raw data into this directory
        Collect.collect(raw_workload, [workload], STRATEGIES)
        print("[MAIN]: Finished collecting data for:", workload)

        # parse the workload data into one file
        parsed_workload = os.path.join(parsed, f'{workload}.json')
        Parse.parse_dir(raw_workload, parsed_workload)
        print("[MAIN]: Parsed data into:\n", parsed_workload)

        # create the plot
        Analysis.analyze(parsed, analyzed, [
                         s.framework + s.strategy.capitalize() for s in STRATEGIES], [workload])
        print(f"[MAIN]: Created plot in:\n{analyzed}/{workload}.png",)


if __name__ == '__main__':
    main()
