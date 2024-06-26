import argparse
from benchtool.Analysis import *
from benchtool.Plot import *
from functools import partial

# use this to adjust which plots are generated
WORKLOADS = ['BST', 'RBT', 'STLC']
WORKLOADS = ['RARE']
STRATEGIES = [
    # 'qcheckBespoke',
    'qcheckType',
    # 'crowbarBespoke',
    'crowbarType',
    # 'aflBespoke',
    'aflType',
    # 'baseBespoke',
    'baseType',
    'afl2Type',
]


def analyze(json_dir: str, image_dir: str, strategies=STRATEGIES, workloads=WORKLOADS):
    df = parse_results(json_dir)

    if not os.path.exists(image_dir):
        os.makedirs(image_dir)

    # Generate task bucket charts used in Figure 1.
    for workload in workloads:
        times = partial(stacked_barchart_times, case=workload, df=df)

        limits = [1, 10, 60, 605] if workload == 'RARE' else [0.1, 1, 10, 60]
        agg = 'any' if workload == 'RARE' else 'all'

        times(
            strategies=strategies,
            limits=limits,
            limit_type='time',
            image_path=image_dir,
            agg=agg,
            show=False,
        )

    # Compute solve rates.
    dfa = overall_solved(df, 'all').reset_index()
    dfa = dfa.groupby('strategy').sum(numeric_only=True)
    dfa['percent'] = dfa['solved'] / dfa['total']
    print(dfa)


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument('--data', help='path to folder for JSON data')
    p.add_argument('--figures', help='path to folder for figures')
    args = p.parse_args()

    results_path = f'{os.getcwd()}/{args.data}' if args.data else '/Users/nikhil/Code/Research/etna/experiments/ocaml-experiments/parsed'
    images_path = f'{os.getcwd()}/{args.figures}' if args.figures else '/Users/nikhil/Code/Research/etna/experiments/ocaml-experiments/analyzed'
    analyze(results_path, images_path)
