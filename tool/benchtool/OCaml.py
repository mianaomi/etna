from benchtool.BenchTool import BenchTool, Entry
from benchtool.Types import Config, LogLevel, ReplaceLevel, TrialArgs

import json
import os
import re
import subprocess
import ctypes
import platform

STRATEGIES_DIR = 'lib/Strategies'
IMPL_PATH = 'lib'
SPEC_PATH = 'lib/spec.ml'
WORKLOAD = 'BST'


class OCaml(BenchTool):

    def __init__(self, results: str, log_level: LogLevel = LogLevel.INFO, replace_level: ReplaceLevel = ReplaceLevel.REPLACE):
        super().__init__(
            Config(start='(*',
                   end='*)',
                   ext='.ml',
                   path='workloads/OCaml',
                   ignore='nothing',
                   strategies=STRATEGIES_DIR,
                   impl_path=IMPL_PATH,
                   spec_path=SPEC_PATH), results, log_level, replace_level)

    def all_properties(self, workload: Entry) -> list[Entry]:
        spec = os.path.join(workload.path, self._config.spec_path)
        with open(spec) as f:
             contents = f.read()
             regex = re.compile(r'prop_[^\s]*')
             matches = regex.findall(contents)
             return list(dict.fromkeys(matches))

    def _build(self, workload_path: str):
        with self._change_dir(workload_path):
            self._shell_command(['dune', 'build'])

    def _run_trial(self, workload_path: str, params: TrialArgs):

        def reformat():
            with open(params.file) as f:
                results = [json.loads(line) for line in f]
            open('file.txt', 'w').close()
            json.dump(results, open(params.file, 'w'))

        with self._change_dir(workload_path):
            for _ in range(params.trials):
                p = params.to_json()
                self._shell_command(['dune', 'exec', WORKLOAD])

        reformat()

    def _preprocess(self, workload: Entry) -> None:
        pass