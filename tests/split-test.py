#!/usr/bin/env python


import sys
import json
import os
import subprocess

# Example usage: tests/ethereum-tests/VMTests/abc.json tests/VMTests/abc/
source_file = sys.argv[1]
target_dir = sys.argv[2]

evm_test_to_iele = os.path.join(os.path.dirname(__file__), "evm-to-iele", "evm-test-to-iele")

with open (source_file, "r") as source:
    original_test = json.load(source)
    for subtest in original_test.keys():
        target_file = os.path.join(target_dir, subtest + ".json")
        target_iele_file = os.path.join(target_dir, subtest + ".iele.json")
        with open (target_file, "w+") as target:
            json.dump({ subtest: original_test[subtest] }, target, indent=4)
        with open (target_iele_file, "w+") as target:
            subprocess.check_call([evm_test_to_iele, target_file, target_iele_file])
