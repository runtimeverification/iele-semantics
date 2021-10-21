#!/usr/bin/python3

import argparse
import sys
import json
from .fetchFunctionData import fetch_function_data
from .blackbox import *
from .testrunner import *
from .rpc import *
from .config import config
import os

def make_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command')

    blackboxParser = subparsers.add_parser('blackbox', help = 'Blackbox testing.')
    blackboxParser.add_argument('-p', '--port', type=int, default=8342, metavar="<port>",
                        help="Port that Midnight client is listen on (default: %(default)s)")
    blackboxParser.add_argument('-o', '--timeout', type=int, default=15, metavar="<coverage timeout>",
                        help="Timeout for trying each function (default: %(default)s)")
    blackboxParser.add_argument('--build-dir', type=str, default="contract_artifacts", metavar="<build dir>",
                        help="Path to the JSON output of solc (default: %(default)s)")
    blackboxParser.add_argument('--passphrase', type=str, default="walletNotSecure", metavar="<passphrase>",
                        help="Wallet passphrase (default: %(default)s)")
    blackboxParser.add_argument('--spending-key', type=str, default="m-test-shl-sk1fj335eanpmupaj9vx5879t7ljfnh7xct486rqgwxw8evwp2qkaksmcqu88", metavar="<spending key>",
                        help="Wallet spending key (default: %(default)s)")

    testrunnerParser = subparsers.add_parser('test', help = "Test runner.")
    testrunnerParser.add_argument('-p', '--port', type=int, default=8342, metavar="<port>",
                        help="Port that Midnight client is listen on (default: %(default)s)")
    testrunnerParser.add_argument('-l', '--gas-limit', type=str, default="0x166f5777", metavar="<gas limit>",
                        help="Transaction gas limit (default: %(default)s)")
    testrunnerParser.add_argument('-g', '--gas-price', type=str, default="0x0", metavar="<gas price>",
                        help="Gas price value (default: %(default)s)")
    testrunnerParser.add_argument('-f', '--file', type=str, metavar="<tests>",
                        help="Path of the test file/files to be executed (default: %(default)s)")
    testrunnerParser.add_argument('--build-dir', type=str, default="contract_artifacts", metavar="<build dir>",
                        help="Path to the JSON output of solc (default: %(default)s)")
    testrunnerParser.add_argument('--passphrase', type=str, default="walletNotSecure", metavar="<passphrase>",
                        help="Wallet passphrase (default: %(default)s)")
    testrunnerParser.add_argument('--spending-key', type=str, default="m-test-shl-sk1fj335eanpmupaj9vx5879t7ljfnh7xct486rqgwxw8evwp2qkaksmcqu88", metavar="<spending key>",
                        help="Wallet spending key (default: %(default)s)")
    testrunnerParser.add_argument('--tests-dir', type=str, default="tests", metavar="<tests-dir>",
                     help="Path to the default tests directory (default: %(default)s)")

    compileParser = subparsers.add_parser('compile', help='Compiling Solidity contracts using isolc.')
    compileParser.add_argument('-f', '--file', type=str, metavar="<solidity-file>",
                     help="Path to the Solidity file/dir to be compiled")
    compileParser.add_argument('--contracts-dir', type=str, default="contracts", metavar="<contracts-dir>",
                     help="Path to the default contracts directory (default: %(default)s)")
    compileParser.add_argument('--build-dir', type=str, default="contract_artifacts", metavar="<build dir>",
                        help="Path to the JSON output of solc (default: %(default)s)")

    return parser


if __name__ == '__main__':
    parser = make_parser()
    args = parser.parse_args()

    if args.command == 'compile':
        config.contracts_dir=args.contracts_dir
        config.target_directory=args.build_dir
        compile(args.file)

    elif args.command == 'test':
        config.port = args.port
        config.gas_limit = args.gas_limit
        config.gas_price = args.gas_price
        config.tests_dir = args.tests_dir
        config.target_directory = args.build_dir
        config.spending_key = args.spending_key
        config.passphrase = args.passphrase

        if(args.file is None):
            if os.path.isdir(config.tests_dir):
                for file_name in os.listdir(config.tests_dir):
                    if file_name.endswith(".sol"):
                        run_test_file(config.tests_dir + "/" + file_name)
            elif os.path.isfile(config.test_dir):
                    if file_name.endswith(".sol"):
                        run_test_file(file_name)
        else: 
            if (os.path.isdir(args.file)):
                for file_name in os.listdir(args.file):
                    if file_name.endswith(".sol"):
                        run_test_file(args.file)
            elif (os.path.isfile(args.file) and (args.file).endswith(".sol")):
                run_test_file(args.file)
            else:
                fatal("Invalid file: "+ args.file)

    elif args.command == 'blackbox':
        config.port = args.port
        config.coverage = timeout=args.timeout
        config.spending_key = args.spending_key
        config.passphrase = args.passphrase

        notif("Collecting data for blackbox random testing...")

        # Retrive coverage_report,contract_map
        coverage_report = get_coverage_report()
        contract_map = get_contract_map(config.target_directory)

        # Extract contracts that are available for blackbox testing
        available_contract = get_available_contract(coverage_report, contract_map)

        notif("Preparation finished, altogether " + str(len(available_contract)) + " contacts to test")
        notif("")

        # Process contracts in alphabetical order
        available_contract = dict(sorted(available_contract.items()))
        all_calls = []
        all_constructors = {}
        for solidity_file in available_contract:
            contract_json = config.target_directory + "/" + get_file_name(solidity_file) + ".json"
            solidity_functions = fetch_function_data(contract_json)
            for contract in solidity_functions:
                if(not contract["contractName"] in list(available_contract.keys())):
                    continue
                contract_calls, constructor_info = blackbox_test_single_contract(contract, available_contract)
                all_calls.extend(contract_calls)
            if len(constructor_info) == 0:
                all_constructors[solidity_file] = []
            else:
                all_constructors[solidity_file] = constructor_info[0]["input"]

        # Write founded tests to blackbox-random.json
        with open('blackbox-random.json', 'w') as outfile:
            json.dump(all_calls, outfile)


