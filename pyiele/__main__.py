#!/usr/bin/python3

import argparse
import sys
import json
from .fetchFunctionData import fetch_function_data
from .blackbox import *
from .testrunner import *
from .rpc import *
from .config import config
from .kieleCoverage import *
import os

def make_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='command')
    parser.add_argument('-p', '--port', type=int, default=config.port, metavar="<port>",
                      help="Port that Midnight client is listen on (default: %(default)s)")
    parser.add_argument('--host', type=str, default=config.host, metavar="<host>",
                      help="Address of the Midnight client (default: %(default)s)")
    parser.add_argument('-o', '--output', type=str, default='report.json', metavar='<output file>', dest='output',
                      help='Output file')

    blackboxParser = subparsers.add_parser('blackbox', help = 'Blackbox testing.')
    blackboxParser.add_argument('-o', '--timeout', type=int, default=config.coverage_timeout, metavar="<coverage timeout>",
                      help="Timeout for trying each function (default: %(default)s)")
    blackboxParser.add_argument('--build-dir', type=str, default=config.target_directory, metavar="<build dir>",
                      help="Path to the JSON output of solc (default: %(default)s)")
    blackboxParser.add_argument('--passphrase', type=str, default=config.passphrase, metavar="<passphrase>",
                      help="Wallet passphrase (default: %(default)s)")
    blackboxParser.add_argument('--master-key', type=str, default=config.master_key, metavar="<master key>",
                      help="Wallet master key (default: %(default)s)")

    testrunnerParser = subparsers.add_parser('test', help = "Test runner.")
    testrunnerParser.add_argument('-l', '--gas-limit', type=str, default=config.gas_limit, metavar="<gas limit>",
                        help="Transaction gas limit (default: %(default)s)")
    testrunnerParser.add_argument('-g', '--gas-price', type=str, default=config.gas_price, metavar="<gas price>",
                        help="Gas price value (default: %(default)s)")
    testrunnerParser.add_argument('-f', '--file', type=str, metavar="<tests>",
                        help="Path of the test file/files to be executed")
    testrunnerParser.add_argument('--build-dir', type=str, default=config.target_directory, metavar="<build dir>",
                        help="Path to the JSON output of solc (default: %(default)s)")
    testrunnerParser.add_argument('--passphrase', type=str, default=config.passphrase, metavar="<passphrase>",
                        help="Wallet passphrase (default: %(default)s)")
    testrunnerParser.add_argument('--master-key', type=str, default=config.master_key, metavar="<master key>",
                        help="Wallet master key (default: %(default)s)")
    testrunnerParser.add_argument('--tests-dir', type=str, default=config.tests_dir, metavar="<tests-dir>",
                        help="Path to the default tests directory (default: %(default)s)")

    compileParser = subparsers.add_parser('compile', help='Compiling Solidity contracts using isolc.')
    compileParser.add_argument('-f', '--file', type=str, metavar="<solidity-file>",
                     help="Path to the Solidity file/dir to be compiled")
    compileParser.add_argument('--contracts-dir', type=str, default=config.contracts_dir, metavar="<contracts-dir>",
                     help="Path to the default contracts directory (default: %(default)s)")
    compileParser.add_argument('--build-dir', type=str, default=config.target_directory, metavar="<build dir>",
                     help="Path to the JSON output of isolc (default: %(default)s)")

    coverageParser = subparsers.add_parser('coverage', help='Generate report.json')
    coverageParser.add_argument('-b', '--build-dir',type=str, default=config.target_directory, metavar='<isolc output>',
                      help='Path of the JSON output of isolc (default: %(default)s)')
    return parser


if __name__ == '__main__':
    parser = make_parser()
    args = parser.parse_args()
    config.port = args.port
    config.host = args.host

    if args.command == 'compile':
        config.contracts_dir    = args.contracts_dir
        config.target_directory = args.build_dir
        compile(args.file)

    elif args.command == 'test':
        config.gas_limit = args.gas_limit
        config.gas_price = args.gas_price
        config.tests_dir = args.tests_dir
        config.target_directory = args.build_dir
        config.master_key = args.master_key
        config.passphrase = args.passphrase

        if(args.file is None):
            if os.path.isdir(config.tests_dir):
                for child_file in os.listdir(config.tests_dir):
                    if child_file.endswith(".sol"):
                        run_test_file(config.tests_dir + "/" + child_file)
            else:
                fatal("Invalid path: "+ config.tests_dir)
        else:
            file_path = args.file
            if (os.path.isdir(file_path)):
                for child_file in os.listdir(file_path):
                    if child_file.endswith(".sol"):
                        run_test_file(file_path + "/" + child_file)
            elif (os.path.isfile(file_path) and (file_path).endswith(".sol")):
                run_test_file(file_path)
            else:
                fatal("Invalid file: "+ file_path)
        print("==  Passing Tests:", config.passing_tests)
        print("==  Failing Tests:", config.failing_tests)

    elif args.command == 'blackbox':
        config.coverage_timeout = timeout=args.timeout
        config.master_key = args.master_key
        config.passphrase = args.passphrase

        notif("Collecting data for blackbox random testing...")

        # Retrive coverage_report,contract_map
        coverage_report = get_coverage_report()
        contract_map = get_contract_map(config.target_directory)

        # Extract contracts that are available for blackbox testing
        available_contract = get_available_contract(coverage_report, contract_map)

        notif("Preparation finished, altogether " + str(len(available_contract)) + " contracts to test")
        notif("")

        # Process contracts in alphabetical order
        available_contract = dict(sorted(available_contract.items()))
        all_calls = []
        all_constructors = {}
        for contract_id in available_contract:
            compiled_json = config.target_directory + "/" + get_file_name(contract_id) + ".json"
            contracts = fetch_function_data(compiled_json)
            for contract in contracts:
                if(contract["contractName"] != contract_id):
                    print("Skipped", contract["contractName"])
                    continue
                contract_calls, constructor_info = blackbox_test_single_contract(contract, available_contract)
                all_calls.extend(contract_calls)
            if len(constructor_info) == 0:
                all_constructors[contract_id] = []
            else:
                all_constructors[contract_id] = constructor_info[0]["input"]

        # Write founded tests to blackbox-random.json
        with open('blackbox-random.json', 'w') as outfile:
            json.dump(all_calls, outfile)

    elif args.command == 'coverage':
        """ Parse command line arguments """
        config.target_directory = args.build_dir
        output = {}

        """ Start JSON report with locally available data """
        if (os.path.isdir(config.target_directory)):
            for filepath in os.listdir(config.target_directory):
                if filepath.endswith(".json"):
                    output.update(process_file(config.target_directory + "/" + filepath))
        else:
            fatal("Could not access path:" + config.target_directory)
        """ Get coverage from client """
        retrieve_coverage(output)

        to_remove = []
        for contract_file, contract_file_data in output.items():
            for c_hash, contract_item in contract_file_data["contracts"].items():
                if "coverage" not in contract_item:
                    to_remove.append((contract_file, c_hash))

        for (contract_file, c_hash) in to_remove:
            del output[contract_file]["contracts"][c_hash]
            if output[contract_file]["contracts"] == {}:
                del output[contract_file]

        with open(args.output, mode='w') as f:
          f.write(json.dumps(output, indent=2))

