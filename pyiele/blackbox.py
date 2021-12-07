#!/usr/bin/python3

import sys
import os
import json
import time
import glob
from .transactionGeneration import generate_tx, generate_cons_args
from .config import config
from .rpc import *
from .testrunner import init_wallet, send, deploy_contract, transaction_call_data, mine_blocks
from .utils import *
from Crypto.Hash import keccak
from .rlp import *

def calculate_coverage(input):
    ''' Calculate the coverage percentage represented by the bytearray '''
    b_input = bytearray.fromhex(input[2:])
    count = 0
    for elem in b_input:
        if (elem):
            count=count + 1
    return (int((100 * count)/len(b_input)))

def get_coverage_report():
    ''' Get the coverage_report '''
    # Format: { bytecodeHash:coverageBytes, ... }
    report_raw = send(firefly_getCoverage())
    report = {}
    for contract in list(report_raw.keys()):
        report[contract] = calculate_coverage(report_raw[contract])
    return report

def get_contract_map(build_dir):
    ''' Get the contract_map '''
    # Format: { bytecodeHash:{"contractName":...,"bytecode":...}, ... }
    contract_map = {}
    solidity_files = glob.glob(os.getcwd() + '/' + build_dir + '/*.json')
    for solidity_file in solidity_files:
        with open(solidity_file) as solidity_json:
            data = json.load(solidity_json)
            for contract_name, contract_data in data["contracts"].items():
                code = remove_metadata_bin(contract_data)
                codehash = keccak.new(digest_bits=256)
                codehash.update(bytes.fromhex(code))
                contract_map["0x"+codehash.hexdigest()] = {"name": contract_name, "bytecode": code}
    return contract_map


def get_available_contract(coverage_report, contract_map):
    ''' Get the set of available_contract for testing '''
    # Format: { contractName:{"bytecodeHash":..., "coverage":...}, ... }
    available_contract = {}
    for bytehash in contract_map:
        if bytehash in coverage_report:
            available_contract[contract_map[bytehash]["name"]] = {"bytecodeHash": bytehash, "coverage": coverage_report[bytehash], "bytecode":contract_map[bytehash]["bytecode"]}
    return available_contract


def print_cov_info(contract_name, coverage_init, coverage_curr):
    ''' Print coverage report information for contract_name '''

    if coverage_curr > coverage_init:
        # Use green for improved coverage
        notif("Improved target coverage for contract " + contract_name, colors.fg.green)
    else:
        # Use orange for contracts that already meet the coverage
        notif("Could not improve coverage for " + contract_name, colors.fg.orange)

    report = contract_name + ": " + str(coverage_init) + " -> " + str(coverage_curr)

    # Use green for improved coverage, else orange
    notif(report, colors.fg.green if coverage_init < coverage_curr else colors.fg.orange)
    notif("")


def blackbox_test_single_contract(contract, available_contract):
    ''' Deploys a Solidity contract, extracts information about its functions.
        For each function it will generate random input values and execute the transaction. '''
    # Init essential parameters
    contract_name = contract["contractName"]
    bytecode_hash = available_contract[contract_name]["bytecodeHash"]
    coverage_init = available_contract[contract_name]["coverage"]
    bytecode      = available_contract[contract_name]["bytecode"]

    function_count = len(contract["functionData"])
    constructor_data = contract["contractConstructor"]

    coverage_curr = coverage_init
    notif("Start testing contract " + contract_name)
    notif("Found " + str(function_count) + " functions: " + str([ f['name'] for f in contract["functionData"]]))

    #Generate midnight address
    walletId = init_wallet()
    sender = send(wallet_generateTransparentAccount(walletId))["address"]
    
    #Deploy contract on network with possible args
    c_args = []
    printable_args = []
    if (len(constructor_data)):
        (c_args, printable_args) = generate_cons_args(constructor_data)
    contract_owner=deploy_contract(walletId, sender, bytecode, c_args)
    print("Deployed contract:", contract_name, "at address:", contract_owner)
    if(len(printable_args)):
        print("    with args:", printable_args)

    # Configure right format for contract_owner to deal with None case
    if len(contract_owner) == 0:
        contract_owner = "null"

    # Start finding new tests!
    contract_calls = []
    for function_sig in contract["functionData"]:
        notif("Finding tests for: " + function_sig['name'])
        notif("Current contract coverage is at " + str(coverage_curr))
        start_time = time.perf_counter()
        while int(time.perf_counter() - start_time) < config.coverage_timeout:
            # Randomly generate a transaction
            (rlp, func_name, func_args) = generate_tx(function_sig)
            tx_data = transaction_call_data(sender, rlp, contract_owner, config.gas_limit, config.gas_price)
            private_key = send(wallet_getDefaultPrivateAddress(walletId))
            tx_hash = send(wallet_callContract(walletId, private_key, config.passphrase, tx_data))
            mine_blocks(1)

            # Get the new coverage percentage for this contract after the eth_call
            coverage_new = 0
            result = send(firefly_getCoverage())
            if bytecode_hash in list(result.keys()):
                coverage_new = calculate_coverage(result[bytecode_hash])

            # Check whether coverage is improved
            if coverage_new > coverage_curr:
                notif("...Found a function call that increased coverage!:", colors.fg.cyan)
                printable = func_name + "(" + ", ".join(func_args) + ")"
                notif("called: " + printable)
                notif("txdata: " + json.dumps(tx_data))
                coverage_curr = coverage_new
                contract_calls.append({"contract": contract_name, "func_name": func_name, "func_args": func_args, "txdata": tx_data, "coverage": coverage_curr})
                notif("Current contract coverage is at " + str(coverage_curr))

    # Test for current contract ends, display the report
    print_cov_info(contract_name, coverage_init, coverage_curr)
    return contract_calls, constructor_data

