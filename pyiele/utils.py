#!/usr/bin/python3
import json
from .config import config
from .rlp import decode_response, encode_function, encode_contract
from .rpc import *
from time import sleep
from subprocess import run, PIPE
from os import path, listdir, makedirs

def get_result(input):
    '''Parses the response of an rpc method and retrieves the contains of the `result` field.'''
    try:
        return (json.loads(input.decode("utf8"))["result"])
    except KeyError:
        print("ERROR:", json.loads(input.decode("utf8"))["error"])
    except:
        print("ERROR: Could not decode input", input)

def send(rpc):
    '''Sends an rpc method call and returns the parsed response.'''
    return get_result(send_rpc(rpc, config.port))

def transaction_deploy_data(sender, data, gas_limit, gas_price):
    '''Template for a transaction which deploys a contract'''
    return ( "{" f""""from": "{sender}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def transaction_call_data(sender, data, to, gas_limit, gas_price):
    '''Template for a transaction which calls a contract function'''
    return ( "{" f""""from": "{sender}", "to":"{to}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def deploy_contract(walletId, sender, contract_bytecode, contract_args):
    '''RLP encodes the bytecode of a contract and sends a transaction to deploy the contract'''
    rlp = encode_contract(contract_bytecode, contract_args)
    tx_data = transaction_deploy_data(sender, rlp, config.gas_limit, config.gas_price)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, config.passphrase, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(config.sleep_time)
    contract_address = send(eth_getTransactionReceipt(tx_hash))["contractAddress"]
    return contract_address

def run_function(f_name, f_args, walletId, sender, to):
    '''-RLP encodes the `f_name` and `f_args` and sends a transaction to call `f_name`'''
    rlp = encode_function(f_name, f_args)
    tx_data = transaction_call_data(sender, rlp, to, config.gas_limit, config.gas_price)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, config.passphrase, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(config.sleep_time)
    receipt = send(eth_getTransactionReceipt(tx_hash))
    return_data = receipt["returnData"]
    return (decode_response(return_data))

def init_wallet():
    '''Retrieves the walletID and unlocks the wallet.'''
    walletId = send(wallet_restore(config.passphrase, config.spending_key))
    send(wallet_unlock(walletId, config.passphrase))
    if(send(eth_blockNumber()) == "0x0"):
        send(qa_mineBlocks(1, "true"))
        sleep(config.sleep_time)
    return walletId

def remove_metadata_bin(compiled_data, file_path, contract_name):
    '''Removes the trailing metadata bytecode from the bytecode of `contract_name`'''
    bin = compiled_data['contracts'][file_path + ":" + contract_name]['bin']
    metadatabin=compiled_data['contracts'][file_path + ":" + contract_name]['metadata-bin']
    if bin.endswith(metadatabin):
        bin = bin[:-len(metadatabin)]
    return bin

def get_bytecode_of(file_path, contract_name):
    '''- Takes a Solidity file and a contract name
       - Compiles the file and returns the bytecode of the `contract_name`, without the metadata-bin
    '''
    compiled_data = json.loads(compile_file(file_path))
    return remove_metadata_bin(compiled_data, file_path, contract_name)

def compile_file(file_path):
    '''Calls isolc on the provided file. Output is first written to a file and then returned'''
    print(" > Compiling", file_path)
    command = ["isolc", file_path, "--combined-json", "asm,bin,metadata-bin,srcmap,abi", "--allow-paths", "."]
    result = run(command, stdout=PIPE, stderr=PIPE, universal_newlines=True)
    if( result.returncode == 0 ):
        write_file(file_path, result.stdout)
        return result.stdout
    else:
        print(result.stderr)
        exit()

def get_file_name(file_path):
    '''Extracts only the name of a file from a path, ignoring its extension'''
    return path.splitext(path.basename(file_path))[0]

def write_file(file_path, content):
    ''' Writes `content` at `config.target_directory`/`file_name`.json'''
    if not path.exists(config.target_directory):
        makedirs(config.target_directory)
    file_name = get_file_name(file_path) + ".json"
    with open(config.target_directory + "/" + file_name, 'w') as f:
        f.write(content)

def assert_result(test_result, expected_result, msg):
    '''Checks if `test_result` and `expected_result` are equal. If not, `msg` is displayed.'''
    try:
        assert test_result==expected_result, msg
    except AssertionError as err:
        print(err)
        print(bcolors.SUCCESS, "    Expected:", expected_result, bcolors.ENDC)
        print(bcolors.FAIL, "    Received:", test_result, bcolors.ENDC)

class bcolors:
    SUCCESS = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

def compile(file_path=None):
    '''- Takes a path as an optional argument
       - If the path is a file, it compiles it
       - If it is a directory, compiles all the solidity files found in that dir
       - If no path is provided, compiles all solidity files from `config.contracts_dir` and `config.tests_dir`
    '''
    print("Compiling contracts ...")
    print("=======================")
    if(file_path != None):
        if path.isfile(file_path):
            compile_file(file_path)
        elif path.isdir(file_path):
            [compile_file(file_path + "/" + x) for x in listdir(file_path) if x.endswith(r".sol")]
    else:
        for directory in [config.contracts_dir, config.tests_dir]:
            [compile_file(directory + "/" + x) for x in listdir(directory) if x.endswith(r".sol")]

def run_test_file(file_path):
    '''- Takes a solidity file path as argument
       - Compiles the file, extracts the bytecode and all functions which start with "test"
       - intialises the wallet; deploys the contract and runs the test functions
    '''
    compiled_data = json.loads(compile_file(file_path))
    name = get_file_name(file_path)
    contract_name = name[0].upper() + name[1:]
    code = remove_metadata_bin(compiled_data, file_path, contract_name)
    test_functions = [x["name"] for x in compiled_data["contracts"][file_path + ":" + contract_name]["abi"] if x["name"].startswith(r"test") and x["type"] == "function"]

    print("\nContract:", contract_name)
    walletId = init_wallet()
    sender = send(wallet_generateTransparentAccount(walletId))["address"]
    contract_address = deploy_contract(walletId, sender, code, [])
    for test in test_functions:
        print("  Test: ", test)
        result = run_function(test, [], walletId, sender, contract_address)
        print("  Result:", result)
