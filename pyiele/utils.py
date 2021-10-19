#!/usr/bin/python3
import json
from .config import config
from .rlp import decode_response, encode_function, encode_contract
from .rpc import *
from time import sleep
from subprocess import run, PIPE
from os import path

def get_result(input):
    try:
        return (json.loads(input.decode("utf8"))["result"])
    except KeyError:
        print("ERROR:", json.loads(input.decode("utf8"))["error"])
    except:
        print("ERROR: Could not decode input", input)

def send(rpc):
    return get_result(send_rpc(rpc, config.port))

def transaction_deploy_data(sender, data, gas_limit, gas_price):
    return ( "{" f""""from": "{sender}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def transaction_call_data(sender, data, to, gas_limit, gas_price):
    return ( "{" f""""from": "{sender}", "to":"{to}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def deploy_contract(walletId, sender, bytecode):
    rlp = encode_contract(bytecode)
    tx_data = transaction_deploy_data(sender, rlp, config.gas_limit, config.gas_price)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, config.passphrase, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(config.sleep_time)
    contract_address = send(eth_getTransactionReceipt(tx_hash))["contractAddress"]
    return contract_address

def run_function(f_name, f_args, walletId, sender, to):
    rlp = encode_function(f_name, f_args)
    tx_data = transaction_call_data(sender, rlp, to, config.gas_limit, config.gas_price)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, config.passphrase, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(config.sleep_time)
    return_data = send(eth_getTransactionReceipt(tx_hash))["returnData"]
    return (decode_response(return_data))

def init_wallet():
    walletId = send(wallet_restore(config.passphrase, config.spending_key))
    send(wallet_unlock(walletId, config.passphrase))
    if(send(eth_blockNumber()) == "0x0"):
        send(qa_mineBlocks(1, "true"))
        sleep(config.sleep_time)
    return walletId

def get_bytecode_of(file_path, contract_name):
    file_dict = json.loads(compile_file(file_path))
    bin = file_dict['contracts']['erc20.sol:'+contract_name]['bin']
    metadatabin=file_dict['contracts']['erc20.sol:'+contract_name]['metadata-bin']
    if bin.endswith(metadatabin):
        bin = bin[:-len(metadatabin)]
    return bin

def compile_file(file_path):
    command = ["isolc", file_path, "--combined-json", "asm,bin,metadata-bin,srcmap"]
    result = run(command, stdout=PIPE, stderr=PIPE, universal_newlines=True)
    if( result.returncode == 0 ):
        write_file(file_path, result.stdout)
        return result.stdout
    else:
        print(result.stderr)
        return

def write_file(file_path, content):
    if not path.exists(config.directory):
        makedirs(config.directory)
    base = path.basename(file_path)
    file_name = path.splitext(base)[0] + ".json"
    with open(config.directory + "/" + file_name, 'w') as f:
        f.write(content)
    print(bcolors.BOLD, "Contracts compiled successfully at:", config.directory, bcolors.ENDC)

def assert_result(test_result, expected_result, msg):
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