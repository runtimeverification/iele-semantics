#!/usr/bin/python3
import json
from .config import *
from .rlpencoder import decode_response, encode_function
from .rpc import *
from time import sleep

def get_result(input):
    try:
        return (json.loads(input.decode("utf8"))["result"])
    except KeyError:
        print("ERROR:", json.loads(input.decode("utf8"))["error"])
    except:
        print("ERROR: Could not decode input", input)

def send(rpc):
    return get_result(send_rpc(rpc, PORT))

def transaction_deploy_data(sender, data, gas_limit, gas_price):
    return ( "{" f""""from": "{sender}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def transaction_call_data(sender, data, to, gas_limit, gas_price):
    return ( "{" f""""from": "{sender}", "to":"{to}", "gasLimit":"{gas_limit}", "gasPrice":"{gas_price}", "data": "{data}" """ "}" )

def deploy_contract(walletId, sender, bytecode):
    tx_data = transaction_deploy_data(sender, bytecode, GAS_LIMIT, GAS_PRICE)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, PASSPHRASE, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(SLEEP_TIME)
    contract_address = send(eth_getTransactionReceipt(tx_hash))["contractAddress"]
    return contract_address

def run_function(f_name, f_args, walletId, sender, to):
    rlp = encode_function(f_name, f_args)
    tx_data = transaction_call_data(sender, rlp, to, GAS_LIMIT, GAS_PRICE)
    private_key = send(wallet_getDefaultPrivateAddress(walletId))
    tx_hash = send(wallet_callContract(walletId, private_key, PASSPHRASE, tx_data))
    send(qa_mineBlocks(1, "true"))
    sleep(SLEEP_TIME)
    return_data = send(eth_getTransactionReceipt(tx_hash))["returnData"]
    return (decode_response(return_data))

def init_wallet():
    walletId = send(wallet_restore(PASSPHRASE, SPENDING_KEY))
    send(wallet_unlock(walletId, PASSPHRASE))
    if(send(eth_blockNumber()) == "0x0"):
        send(qa_mineBlocks(1, "true"))
        sleep(SLEEP_TIME)
    return walletId
