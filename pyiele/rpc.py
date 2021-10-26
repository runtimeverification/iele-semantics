#!/usr/bin/python3
from subprocess import Popen, PIPE
import json
## RPC Call helpers

def send_rpc(rpc, port):
    process = Popen(['curl', '--silent', '-H', 'Content-Type: application/json', '--data', rpc, 'http://localhost:' + str(port)], stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate()
    return stdout

def rpc_call(method, params=[], secrets=None):
    rpcobj = { "jsonrpc": "2.0", "id": 1, "method": method, "params": params }
    if(secrets):
      rpcobj["secrets"] = secrets
    return json.dumps(rpcobj)

def wallet_restore(passphrase, master_key):
    return rpc_call("wallet_restore", secrets={ "passphrase": passphrase, "masterKey": master_key })

def bech32_decodeTransparentAddress(address):
    return rpc_call("bech32_decodeTransparentAddress", params=[ address ])

def qa_mineBlocks(number, wait_confirm):
    return rpc_call("qa_mineBlocks", params=[ number, wait_confirm ])

def qa_getPendingTransactions():
    return rpc_call("qa_getPendingTransactions")

def wallet_start(walletId):
    return rpc_call("wallet_start", params=[ walletId ])

def wallet_getDefaultPrivateAddress(walletId):
    return rpc_call("wallet_getDefaultPrivateAddress", params=[ walletId ])

def wallet_callContract(walletId, private_key, passphrase, transaction_data):
    return rpc_call("wallet_callContract", params=[ walletId, { "sender": private_key, "transparentTx": transaction_data } ], secrets={ "passphrase": passphrase })

def wallet_generateTransparentAccount(walletId):
    return rpc_call("wallet_generateTransparentAccount", params=[ walletId ])

def firefly_getCoverage():
    return rpc_call("firefly_getCoverage")

def eth_getTransactionReceipt(txHash):
    return rpc_call("eth_getTransactionReceipt", params=[ txHash ])

def eth_getCode(address, tag):
    return rpc_call("eth_getCode", params=[ address, tag ])

def eth_blockNumber():
    return rpc_call("eth_blockNumber")
