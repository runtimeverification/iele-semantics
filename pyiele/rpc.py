#!/usr/bin/python3
from subprocess import Popen, PIPE
## RPC Call helpers

def send_rpc(rpc, port):
    process = Popen(['curl', '--silent', '-H', 'Content-Type: application/json', '--data', rpc, 'http://localhost:' + str(port)], stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate()
    return stdout

def rpc_call(method, params="", secrets=None):
    if(secrets):
      return ( "{" f""""jsonrpc":"2.0","id":1,"method":"{method}", "secrets":{secrets}, "params":[{params}]""" "}" )
    else:
      return ( "{" f""""jsonrpc":"2.0","id":1,"method":"{method}", "params":[{params}]""" "}" )

def wallet_restore(passphrase, spending_key):
    return rpc_call("wallet_restore", secrets="{" f""""passphrase":"{passphrase}", "spendingKey":"{spending_key}" """ "}")

def bech32_decodeTransparentAddress(address):
    return rpc_call("bech32_decodeTransparentAddress", params=f""""{address}" """)

def qa_mineBlocks(number, wait_confirm):
    return rpc_call("qa_mineBlocks", params=f"""{number}, {wait_confirm}""")

def qa_getPendingTransactions():
    return rpc_call("qa_getPendingTransactions")

def wallet_unlock(walletId, passphrase):
    return rpc_call("wallet_unlock", params=f""""{walletId}" """, secrets="{" f""""passphrase":"{passphrase}" """ "}")

def wallet_getDefaultPrivateAddress(walletId):
    return rpc_call("wallet_getDefaultPrivateAddress", params=f""""{walletId}" """)

def wallet_callContract(walletId, private_key, passphrase, transaction_data):
    return rpc_call("wallet_callContract", params=f""""{walletId}","{private_key}",{transaction_data} """, secrets="{" f""""passphrase":"{passphrase}" """ "}")

def wallet_generateTransparentAccount(walletId):
    return rpc_call("wallet_generateTransparentAccount", params=f""""{walletId}" """)

def firefly_getCoverage():
    return rpc_call("firefly_getCoverage")

def eth_getTransactionReceipt(txHash):
    return rpc_call("eth_getTransactionReceipt", params=f""""{txHash}" """)

def eth_getCode(address, tag):
    return rpc_call("eth_getCode", params= f""""{address}","{tag}" """)

def eth_blockNumber():
    return rpc_call("eth_blockNumber")
