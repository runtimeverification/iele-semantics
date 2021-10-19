#!/usr/bin/env python3

import rlp
import json
import sys

def encode_function(f_signature, f_args=[]):
    """encode_function("simpleTransfer(uint256,uint256)",["03E8","0190"])"""
    args = []
    for elem in f_args:
      args.append(elem.to_bytes((elem.bit_length() + 7) // 8, 'big'))
    return ("0x" + rlp.encode([bytearray(f_signature, "utf-8"), args]).hex())

def encode_contract(bytecode, c_args=[]):
    args = []
    for elem in c_args:
      args.append(elem.to_bytes((elem.bit_length() + 7) // 8, 'big'))
    return ("0x" + rlp.encode([bytearray.fromhex(bytecode), args]).hex())

def decode_response(input):
    b_result = rlp.decode(bytearray.fromhex(input[2:]))
    result = []
    for elem in b_result:
      result.append(int.from_bytes(elem, "big"))
    return result
