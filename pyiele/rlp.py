#!/usr/bin/env python3

import rlp
import json
import sys

def encode_function(f_signature, f_args=[]):
    '''RLP encodes a function call in the format [f_signature, [f_args]]'''
    args = []
    for elem in f_args:
      if(isinstance(elem, int)):
          args.append(elem.to_bytes((elem.bit_length() + 7) // 8, 'big'))
      elif (isinstance(elem, str)):
          args.append(bytearray.fromhex(elem))
    return ("0x" + rlp.encode([bytearray(f_signature, "utf-8"), args]).hex())

def encode_contract(bytecode, c_args=[]):
    '''RLP encodes the bytecode of acontract in the format [bytecode, [c_args]]'''
    args = []
    for elem in c_args:
      args.append(elem.to_bytes((elem.bit_length() + 7) // 8, 'big'))
    return ("0x" + rlp.encode([bytearray.fromhex(bytecode), args]).hex())

def decode_response(input):
    '''Decodes an rlp encoded bytearray into a list of int values'''
    b_result = rlp.decode(bytearray.fromhex(input[2:]))
    result = []
    for elem in b_result:
      result.append(int.from_bytes(elem, "big"))
    return result
