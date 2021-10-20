#!/usr/bin/python3

import sys
import json
import random
from .rlp import encode_function

def get_printable_func(type: str):
    printable_funcs = {
        "string" : lambda x : repr(x.decode('utf-8')),
        "bytes"  : lambda x : "0x" + x.hex(),
        "int"    : lambda x : str(int.from_bytes(x,'big')),
        "address": lambda x : "0x" + x.hex(),
        "bool"   : lambda x : 'true' if bool.from_bytes(x,'big') else 'false',
        "array"  : lambda f : lambda x : "[" + ", ".join(map(f,x)) + "]"
    }
    is_dynamic_array = type.endswith("[]")
    if is_dynamic_array:
        type = type[:-2]
    if type.startswith("uint") or type.startswith("int"):
        type = "int"
    if type.startswith("bytes"):
        type = "bytes"
    if is_dynamic_array:
        return printable_funcs["array"](printable_funcs[type])
    else:
        return printable_funcs[type]

def generate_random_value(type: str):
    random_value = ""
    choices  = range(0, 256)
    value_length = random.randint(1, 255)
    is_dynamic_array = type.endswith("[]")
    try:
        if is_dynamic_array:
            type = type[:-2]
        if type == "string":
            choices = range(ord(' '),ord('~'))
        elif type.startswith("bytes") and type != "bytes":
            value_length = int(type[5:])
        elif type.startswith("uint"):
            value_length = int(type[4:]) // 8
        elif type.startswith("int"):
            value_length = int(type[3:]) // 8
        elif type == "address":
            value_length = 20
        elif type == "bool":
            choices = range(2)
            value_length = 1
    except:
        raise Exception("Type not defined")
        sys.exit(2)

    if is_dynamic_array:
        return [ bytes(random.choices(choices,k=value_length)) ]
    else:
        return bytes(random.choices(choices,k=value_length))

def encode_random_value(value, type: str) -> str:
    is_dynamic_type = False
    is_static_bytes = False
    is_dynamic_array = False
    encoded_value = ""

    try:
        is_dynamic_array = type.endswith("[]")
        if is_dynamic_array:
            type = type[:-2]
        if type == "string" or type == "bytes":
            is_dynamic_type = True
        elif type.startswith("bytes"):
            is_static_bytes = True
    except:
        raise Exception("Type not defined")
        sys.exit(2)

    if is_dynamic_array:
        encoded_value += "1".zfill(64)
        value = value[0]

    if is_dynamic_type:
        value_length = len(value)
        length = (len(value) // 32) * 32 + 32
        encoded_value += str(hex(value_length))[2:].zfill(64) + value.hex().ljust(length * 2, "0")
    elif is_static_bytes:
        encoded_value += value.hex().ljust(64, '0')
    else:
        encoded_value += value.hex().zfill(64)
    return encoded_value

def generate_cons_args(constructor_data: dict) -> (list, list):
    in_list = constructor_data[0]["input"]
    printable_list = []
    result = []
    for in_obj in in_list:
        type = in_obj["type"]
        value = generate_random_value(type)
        printable_list.append(get_printable_func(type)(value))
        encoded_value = encode_random_value(value, type)
        result.append(encoded_value)
    return (result, printable_list)

def generate_tx(function_data: dict) -> (str, str, list):
    in_list = function_data["input"]
    tx_list = []
    printable_list = []

    for in_obj in in_list:
        type = in_obj["type"]
        value = generate_random_value(type)
        printable_list.append(get_printable_func(type)(value))
        encoded_value = encode_random_value(value, type)
        tx_list.append(encoded_value)

    tx = encode_function(function_data["signatureHash"], tx_list)
    func_name = function_data["name"]
    func_args = printable_list
    return (tx, func_name, func_args)
