#!/usr/bin/python3

import sys
import os
import json
import re
from Crypto.Hash import keccak

def generate_signature(name:str, inputs:dict) -> str:
    args= "(" +','.join([x["type"] for x in inputs]) + ")"
    return name+args

def process_file(file: str) -> dict:
    result = []
    with open(file, "r") as in_file:
        in_dict = json.load(in_file)
        for (contract_key, contract_dict) in in_dict["contracts"].items():
            out_dict = {}
            contract_name = contract_key
            out_dict["contractName"] = contract_name
            out_dict["contractConstructor"] = [
                {
                    "name": contract_name,
                    "input": [
                        {"name": y["name"], "type": y["type"]} for y in x["inputs"]
                    ]
                }
                for x in contract_dict["abi"]
                if x["type"] == "constructor"
            ]
            out_dict["functionData"] = [
                {
                    "name": x["name"],
                    "signatureHash": generate_signature(x["name"], x["inputs"]),
                    "input": [
                        {"name": y["name"], "type": y["type"]} for y in x["inputs"]
                    ],
                    "output": [
                        {"name": y["name"], "type": y["type"]} for y in x["outputs"]
                    ],
                }
                for x in contract_dict["abi"]
                if x["type"] == "function"
            ]
            result.append(out_dict)
    return result


def process_folder(folder_path: str) -> list:
    result = []
    for file_name in os.listdir(folder_path):
        if False == os.path.isfile(os.path.join(folder_path, file_name)):
            continue
        result.append(process_file(os.path.join(folder_path, file_name)))
    return result


def fetch_function_data(arg: str) -> dict:
    if os.path.isdir(arg):
        result = process_folder(arg)
    elif os.path.isfile(arg):
        result = process_file(arg)
    else:
        raise Exception("Invalid input", arg)
        sys.exit(2)
    return result
