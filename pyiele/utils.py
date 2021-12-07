#!/usr/bin/python3
import sys
import json
from os import path
class colors:
    reset = '\033[0m'

    class fg:
        red = '\033[31m'
        green = '\033[32m'
        orange = '\033[33m'
        cyan = '\033[36m'

def notif(msg, color=colors.reset):
    sys.stderr.write(color + "== " + msg + '\n' + colors.reset)
    sys.stderr.flush()

def fatal(msg, code=1):
    notif(msg)
    sys.exit(code)

def remove_metadata_bin(contract_data):
    '''Removes the trailing metadata bytecode from the bytecode '''
    contract_bin = contract_data["bin"]
    metadata_bin = contract_data["metadata-bin"]
    if contract_bin.endswith(metadata_bin):
        contract_bin = contract_bin[:-len(metadata_bin)]
    return contract_bin

def get_file_name(file_path):
    '''Extracts only the name of a file from a path, ignoring its extension'''
    return path.splitext(path.basename(file_path))[0]