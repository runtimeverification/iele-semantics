#!/usr/bin/env python3
import argparse
import json
import sys
from .testrunner import send
from .rpc import firefly_getCoverage
from .utils import remove_metadata_bin
from Crypto.Hash import keccak
from subprocess import Popen, PIPE

def create_local_data(combinedjson):
  """ Reads a combined.json dictionary from isolc and formats a json for the coverage report """

  output = {}
  """
  Gather source texts for each source file
  """
  for srcname in combinedjson["sourceList"]:
    srcdata = output[srcname] = {}
    with open(srcname) as f:
      srcdata["solsrc"] = f.read()
    srcdata["contracts"] = {}
    asm = ""
    for contract, v in (x for x in combinedjson["contracts"].items() if x[0].startswith(srcname)):
      """
      isolc generates iele assembly for each contract.
      We're only interested in the assembly for the "main" contract, which will have the longest length.
      """
      if len(v["asm"]["code"]) > len(asm):
        asm = v["asm"]["code"]

    srcdata["ielesrc"] = asm

  """
  Gather contract information (codehash and sourcemaps) for each source file
  """
  for srcname, v in output.items():
    s = Popen(["iele-assemble", "--sourceMap", "-"], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    ielemaps = s.communicate(input=bytes(v["ielesrc"],"utf-8"))[0].decode("utf-8").splitlines()

    for contract, srcmap in [x.split(',') for x in ielemaps if x.split(',')[0].startswith(srcname)]:
      contractbin = remove_metadata_bin(combinedjson["contracts"][contract])
      codehash = keccak.new(digest_bits=256)
      codehash.update(bytes.fromhex(contractbin))

      v["contracts"][contract] = {}
      v["contracts"][contract]["hash"]    = codehash.hexdigest()
      v["contracts"][contract]["name"]    = contract
      v["contracts"][contract]["solmap"]  = combinedjson["contracts"][contract]["srcmap"]
      v["contracts"][contract]["ielemap"] = srcmap

  """
  Key the contracts by their bytecode hash instead of their name
  """
  for _, srcinfo in output.items():
    for contract in list(srcinfo["contracts"].keys()):
      codehash = "0x" + srcinfo["contracts"][contract].pop("hash")
      srcinfo["contracts"][codehash] = srcinfo["contracts"].pop(contract)

  return output

def retrieve_coverage(report, port):
  """ Retrieve coverage from RPC client and update the report JSON """
  result = send(firefly_getCoverage())
  if result == None:
    return

  """ Insert the coverage data into the JSON report """
  for codehash, coveragedata in result.items():
    for _, data in report.items():
      if codehash in data["contracts"]:
        data["contracts"][codehash]["coverage"] = coveragedata

  return
