#!/usr/bin/env python3
import argparse
import json
import sys
from pyiele import *
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
      contractbin = combinedjson["contracts"][contract]["bin"]
      metadatabin = combinedjson["contracts"][contract]["metadata-bin"]
      if contractbin.endswith(metadatabin):
        contractbin = contractbin[:-len(metadatabin)]
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

def make_parser():
  parser = argparse.ArgumentParser()
  parser.add_argument(type=str, metavar='<isolc combined-json>', dest='combined',
                      help='combined.json file generated by isolc with asm,srcmap options')
  parser.add_argument('-o', '--output', type=str, default='report.json', metavar='<output file>', dest='output',
                      help='Output file')
  parser.add_argument('-p', '--port', type=int, default=8546, metavar='<port>', dest='port',
                      help='Port number for RPC client')
  return parser

""" Parse command line arguments """
parser = make_parser()
args = parser.parse_args()
config.port = args.port

""" Start JSON report with locally available data """
data = []
with open(args.combined) as f:
  data = json.load(f)

try:
  output = create_local_data(data)
except KeyError as key:
  if key.args[0] in ['asm','bin','metadata-bin','srcmap']:
    print(f"""[ERROR] KeyError while reading "{args.combined}": {key} """)
    print("[ERROR] Make sure you compiled with '--combined-json asm,bin,metadata-bin,srcmap'")
    sys.exit(1)
  else:
    raise key

""" Get coverage from client """
retrieve_coverage(output, args.port)

with open(args.output, mode='w') as f:
  f.write(json.dumps(output, indent=2))
