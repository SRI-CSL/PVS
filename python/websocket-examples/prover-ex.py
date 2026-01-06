#!/usr/bin/env python3
import asyncio
import websockets
import uuid
import json
import os
import time

async def request(method, params, ws):
  """ Turns method, params into a json-rpc request
  """
  id = uuid.uuid4().hex
  req = {"jsonrpc":"2.0","id":id,"method":method,"params":params}
  await ws.send(json.dumps(req))
  response_str = await ws.recv()
  response = json.loads(response_str)
  while not valid_response(id, response) :
    print('response = ', response)
    response_str = await ws.recv()
    response = json.loads(response_str)
  if "result" in response:
    return response["result"]
  else:
    print('Error: ', response['error'])

def valid_response(id, response):
  """A valid response to a request is either a 'result' or 'error' with the
  corresponding id.
  """
  return (type(response) == dict and 'id' in response and response['id'] == id
          and ('result' in response or 'error' in response))

def print_proofstate(ps):
  print()
  print(ps["label"])
  print_sequent(ps["sequent"])

def print_sequent(seq):
  for sform in seq["antecedents"]:
    print_sform(sform)
  print('|-----')
  for sform in seq["succedents"]:
    print_sform(sform)

def print_sform(sform):
  print(sform["labels"], ' ', sform["formula"])

async def prove_ex():
  async with websockets.connect('ws://localhost:23456') as ws:
    path = await request('lisp', ['*pvs-path*'], ws)
    formula = path + "/Examples/sum#closed_form"
    ps1 = await request("prove-formula", [ formula ], ws)
    if ps1:
      prfid1 = ps1[0]["id"]
      print_proofstate(ps1[0])
      ps1_1 = await request("proof-command", [ prfid1, "(induct \"n\")" ], ws)
      if ps1_1:
        print_proofstate(ps1_1[0])
    ps2 = await request("prove-formula", [ formula ], ws)
    if ps2:
      prfid2 = ps2[0]["id"]
      print_proofstate(ps2[0])
      ps2_1 = await request("proof-command", [ prfid2, "(induct \"n\")" ], ws)
      if ps2_1:
        print_proofstate(ps2_1[0])
    qres = await request("quit-all-proof-sessions", [], ws)
    print('qres = ', qres)
    # os._exit(0)

if __name__ == "__main__":
  asyncio.run(prove_ex())
