#!/usr/bin/env python3

# A simple PVS REPL through a Websocket connection.

# Probably not very useful directly, as the Emacs/Ilisp interface has more
# features. However, this could be a starting point to defining an agent or
# GUI that controls PVS.

import asyncio
import websockets
import uuid
import json
import sys
import os
import subprocess

def find_running_pvs_url():
  """ Uses ss -lp (listening sockets, processes), searches for occurrences of pvs-sbclisp,
  and returns the url of the first one found (usually 127.0.0.1:port)
  """
  cmd = "ss -lp | grep pvs-sbclisp | tr -s '[:space:]' | cut -d ' ' -f 5"
  sockets = subprocess.check_output(cmd, shell=True).strip()
  if sockets :
    socket = sockets.splitlines()[0]
    url = 'ws://' + socket.decode("utf-8")
    return url
  else :
    print("No PVS websocket server found")
    print('Either start pvs with the "-port #" option')
    print('or use M-x start-pvs-server in a running PVS/Emacs')
    os._exit(1)

def request(id, method, params):
  """ Turns method, params into a json-rpc request
  """
  return {"jsonrpc":"2.0","id":id,"method":method,"params":params}

def valid_response(id, response):
  """A valid response to a request is either a 'result' or 'error' with the
  corresponding id.
  """
  return (type(response) == dict and 'id' in response and response['id'] == id
          and ('result' in response or 'error' in response))

async def pvs_lisp():
  url = find_running_pvs_url()
  print('Connecting to url = ', url)
  async with websockets.connect(url) as websocket:
    while True :
      try:
        sexpr = input('PVS> ')
      except EOFError:
        websockets.Close(1000, "")
        os._exit(0)
      id = uuid.uuid4().hex
      req = request(id, 'lisp', [sexpr])
      await websocket.send(json.dumps(req))
      response_str = await websocket.recv()
      response = json.loads(response_str)
      while not valid_response(id, response) :
        if ('method' in response and response['method'] == 'pvsMessage') :
          print(response['message'])
        else :
          print('Ignoring ', response)
        response_str = await websocket.recv()
        response = json.loads(response_str)
      if ('error' in response) :
        print('Error: ', response['error']['data'], response['error']['message'])
      else :
        print(response["result"])

if __name__ == "__main__":
  asyncio.run(pvs_lisp())
