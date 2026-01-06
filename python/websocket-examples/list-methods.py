#!/usr/bin/env python3
import asyncio
import websockets
import json

async def get_method_list():
  async with websockets.connect('ws://localhost:23456') as websocket:
    # Get the list of methods supported by PVS
    await websocket.send('{"jsonrpc":"2.0","id":"234","method":"list-methods"}')
    response_str = await websocket.recv()
    response = json.loads(response_str)
    methods = response["result"]
    for method in methods :
      print(method)

# asyncio.get_event_loop().run_until_complete(get_method_list())

if __name__ == "__main__":
  asyncio.run(get_method_list())
