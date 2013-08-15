"""
This is a simple client for the PVS XML-RPC connection

The basic idea is that your run PVS, which starts an XML-RPC server on some
port.  A client connects, sets up a server and sends its server port.

The idea is that there are two connections, to essentially implement
JSON-RPC, which has an implementation in Python, but does not seem to
in Lisp (though there is undocumented JSON-RPC in CL-JSON).

The advantage of using XML-RPC is that it is ubiquitous. 
"""

import sys
import os
import wx
import json
import xmlrpclib
import threading
from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class XMLRpcError(Error):
    """Exception raised for errors in the JSON input."""
    
    def __init__(self, expr, msg):
        self.expr = expr
        self.msg = msg

class MethodError(Error):
    """Exception raised for errors in executing the method."""
    def __init__(self, request):
        self.request = request

# Restrict to a particular path.
class RequestHandler(SimpleXMLRPCRequestHandler):
    rpc_paths = ('/RPC2',)


class PVS_XMLRPC(object):

    def __init__(self, host='localhost', port=22335):
        # Create server
        print 'Initializing gui_server with ({0}, {1})'.format(host,port)
        self.ctr = 0
        self.gui_url = 'http://{0}:{1}/RPC2'.format(host, port)
        self.gui_server = SimpleXMLRPCServer((host, port),
                                             requestHandler=RequestHandler)
        self.gui_server.register_function(self.request, 'request')
        self.server_thread = threading.Thread(target=self.gui_server.serve_forever)
        self.server_thread.start()
        self.pvs_proxy = xmlrpclib.ServerProxy('http://localhost:22334')
        self.json_methods = {'debug': self.pvs_debug,
                             'info': self.pvs_info,
                             'warning': self.pvs_warning,
                             'yes-no': self.pvs_yes_no,
                             'dialog': self.pvs_dialog}

    def pvs_request(self, method, params=None):
        """ Send a request to PVS """
        reqid = 'testgui_{0}'.format(self.ctr)
        self.ctr += 1
        if params is None:
            request = {'method': method, 'id': reqid}
        else:
            request = {'method': method, 'params': params, 'id': reqid}
        result_str = self.pvs_proxy.pvs.request(json.dumps(request), self.gui_url)
        # print 'result_str = {0}'.format(result_str)
        result = json.loads(result_str)
        return result

    def request(self, json_string):
        """
        Process a request from PVS
        
        An XML-RPC request presumably from PVS - typically a JSON-RPC
        message without id.  This always returns a value at the XML-RPC
        level.  If the JSON-RPC message cannot be parsed, or some other
        problem happens at the XML-RPC level, then it returns an error
        string.  If the JSON-RPC message is parsed, but doesn't include an
        id, then if an error occurs it is returned as a string.  If no id
        and no error, "null" is returned.  Otherwise, a valid JSON-RPC
        response/error form is returned.
        """
        print('\nrequest = {0}\n'.format(json_string))
        try:
            request = json.loads(json_string, object_hook=self.request_check)
            return self.process_request(request)
        except TypeError as err:
            return 'request: {0} is of type {1}, string expected'.format(
                json_string, type(json_string))
        except ValueError as err:
            return 'request: {0} is invalid - {1}'.format(json_string, err)
        except XMLRpcError as err:
            # Can't give normal JSON-RPC error response,
            # This is just an XML-RPC answer.
            return 'request: {0} is invalid - {1}'.format(json_string, err)

    def process_request(self, request):
        """
        Processes the json-rpc request
        """
        method = request['method']
        if 'id' in request:
            id = request['id']
        else:
            id = None
        if 'params' in request:
            params = request['params']
        else:
            params = []
        if method in self.json_methods:
            try:
                print '\nCalling method {}\n'.format(method)
                mthd = self.json_methods[method]
                result = self.json_methods[method](*params)
                if id is not None:
                    return json.dumps({'jsonrpc_result': {'id': id, 'result': result, 'jsonrpc': "2.0"}})
                else:
                    return "OK"
            except Exception as err:
                errmsg = ('Error in handling request: {0}\n  {1}'
                          .format(request, err))
                if id is None:
                    # No json-rpc return, but still have xml-rpc level
                    print(errmsg)
                    return errmsg
                else:
                    return json.dumps({'id': id, 'error': errmsg})
        else:
            errmsg = "method '{0}' not handled".format(method)
            if id is None:
                # No json-rpc return, but still have xml-rpc level
                print(errmsg)
                return errmsg
            else:
                return json.dumps({'id': id, 'error': errmsg})

    def request_check(self, dct):
        """
        Checks that the request is a JSON object (i.e., dictionary)
        with a method, jsonrpc of '2.0', optional id and params, and
        nothing else.
        """
        if not isinstance(dct, dict):
            raise XMLRpcError(dct, 'Request must be a JSON object')
        if 'jsonrpc' not in dct or dct['jsonrpc'] != '2.0':
            raise XMLRpcError(dct, 'Request must include "jsonrpc": "2.0"')
        if 'method' not in dct:
            raise XMLRpcError(dct, 'Request must include a method')
        if not all(k in ['jsonrpc', 'method', 'id', 'params'] for k in dct.keys()):
            raise XMLRpcError(dct, 'Request must only have "jsonrpc", "method", "id", and "params" fields')
        return dct

    def pvs_debug(self, msg):
        """ JSON-RPC method """
        print('DEBUG: {0}'.format(msg))

    def pvs_info(self, msg):
        """ JSON-RPC method """
        print('INFO: {0}'.format(msg))

    def pvs_warning(self, msg):
        """ JSON-RPC method """
        print('WARNING: {0}'.format(msg))

    def pvs_yes_no(self, prompt, fullp, timeout):
        """ JSON-RPC method """
        print('yes-no: {0} {1} {2}'.format(prompt, fullp, timeout))
        answer = raw_input(prompt)
        while answer != "yes" and answer != "no":
            answer = raw_input('Please answer yes or no: ')
        if answer.lower() == "yes":
            return True
        else:
            return False

    def pvs_dialog(self, prompt):
        print('dialog: {0}'.format(prompt))
        answer = raw_input(prompt)
        return answer
        

# class MyFrame(wx.Frame):
#   def YesNo(self, question, caption = 'Yes or no?'):
#     dlg = wx.MessageDialog(None, question, caption, wx.YES_NO | wx.ICON_QUESTION)
#     result = dlg.ShowModal() == wx.ID_YES
#     dlg.Destroy()
#     self.Destroy()
#     print result

# class MyApp(wx.App):
#   def OnInit(self):
#     myframe = MyFrame(None)
#     myframe.YesNo('foo?')
#     return True

# app = MyApp(0)
# app.MainLoop()
        
def main():
    ''' Starts the server, runs tests '''
    
    server.serve_forever()

if __name__ == '__main__':
    main()
