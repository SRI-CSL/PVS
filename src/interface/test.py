#! /usr/bin/env python3

from unittest import TestCase, main
import xmlrpc_test
import random
import os

class TestPVS(TestCase):
  # This class variable creates the connection
  # myport and pvsport can be given, default to 22335 and 22334
  pvs_rpc = xmlrpc_test.PVS_XMLRPC(myport='')
  def request(self, method, args, debug=False):
    if (debug):
      print('Invoking {0} {1}'.format(method, args))
    result = TestPVS.pvs_rpc.pvs_request(method, args)
    if (debug):
      print('result {0}'.format(result))
    if not isinstance(result, dict):
      print('Expected dict - result type {0}: {1}'.format(type(result), result))
    if 'error' in result:
      print('Found error: {0}'.format(result))
      errmsg = result['error']['message']
      # errdata = {error_file: filename, theory: thname, place: {:begin (r, c)}}
      #    thname and place are optional
      print('Error: {0}\n'.format(errmsg))
      if 'data' in result['error']:
        errdata = result['error']['data']
        errfile = errdata['error_file']
        with open (errfile,'r') as ef:
          for line in ef:
            print("  {0}".format(line))
      return 
    elif 'result' in result:
      # Assuming pvs-json-response -> pvs-json-result -> result
      return result['result']
    else:
      return result

  def test_list_methods(self):
    expected = ['change-context', 'help', 'lisp', 'list-client-methods',
                'list-methods', 'names-info', 'proof-command',
                'prove-formula', 'reset', 'typecheck']
    result = self.request('list-methods', [])
    self.assertEqual(result, expected)

  def test_hooks(self):
    result = self.request('lisp', ['(pvs-message "foo")'])
    result = self.request('lisp', ['(pvs-warning "bar")'])
    result = self.request('lisp', ['(pvs-error "foo" "bar")'])
    result = self.request('lisp', ['(pvs-buffer "buf" "Put this in your buffer\nand smoke it")'])
    # result = self.request('lisp', ['(pvs-y-or-n-p "Raise the bar? (y or n) " nil nil)'])
    # result = self.request('lisp', ['(pvs-query )'])
    # result = self.request('lisp', ['(pvs-dialog )'])
    
  def test_change_context(self):
    result = self.request('change-context', ['/home/owre/pvs-specs'], debug=False)
    self.assertEqual(result, '~/pvs-specs/')

  def test_help(self):
    expected = 'Get help for the specified methodname -\n   provides the docstring and the argument spec'
    result = self.request('help', ['help'])
    self.assertEqual(result['docstring'], expected)

  def test_lisp(self):
    result = self.request('lisp', ['(+ 2 3)'])
    
  def test_list_client_methods(self):
    result = self.request('list-client-methods', [])
      
  def test_list_methods(self):
    result = self.request('list-methods', [])
    
  def test_names_info(self):
    result = self.request('names-info', ['sqrt'])

  def test_basic_pvs(self):
    result = self.request('lisp', ['(setq *output-proofstate-p* t)'])
    result = self.request('lisp', ['(setq *proceed-without-asking* t)'])
    self.assertEqual(result, 'noquestions')
    
    cdir = os.path.dirname(os.path.abspath(__file__))
    result = self.request('change-context', [cdir])
    self.assertEqual(os.path.expanduser(result), cdir + '/')
    
    #result = self.request('typecheck', ['sqrt'])
    result = self.request('typecheck', ['sqrt'])
    self.assertEqual(result[0]['id'], 'sqrt')

    result = self.request('prove-formula', ['sqrt_div', 'sqrt'])
    self.assertEqual(result['label'], 'sqrt_div')
    
    result = self.request('proof-command', ['(grind)'])
    self.assertEqual(result['num-subgoals'], 1)
    
    result = self.request('proof-command', ['(quit)'])

  def test_reset(self):
    result = self.request('reset', [])

if __name__ == '__main__':
  main()
