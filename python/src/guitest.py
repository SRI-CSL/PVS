import pvscomm
import unittest
import config
import os.path

class PVSGUITestSuite(unittest.TestCase):

    def setUp(self):
        self.testContext = "/Users/saadati/projects/pvs/Examples"
        self.testPVSFile = "ustacks"
        self.typeCheckReturnValue = [{u'theory': {u'decls': [{u'importing': u'stacks[int]', u'kind': u'importing', u'place': [3, 1, 3, 22]}, {u'importing': u'stacks[stack[int]]', u'kind': u'importing', u'place': [3, 1, 3, 42]}, {u'place': [5, 1, 5, 17], u'kind': u'expr', u'type': u'stack[int]', u'id': u'si'}, {u'place': [6, 1, 6, 42], u'kind': u'expr', u'type': u'stack[stack[int]]', u'id': u'sos'}], u'id': u'ustacks'}}]
        self.SampleDeclInNamesInfo = {u'decl': u'stacks[t: TYPE+]: theory', u'decl-place': [2, 0, 26, 10], u'place': [3, 11, 3, 17], u'id': u'stacks', u'decl-file': u'/Users/saadati/projects/pvs/Examples/stacks.pvs'}

    def test_pvscalls(self):
        pm = pvscomm.PVSCommandManager()
        
        result = pm.ping()
        self.assertEqual(result, "3")
        
        result = pm.lisp("(* 7 5)")
        self.assertEqual(result, "35")
        
        result = pm.changeContext(self.testContext)
        self.assertEqual(result, self.testContext)
        
        result = pm.changeContext(self.testContext)
        self.assertEqual(result, self.testContext)

        result = pm.typecheck(self.testPVSFile)
        self.assertEqual(result, self.typeCheckReturnValue)

        result = pm.names_info(self.testPVSFile)
        # Since the result may be too long, we just check one sample decl to ensure it is in result:
        self.assertTrue(self.SampleDeclInNamesInfo in result)        

if __name__ == '__main__':
    utilDirectory = os.path.dirname(config.__file__)
    applicationFolder = os.path.abspath(os.path.join(utilDirectory, os.path.pardir))    
    config.PVSIDEConfiguration().initialize(applicationFolder)
    suite = unittest.TestLoader().loadTestsFromTestCase(PVSGUITestSuite)
    unittest.TextTestRunner(verbosity=2).run(suite)
    pvscomm.PVSCommunicator().shutdown()
    
