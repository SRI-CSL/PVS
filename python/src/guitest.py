import pvscomm
import unittest
import config
import os.path
import constants

class PVSGUITestSuite(unittest.TestCase):

    def setUp(self):
        self.testContext = "/Users/saadati/projects/pvs/Examples"
        self.testPVSFile = "sum"
        self.typeCheckReturnValue = [{u'theory': {u'decls': [{u'kind': u'formula', u'place': None, u'id': u'sum_TCC1'}, {u'kind': u'formula', u'place': None, u'id': u'sum_TCC2'}, {u'place': [6, 1, 8, 12], u'kind': u'expr', u'type': u'[nat -> nat]', u'id': u'sum'}, {u'kind': u'formula', u'place': [10, 1, 10, 46], u'id': u'closed_form'}], u'id': u'sum'}}]
        self.SampleDeclInNamesInfo = {u'decl': u'nat: TYPE+ = naturalnumber', u'decl-place': [2253, 2, 2253, 36], u'place': [4, 8, 4, 11], u'id': u'nat', u'decl-file': u'/Users/saadati/projects/pvs/lib/prelude.pvs'}

    def test_pvscalls(self):
        pm = pvscomm.PVSCommandManager()
        
        result = pm.ping()
        #self.assertEqual(result, "3")
        
        result = pm.lisp("(* 7 5)")
        self.assertEqual(result, "35")
        
        result = pm.changeContext(self.testContext)
        self.assertEqual(result, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
        
        result = pm.changeContext(self.testContext)
        self.assertEqual(result, self.testContext)
        self.assertEqual(pm.pvsContext, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)

        result = pm.typecheck(self.testPVSFile)
        self.assertEqual(result, self.typeCheckReturnValue)
        self.assertEqual(pm.pvsContext, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)

        result = pm.namesInfo(self.testPVSFile)
        # Since the result may be too long, we just check one sample decl to ensure it is in result:
        self.assertTrue(self.SampleDeclInNamesInfo in result)
        self.assertEqual(pm.pvsContext, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
        
        result = pm.startProver("sum", "closed_form")
        self.assertTrue("sequent" in result)
        self.assertTrue("label" in result)
        self.assertEqual(pm.pvsContext, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)

        result = pm.proofCommand("(grind)")
        self.assertEqual(pm.pvsContext, self.testContext)
        self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)        
        print result
        
        

if __name__ == '__main__':
    utilDirectory = os.path.dirname(config.__file__)
    applicationFolder = os.path.abspath(os.path.join(utilDirectory, os.path.pardir))    
    config.PVSIDEConfiguration().initialize(applicationFolder)
    suite = unittest.TestLoader().loadTestsFromTestCase(PVSGUITestSuite)
    unittest.TextTestRunner(verbosity=2).run(suite)
    pvscomm.PVSCommunicator().shutdown()
    
