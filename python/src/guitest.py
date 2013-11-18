import pvscomm
import unittest
import config
import os.path
import constants
import logging

class PVSGUITestSuite(unittest.TestCase):

    def setUp(self):
        pass

    def test_pvscalls(self):
        testContext = "/Users/saadati/projects/pvs/Examples"
        testPVSFile = "sum"
        tobeProved = ("sum", "closed_form")
        proofCommand = "(grind)"

        def _verifyPlace(_dict, key):
            self.assertTrue(key in _dict.keys())
            self.assertTrue(_dict[key] is None or len(_dict[key]) == 4)
            
        def _verifyString(_dict, thing, optional=False):
            if optional:
                if thing in _dict:
                    self.assertTrue(isinstance(_dict[thing], str) or isinstance(_dict[thing], unicode))
            else:            
                self.assertTrue(thing in _dict)
                self.assertTrue(isinstance(_dict[thing], str) or isinstance(_dict[thing], unicode))
            
        def _testing_ping():
            result = pm.ping()
            self.assertEqual(result, "3")
            
        def _testing_lisp():
            result = pm.lisp("(* 7 5)")
            self.assertEqual(result, "35")
            
        def _testing_change_context():
            result = pm.changeContext(testContext)
            self.assertEqual(result, testContext)
            self.assertEqual(pm.pvsContext, testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
    
        def _testing_typecheck():
            result = pm.typecheck(testPVSFile)
            for theory in result:
                theoryInfo = theory[constants.LTHEORY]
                decls = theoryInfo[constants.DECLS]
                for decl in decls:
                    _verifyString(decl, constants.ID_L)
                    _verifyString(decl, constants.LKIND)
                    _verifyString(decl, constants.LTYPE, optional=True)
                    _verifyPlace(decl, constants.LPLACE)
            self.assertEqual(pm.pvsContext, testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
    
        def _testing_names_info():
            result = pm.namesInfo(testPVSFile)
            for inf in result:
                _verifyPlace(inf, constants.LPLACE)
                _verifyPlace(inf, constants.DECLPLACE)
                _verifyString(inf, constants.ID_L)
                _verifyString(inf, constants.DECLFILE)
                _verifyString(inf, constants.DECL)
            self.assertEqual(pm.pvsContext, testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
            
        def _testing_start_prover():
            result = pm.startProver(*tobeProved)
            self.assertTrue("sequent" in result)
            self.assertTrue("label" in result)
            self.assertEqual(pm.pvsContext, testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)
    
        def _testing_proof_command():
            result = pm.proofCommand(proofCommand)
            self.assertEqual(pm.pvsContext, testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)
            
        pm = pvscomm.PVSCommandManager()
        theTests = [_testing_ping, \
                 _testing_lisp, \
                 _testing_change_context, \
                 _testing_typecheck, \
                 _testing_names_info, \
                 _testing_start_prover, \
                 _testing_proof_command, \
                 ]
        for aTest in theTests:
            print "Testing %s..."%(aTest.__name__)
            aTest()
        
    

if __name__ == '__main__':
    logging.getLogger(constants.LROOT).setLevel(logging.INFO)
    utilDirectory = os.path.dirname(config.__file__)
    applicationFolder = os.path.abspath(os.path.join(utilDirectory, os.path.pardir))    
    config.PVSIDEConfiguration().initialize(applicationFolder)
    suite = unittest.TestLoader().loadTestsFromTestCase(PVSGUITestSuite)
    unittest.TextTestRunner(verbosity=2).run(suite)
    pvscomm.PVSCommunicator().shutdown()
    
