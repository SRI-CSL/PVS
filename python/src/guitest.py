import pvscomm
import unittest
import config
import os.path
import constants
import logging

class PVSGUITestSuite(unittest.TestCase):

    def setUp(self):
        pass

    def _initialize(self):
        self.testContext = "/Users/saadati/projects/pvs/Examples"
        self.testPVSFile = "sum"
        self.tobeProved = ("sum", "closed_form")
        self.proofCommand = "(grind)"
        logging.getLogger(constants.LROOT).setLevel(logging.DEBUG)
        configDirectory = os.path.dirname(config.__file__)
        applicationFolder = os.path.abspath(os.path.join(configDirectory, os.path.pardir))    
        config.PVSIDEConfiguration().initialize(applicationFolder)

    def test_pvscalls(self):
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
            result = pm.changeContext(self.testContext)
            self.assertEqual(result, self.testContext)
            self.assertEqual(pm.pvsContext, self.testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
    
        def _testing_typecheck():
            result = pm.typecheck(self.testPVSFile)
            for theory in result:
                theoryInfo = theory[constants.LTHEORY]
                decls = theoryInfo[constants.DECLS]
                for decl in decls:
                    _verifyString(decl, constants.ID_L)
                    _verifyString(decl, constants.LKIND)
                    _verifyString(decl, constants.LTYPE, optional=True)
                    _verifyPlace(decl, constants.LPLACE)
            self.assertEqual(pm.pvsContext, self.testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
    
        def _testing_names_info():
            result = pm.namesInfo(self.testPVSFile)
            for inf in result:
                _verifyPlace(inf, constants.LPLACE)
                _verifyPlace(inf, constants.DECLPLACE)
                _verifyString(inf, constants.ID_L)
                _verifyString(inf, constants.DECLFILE)
                _verifyString(inf, constants.DECL)
            self.assertEqual(pm.pvsContext, self.testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_LISP)
            
        def _testing_start_prover():
            result = pm.startProver(*self.tobeProved)
            self.assertTrue("sequent" in result)
            self.assertTrue("label" in result)
            self.assertEqual(pm.pvsContext, self.testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)
    
        def _testing_proof_command():
            result = pm.proofCommand(self.proofCommand)
            self.assertEqual(pm.pvsContext, self.testContext)
            self.assertEqual(pm.pvsMode, constants.PVS_MODE_PROVER)
        
        self._initialize()
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
        self._clean()

    def _clean(self):
        pvscomm.PVSCommunicator().shutdown()

    
