import os.path
import util
import runr
from constants import FULLNAME, PVS_MODE_EDIT, PVS_MODE_PROVER
import gui

log = util.getLogger(__name__)

PARSE = "parse"
TYPECHECK = "typecheck-file"
CHANGECONTEXT = "change-context"
PVSJSONPROVEFORMULA = "json-prove-formula"
INPROVER = "in_prover"

PVS_NOT_RUNNING_MESSAGE ="PVS is not running or it is in prover mode"

def execute(callback, command, *parameters, **keywords):
    """execute a given command with parameters and optional extra keywords"""
    util.runner.sendAsyncCommand(callback, command, *parameters, **keywords)

def parse(filename):
    """execute the command 'parse'"""
    execute(onParse, PARSE, filename)

def onParse(request, result):
    """called when the pvs response is received for the command 'parse'"""
    log.info("onParse for %s returned %s", request, result)

def typecheck(filename):
    """execute the command 'typecheck'"""
    if util.runner == None or util.runner.status != PVS_MODE_EDIT:
        gui.manager.showError(PVS_NOT_RUNNING_MESSAGE)
    elif filename == None:
        gui.manager.showError("No file is open")
    else:
        name = os.path.basename(filename)
        name = os.path.splitext(name)[0] # just get the filename without the extension 
        execute(onTypecheck, TYPECHECK, name, fullname=filename)

def onTypecheck(request, result):
    """called when the pvs response is received for the command 'typecheck'"""
    log.info("onTypecheck for %s returned %s", request, result)
    name = request[runr.PVSRunner.PARAMETERS][0]
    filename = request[FULLNAME]
    execute(onGetTheories, "json-all-theories-info", name, fullname=filename)
    
def onGetTheories(request, result):
    log.info("onGetTheories for %s returned %s", request, result)
    filename = request[FULLNAME]
    gui.manager.filesTreeManager.addTheoriesToFile(filename, result)
    
def changeContext(newContext):
    """execute the command 'change-context'"""
    execute(onChangedContext, CHANGECONTEXT, newContext)

def onChangedContext(request, result):
    log.info("onChangeContext for %s returned %s", request, result)

def startProver(theory, theorem):
    if util.runner == None or util.runner.status != PVS_MODE_EDIT:
        gui.manager.showError(PVS_NOT_RUNNING_MESSAGE)
    elif theory == None or theorem == None:
        gui.manager.showError("Theory or Theorem is not specified")
    else:
        execute(onProverStarted, PVSJSONPROVEFORMULA, theory, theorem)
    
def onProverStarted(request, result):
    log.info("onProverStarted for %s returned %s", request, result)
    if result.has_key[INPROVER] and result[INPROVER]==True:
        util.runner.setStatus(PVS_MODE_PROVER)
        log.info("Prover started for %s", request)

        
