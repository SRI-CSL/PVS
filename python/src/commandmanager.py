import os.path
import common
import pvsrunner
from constants import FULLNAME, PVS_MODE_EDIT, PVS_MODE_PROVER
import ui.dialogs

log = common.getLogger(__name__)

PARSE = "parse"
TYPECHECK = "typecheck-file"
CHANGECONTEXT = "change-context"
PVSJSONPROVEFORMULA = "json-prove-formula"
INPROVER = "in_prover"

PVS_NOT_RUNNING_MESSAGE ="PVS is not running or it is in prover mode"

def execute(callback, command, *parameters, **keywords):
    common.runner.sendAsyncCommand(callback, command, *parameters, **keywords)

def parse(filename):
    execute(onParse, PARSE, filename)

def onParse(request, result):
    log.info("onParse for %s returned %s", request, result)

def typecheck(filename):
    if common.runner == None or common.runner.status != PVS_MODE_EDIT:
        ui.dialogs.showError(PVS_NOT_RUNNING_MESSAGE)
    elif filename == None:
        ui.dialogs.showError("No file is open")
    else:
        name = os.path.basename(filename)
        name = os.path.splitext(name)[0] # just get the filename without the extension 
        execute(onTypecheck, TYPECHECK, name, fullname=filename)

def onTypecheck(request, result):
    log.info("onTypecheck for %s returned %s", request, result)
    name = request[pvsrunner.PVSRunner.PARAMETERS][0]
    filename = request[FULLNAME]
    execute(onGetTheories, "json-all-theories-info", name, fullname=filename)
    
def onGetTheories(request, result):
    log.info("onGetTheories for %s returned %s", request, result)
    filename = request[FULLNAME]
    common.filestreemanager.addTheoriesToFile(filename, result)
    
def changeContext(newContext):
    execute(onChangeContext, CHANGECONTEXT, newContext)

def onChangeContext(request, result):
    log.info("onChangeContext for %s returned %s", request, result)
    common.preference.setContext(result)

def startProver(theory, theorem):
    if common.runner == None or common.runner.status != PVS_MODE_EDIT:
        ui.dialogs.showError(PVS_NOT_RUNNING_MESSAGE)
    elif theory == None or theorem == None:
        ui.dialogs.showError("Theory or Theorem is not specified")
    else:
        execute(onProverStarted, PVSJSONPROVEFORMULA, theory, theorem)
    
def onProverStarted(request, result):
    log.info("onProverStarted for %s returned %s", request, result)
    if result.has_key[INPROVER] and result[INPROVER]==True:
        common.runner.setStatus(PVS_MODE_PROVER)
        log.info("Prover started for %s", request)

        
