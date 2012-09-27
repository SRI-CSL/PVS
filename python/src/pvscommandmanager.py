import os.path
import config
import pvsrunner
from constants import FULLNAME, PVS_MODE_EDIT
import dialogs

log = config.getLogger(__name__)

PARSE = "parse"
TYPECHECK = "typecheck-file"
CHANGECONTEXT = "change-context"

def execute(callback, command, *parameters, **keywords):
    config.runner.sendAsyncCommand(callback, command, *parameters, **keywords)

def parse(filename):
    execute(onParse, PARSE, filename)

def onParse(request, result):
    log.info("onParse for %s returned %s", request, result)

def typecheck(filename):
    if config.runner == None or config.runner.status != PVS_MODE_EDIT:
        dialogs.showError("PVS is not running or it is in prover mode")
    elif filename == None:
        dialogs.showError("No file is open")
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
    config.filestreemanager.addTheoriesToFile(filename, result)
    
def changeContext(newContext):
    execute(onChangeContext, CHANGECONTEXT, newContext)

def onChangeContext(request, result):
    log.info("onChangeContext for %s returned %s", request, result)
    config.preference.setContext(result)

