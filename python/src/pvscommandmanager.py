import os.path
import config
import pvsrunner

log = config.getLogger(__name__)

PARSE = "parse"
TYPECHECK = "typecheck-file"
CHANGECONTEXT = "change-context"

def execute(callback, command, *parameters):
    config.runner.sendAsyncCommand(callback, command, *parameters)

def parse(filename):
    execute(onParse, PARSE, filename)

def onParse(request, result):
    log.info("onParse for %s returned %s", request, result)

def typecheck(filename):
    name = os.path.basename(filename)
    name = os.path.splitext(name)[0] # just get the filename without the extension 
    execute(onTypecheck, TYPECHECK, name)

def onTypecheck(request, result):
    log.info("onTypecheck for %s returned %s", request, result)
    name = request[pvsrunner.PVSRunner.PARAMETERS][0]
    execute(onGetTheories, "json-all-theories-info", name)
    
def onGetTheories(request, result):
    log.info("onGetTheories for %s returned %s", request, result)
    
def changeContext(newContext):
    execute(onChangeContext, CHANGECONTEXT, newContext)

def onChangeContext(request, result):
    log.info("onChangeContext for %s returned %s", request, result)