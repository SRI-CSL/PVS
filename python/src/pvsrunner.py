
# This class manages the execution of PVS and communicating with it

import threading, time, json
import subprocess, sys, wx
import config
import fcntl, os
#from eventhandler import *
from pvsresultevent import PVSResultEvent
from constants import PVS_MODE, EMPTY_STRING, PVS_MODE_OFF, PVS_MODE_EDIT, PVS_MODE_PROVER, NEWLINE
from constants import MESSAGE_INITIALIZE_CONSOLE, MESSAGE_PVS_STATUS, MESSAGE_CONSOLE_WRITE_LINE, MESSAGE_CONSOLE_WRITE_PROMPT
from promptprocessor import isPrompt
from pvscommandmanager import changeContext

log = config.getLogger(__name__)



class PVSRunner(threading.Thread):
    ID = "id"
    METHOD = "method"
    PARAMETERS = "params"
    RESULT = "result"
    ERROR = "error"
    RAWCOMMAND = "rawcommand"
    PVSJSONLISPCOMMAND = "pvs-json"
    PVSJSONPROVERCOMMAND = "pvs-prove"
    LCB = "{"
    RCB = "}"
        
    def __init__(self):
        threading.Thread.__init__(self)
        self.status = PVS_MODE_OFF # other possibilities: "Edit" and "Prover"
        self.process = None
        self.asyncCommands = []
        self.resetJSONBuffer()
    
    def getPVSStartingCommand(self):
        return (config.preference.getPVSLocation() + "pvs", "-raw")
    
    def run(self):
        self.tellFrame(MESSAGE_INITIALIZE_CONSOLE)
        command = self.getPVSStartingCommand()
        self.asyncCommands = []
        self.resetJSONBuffer()
        log.debug("PVS Running command is %s", command)
        self.process = subprocess.Popen(command, shell = False, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
        fcntl.fcntl(self.process.stdout.fileno(), fcntl.F_SETFL, os.O_NONBLOCK)
        self.status = PVS_MODE_EDIT
        self.tellFrame(MESSAGE_PVS_STATUS, PVS_MODE_EDIT)
        if config.preference.restoreContextAutomatically():
            time.sleep(1)
            context = config.preference.getRestoredContext()
            changeContext(context)              
        while self.process != None and self.process.poll() == None:
            try:
                text = self.process.stdout.read()
                self.onRawTextReceived(text)
            except IOError:
                pass
            time.sleep(0.01)
        log.debug("Out of the running loop for PVS")
        
            
    def tellPVS(self, text):
        if self.process != None:
            log.info("Telling PVS: %s", text)
            self.process.stdin.write(text)
            self.process.stdin.flush()
        else:
            log.error("PVS is not running")
            
    def tellFrame(self, message, data=None):
        wx.PostEvent(config.frame, PVSResultEvent(message, data))
    
    def terminate(self):
        if self.process != None:
            log.info("Terminating PVS")
            self.process.terminate()
            self.status = PVS_MODE_OFF
            self.tellFrame(MESSAGE_PVS_STATUS, PVS_MODE_OFF)
            self.process = None
            config.runner = None
            
            
    def createID(self):
        return "message_id_%s"%time.time()
            
    def sendRawCommand(self, message):
        return self.sendCommand(PVSRunner.RAWCOMMAND, message)
    
    def sendAsyncCommand(self, callback, command, *parameters, **keywords):
        message = {}
        theID = self.createID()
        message[PVSRunner.ID] = theID
        message[PVSRunner.METHOD] = command
        message[PVSRunner.PARAMETERS] = parameters
        for key, value in keywords.items():
            message[key] = value
        
        self.asyncCommands.append((message, callback))
        jMessage = json.dumps(message)
        self.sendAsyncJSON(callback, theID, jMessage)
        
    def sendAsyncJSON(self, callback, theID, jMessage):
        if self.status == PVS_MODE_EDIT:
            jsonCommand = PVSRunner.PVSJSONLISPCOMMAND
        elif self.status == PVS_MODE_PROVER:
            jsonCommand = PVSRunner.PVSJSONPROVERCOMMAND
        else:
            log.error("Sending message to a non-running PVS")
            return None
        
        modifiedObj = jMessage.replace("\\", "\\\\").replace("\"", "\\\"")
        pvsJSON = "(%s \"%s\")"%(jsonCommand, modifiedObj)
        log.info("Sending JSON message: %s", pvsJSON)
        self.tellPVS(pvsJSON)
            
    def resetJSONBuffer(self):
        self.jsonStarted = False
        self.jsonBuffer = ""
            
    def onRawTextReceived(self, text):
        lines = text.splitlines()
        for line in lines:
            if PVSRunner.LCB == line:
                if self.jsonStarted:
                    log.error("Got another {, but did not expect it")
                    self.resetJSONBuffer()
                else:
                    self.jsonStarted = True
                    self.jsonBuffer = self.jsonBuffer + line + NEWLINE
            elif PVSRunner.RCB == line:
                if not self.jsonStarted:
                    log.error("Got and }, but did not expect it")
                    self.resetJSONBuffer()
                else:
                    self.jsonBuffer = self.jsonBuffer + line + NEWLINE
                    log.debug("json object received: %s", self.jsonBuffer)
                    jObject = json.loads(self.jsonBuffer)
                    self.resetJSONBuffer()
                    self.onJSONObjectReceived(jObject)
            elif not self.jsonStarted:
                if isPrompt(line):
                    self.onPromptReceived(line)
                else:
                    if "nil" != line:
                        self.onRawLineReceived(line)
            else:
                self.jsonBuffer = self.jsonBuffer + line + NEWLINE

    def onJSONObjectReceived(self, jObject):
        log.debug("JSON message %s received", jObject)
        if jObject.has_key(PVSRunner.ID):
            theID = jObject[PVSRunner.ID]
            for message, callback in self.asyncCommands:
                mID = message[PVSRunner.ID]
                if theID == mID:
                    if jObject.has_key(PVSRunner.ERROR):
                        raise Exception(jObject[PVSRunner.ERROR])
                    if callback != None:
                        res = jObject[PVSRunner.RESULT]
                        res = json.loads(res)
                        callback(message, res)
                    self.asyncCommands.remove((message, callback))
                    return
            log.error("JSON message %s does not correspond to any message in the command queue", jObject)
        else:
            self.onJSONNotificationReceived(jObject)
        
    def onPromptReceived(self, prompt):
        log.debug("Prompt %s received", prompt)
        self.tellFrame(MESSAGE_CONSOLE_WRITE_PROMPT, prompt)

    def onRawLineReceived(self, line):
        log.debug("Line %s received", line)
        self.tellFrame(MESSAGE_CONSOLE_WRITE_LINE, line)
        
    def onJSONNotificationReceived(self, jObject):
        # Notifications are JSON messages without an ID. PVS sends them to tell something.
        log.debug("JSON notification %s received", jObject)
        
    