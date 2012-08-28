
# This class manages the execution of PVS and communicating with it

import threading, time
import subprocess, sys, wx
import config
import fcntl, os
#from eventhandler import *
from pvsresultevent import PVSResultEvent
from constants import PVS_MODE, EMPTY_STRING, PVS_MODE_OFF, PVS_MODE_LISP, PVS_MODE_PROVER, PVS_MODE_UNKNOWN
from constants import MESSAGE_INITIALIZE_CONSOLE, MESSAGE_PVS_STATUS, MESSAGE_CONSOLE_WRITE

log = config.getLogger(__name__)



class PVSRunner(threading.Thread):
    
    def __init__(self):
        threading.Thread.__init__(self)
        self.status = "off" # other possibilities: "lisp" and "prover"
        self.process = None
        
    def getPVSLocation(self):
        return "/Applications/pvs-5.0-ix86-MacOSX-allegro/"
    
    def getPVSStartingCommand(self):
        return (self.getPVSLocation() + "pvs", "-raw")
    
    def run(self):
        self.tellFrame(MESSAGE_INITIALIZE_CONSOLE)
        command = self.getPVSStartingCommand()
        log.debug("PVS Running command is %s", command)
        self.process = subprocess.Popen(command, shell = False, stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
        fcntl.fcntl(self.process.stdout.fileno(), fcntl.F_SETFL, os.O_NONBLOCK)
        self.status = PVS_MODE_LISP
        self.tellFrame(MESSAGE_PVS_STATUS, PVS_MODE_LISP)
        while self.process != None and self.process.poll() == None:
            try:
                text = self.process.stdout.read()
                self.tellFrame(MESSAGE_CONSOLE_WRITE, text)
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
        
        
    