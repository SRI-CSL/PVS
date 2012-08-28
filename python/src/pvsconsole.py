
# This class manages the GUI's console for PVS

import wx
import wx.stc as stc
import codecs
import config
from promptprocessor import isPrompt, hasPrompt
from constants import EMPTY_STRING, NEWLINE
from constants import PVS_MODE, EMPTY_STRING, PVS_MODE_OFF, PVS_MODE_LISP, PVS_MODE_PROVER, PVS_MODE_UNKNOWN

log = config.getLogger(__name__)

class PVSConsole(wx.Panel):
    def __init__(self, parent, ID):
        wx.Panel.__init__(self, parent, ID)
        self.prompt = EMPTY_STRING
        self.history = []
        self.pvsin = None
        self.pvsin = None
        
    def setPVSIn(self, i):
        self.pvsin = i
        
    def setPVSOut(self, o):
        self.pvsout = o

    def setBidnings(self):
        config.frame.Bind(wx.EVT_TEXT_ENTER, self.onPVSInTextEntered, self.pvsin)
        config.frame.Bind(wx.EVT_TEXT, self.onPVSInText, self.pvsin)

    def clearOut(self):
        self.pvsout.Clear()
        log.debug("Clearing pvsout")
        
    def clearIn(self):
        self.pvsin.Clear()
        log.debug("Clearing pvsin")
        
    def initializeConsole(self):
        #wx.MutexGuiEnter()
        self.pvsout.Clear()
        self.pvsin.Clear()
        #wx.MutexGuiLeave()
        self.prompt = EMPTY_STRING
        self.history = []
        self.updateFrame(PVS_MODE_OFF)
        
    def appendLineToOut(self, line):
        log.debug("Appending '%s' to pvsout", line)
        self.pvsout.AppendText(line + NEWLINE)
        
    def appendTextToOut(self, text):
        log.debug("Appending '%s' to pvsout", text)
        self.pvsout.AppendText(text)      
        
    def appendTextToIn(self, text):
        log.debug("Appending '%s' to pvsin", text)
        self.pvsin.AppendText(text)
        
    def writeTextToIn(self, text):
        log.debug("Writing '%s' to pvsin", text)
        self.pvsin.WriteText(text)
        
    def writeLine(self, line):
        if isPrompt(line):
            self.writeTextToIn(line)
            self.onPromptReceived(line)
        else:
            self.appendLineToOut(line)
        
    def write(self, text):
        if text == None: return
        lines = text.splitlines()      
        #wx.MutexGuiEnter()
        for line in lines:
            self.writeLine(line)  
        #wx.MutexGuiLeave()
        
    def onPromptReceived(self, prompt):
        self.prompt = prompt
        log.info("Prompt received: %s", prompt)
        
    def updateFrame(self, status = PVS_MODE_UNKNOWN):
        log.info("Updating frame with PVS status as %s", status)
        config.frame.setStatusbarText(PVS_MODE + status)
        if status == PVS_MODE_OFF:
            self.clearIn()
            self.pvsin.SetEditable(False)
        else:
            self.pvsin.SetEditable(True)

    def onPVSInTextEntered(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleTextEntered' not implemented")
        whole = event.GetString()
        pl = len(self.prompt)
        command = whole[pl:]
        log.info("Command is %s", command)
        if config.pvsrunner != None:
            self.appendLineToOut(whole)
            self.clearIn()
            self.history.append(command)
            config.pvsrunner.tellPVS(command + NEWLINE)
        #event.Skip()

    def onPVSInText(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        st = event.GetString()
        log.info("Received: %s", st)
        if not st.startswith(self.prompt):
            self.clearIn()
            self.writeTextToIn(self.prompt)
        #event.Skip()
        

        
        