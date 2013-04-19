
# This class manages the GUI's console for PVS

import wx
import codecs
import util
from promptprocessor import isPrompt
from constants import NEWLINE, PVS_MODE, EMPTY_STRING, PVS_MODE_OFF, PVS_MODE_UNKNOWN

log = util.getLogger(__name__)

class PVSConsole(wx.Panel):
    """This class represents and manages the console.
    This console shows the PVS output, and the user can enter a line of input"""
    
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
        util.frame.Bind(wx.EVT_TEXT_ENTER, self.onPVSInTextEntered, self.pvsin)
        util.frame.Bind(wx.EVT_TEXT, self.onPVSInText, self.pvsin)

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
        util.frame.updateFrame(PVS_MODE_OFF)
        
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
        self.pvsin.write(text)
        
    def writeLine(self, line):
        self.appendLineToOut(line)
        
    def writePrompt(self, prompt):
        self.clearIn()
        self.writeTextToIn(prompt)
        self.prompt = prompt

    def onPVSInTextEntered(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleTextEntered' not implemented")
        whole = event.GetString()
        pl = len(self.prompt)
        command = whole[pl:]
        log.info("Command is %s", command)
        if util.runner != None:
            self.appendLineToOut(whole)
            self.clearIn()
            self.history.append(command)
            util.runner.tellPVS(command + NEWLINE)
        #event.Skip()

    def onPVSInText(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        st = event.GetString()
        log.info("Received: %s", st)
        if not st.startswith(self.prompt):
            self.clearIn()
            self.writeTextToIn(self.prompt)
        #event.Skip()
        

        
        