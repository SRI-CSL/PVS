
# This class manages the GUI's console for PVS

import wx
import codecs
import util
from promptprocessor import isPrompt
import gui
from constants import NEWLINE, PVS_MODE, EMPTY_STRING, PVS_MODE_OFF, PVS_MODE_UNKNOWN, LABEL_PVS_CONSOLE

log = util.getLogger(__name__)

class PVSConsole(wx.Frame):
    """This class represents and manages the console.
    This console shows the PVS output, and the user can enter a line of input"""
    
    def __init__(self):
        """Initializes the console GUI"""
        wx.Frame.__init__(self, wx.GetApp().TopWindow, title=LABEL_PVS_CONSOLE)
        self.prompt = EMPTY_STRING
        self.history = []
        self.pvsout = wx.TextCtrl(self, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE | wx.TE_READONLY)
        self.pvsin = wx.TextCtrl(self, wx.ID_ANY, EMPTY_STRING, style=wx.TE_PROCESS_ENTER)
        
        consoleSizer = wx.BoxSizer(wx.VERTICAL)
        consoleSizer.Add(self.pvsout, 1, wx.EXPAND, 0)
        consoleSizer.Add(self.pvsin, 0, wx.EXPAND, 0)       
        self.SetSizer(consoleSizer)
        self.SetSize((700, 200))
        currentPosition = self.GetPositionTuple()
        self.SetPosition((currentPosition[0], currentPosition[1] + 425))
        self.Bind(wx.EVT_TEXT_ENTER, self.onPVSInTextEntered, self.pvsin)
        self.Bind(wx.EVT_TEXT, self.onPVSInText, self.pvsin)

    def clearOut(self):
        """Clears the PVS output text in the console"""
        self.pvsout.Clear()
        log.debug("Clearing pvsout")
        
    def clearIn(self):
        """Clears the PVS prompt box in the console"""
        self.pvsin.Clear()
        log.debug("Clearing pvsin")
        
    def initializeConsole(self):
        """Initializes PVS In and Out"""
        self.pvsout.Clear()
        self.pvsin.Clear()
        self.prompt = EMPTY_STRING
        self.history = []
        gui.manager.updateFrame(PVS_MODE_OFF)
        
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

    def onPVSInTextEntered(self, event):
        """This method is called whenever the user types something in the prompt and Enters."""
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

    def onPVSInText(self, event):
        """This method is called whenever PVS sends some text to the Editor"""
        st = event.GetString()
        log.info("Received: %s", st)
        if not st.startswith(self.prompt):
            self.clearIn()
            self.writeTextToIn(self.prompt)
        #event.Skip()
        

        
        