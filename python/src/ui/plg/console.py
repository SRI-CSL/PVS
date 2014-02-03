
# This class manages the GUI's console for PVS

import wx
import wx.richtext
import codecs
import logging
from constants import *
from wx.lib.pubsub import setupkwargs, pub 
from ui.plugin import PluginPanel
import pvscomm
import util
import evhdlr
import ui.images
import wx.lib.agw.aui as aui

class ConsolePlugin(PluginPanel):
    """This class represents and manages the console.
    This console shows the PVS output, and the user can enter a line of input"""
    
    def __init__(self, parent, definition):
        PluginPanel.__init__(self, parent, definition)
        commandManager = pvscomm.PVSCommandManager()

        toolbarInfo = [ \
                          ["module.png", "Parse the active file", commandManager.parse], \
                          ["typecheck16.png", "Typecheck the active file", commandManager.typecheck], \
        ]

        toolbar = wx.ToolBar(self, wx.ID_ANY, style = wx.TB_VERTICAL | wx.NO_BORDER)
        self.toolbarButton = {}
        for imageName, tooltipText, command in toolbarInfo:
            buttonID = wx.NewId()
            toolbar.AddTool(buttonID, ui.images.getBitmap(imageName))
            wx.EVT_TOOL(self, buttonID, self.onToolboxButtonClicked)
            self.toolbarButton[buttonID] = command        
        toolbar.Realize()



        splitter  = wx.SplitterWindow(self, style = wx.SP_NOBORDER)
        splitter.SetMinimumPaneSize(35)
        self.pvsout = wx.richtext.RichTextCtrl(splitter, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE | wx.TE_READONLY)

        belowPanel = wx.Panel(splitter)
        self.pvsin = wx.TextCtrl(belowPanel, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE)
        self.historyBox = wx.ComboBox(belowPanel, wx.ID_ANY, choices=[], style=wx.CB_READONLY)

        belowSizer = wx.BoxSizer(wx.HORIZONTAL)
        belowSizer.Add(self.pvsin, 5, wx.EXPAND | wx.UP | wx.DOWN | wx.ALIGN_CENTRE_VERTICAL, 5)
        belowSizer.Add(self.historyBox, 1, wx.EXPAND | wx.LEFT | wx.UP | wx.DOWN | wx.ALIGN_CENTRE_VERTICAL, 5)
        belowPanel.SetSizer(belowSizer)
        splitter.SplitHorizontally(self.pvsout, belowPanel)
        splitter.SetSashPosition(120)
        
        consoleSizer = wx.BoxSizer(wx.HORIZONTAL)
        consoleSizer.Add(splitter, 1, wx.EXPAND | wx.ALL, 5)
        consoleSizer.Add(toolbar, 0, wx.EXPAND | wx.ALL, 5)
        
        self.SetSizer(consoleSizer)

        self.Bind(wx.EVT_TEXT, self.onPVSInText, self.pvsin)
        self.historyBox.Bind(wx.EVT_COMBOBOX, self.OnSelectHistory)
        pub.subscribe(self.clearIn, PUB_CONSOLECLEARIN)
        pub.subscribe(self.initializeConsole, PUB_CONSOLEINITIALIZE)
        pub.subscribe(self.writeLine, PUB_CONSOLEWRITELINE)
        pub.subscribe(self.writePrompt, PUB_CONSOLEWRITEPROMPT)
        pub.subscribe(self.pvsModeUpdated, PUB_UPDATEPVSMODE)
        self.initializeConsole()

    def onToolboxButtonClicked(self, event):
        command = self.toolbarButton[event.GetId()]
        command()
        event.Skip()

    def OnSelectHistory(self, event):
        selection = event.GetSelection()
        if selection > 0:
            command = self.historyBox.GetString(selection)
            self.pvsin.SetValue(command)
        self.historyBox.SetSelection(0)
        event.Skip()

    def pvsModeUpdated(self, pvsMode=PVS_MODE_OFF):
        pass

    def clearOut(self):
        """Clears the PVS output text in the console"""
        self.pvsout.Clear()
        logging.debug("Clearing pvsout")
        
    def clearIn(self):
        """Clears the PVS prompt box in the console"""
        self.pvsin.Clear()
        logging.debug("Clearing pvsin")
                
    def initializeConsole(self):
        """Initializes PVS In and Out"""
        self.pvsout.Clear()
        self.pvsin.Clear()
        self.prompt = "pvs> "
        self.history = []
        self.historyBox.Clear()
        self.historyBox.Insert("history", 0)
        self.historyBox.SetSelection(0)
        #self.pvsModeUpdated(PVS_MODE_OFF)
        
    def appendToOut(self, text, newLine=False, color=None):
        logging.debug("Appending '%s' to pvsout", text)
        if color is not None:
            self.pvsout.BeginTextColour(color)
        self.pvsout.WriteText(text)
        if newLine:
            self.pvsout.Newline()
        if color is not None:
            self.pvsout.EndTextColour()
        
    def appendTextToIn(self, text):
        logging.debug("Appending '%s' to pvsin", text)
        self.pvsin.AppendText(text)
        
    def writeTextToIn(self, text):
        logging.debug("Writing '%s' to pvsin", text)
        self.pvsin.write(text)
        
    def writeLine(self, line):
        self.appendToOut(line)
        
    def writePrompt(self, prompt):
        self.clearIn()
        self.writeTextToIn(prompt)
        self.prompt = prompt
    
    def onPVSInText(self, event):
        """This method is called whenever PVS sends some text to the Editor"""
        #logging.info("Event handler `onPVSInTextEntered' not implemented")
        text = event.GetString()
        if text.endswith("\n"):
            if util.isS_Expression(text):
                command = text.strip()
                logging.info("Command is %s", command)
                self.appendToOut(self.prompt, color=wx.BLUE)
                self.appendToOut(command, newLine=True)
                self.clearIn()
                self.history.append(command)
                self.historyBox.Insert(command, 1)
                result = pvscomm.PVSCommandManager().lisp(command)
                if result is not None:
                    self.appendToOut(result, newLine=True)
        #event.Skip()
