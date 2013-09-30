#!/usr/bin/env python
import wx
import logging
from ui.plugin import PluginPanel
from wx.lib.pubsub import setupkwargs, pub
import constants
import pvscomm
import json
import util
import config
import ui.images

class ProofManagerPlugin(PluginPanel):
    PROMPT = "Rule? "
    
    def __init__(self, parent, definition):
        PluginPanel.__init__(self, parent, definition)
        self.actionLabel = wx.StaticText(self, wx.ID_ANY, "Action: ")
        self.numberOfSubgoalsLabel = wx.StaticText(self, wx.ID_ANY, "Number of Subgoals: 0")
        self.labelLabel = wx.StaticText(self, wx.ID_ANY, "Label: ")
        self.resultLabel = wx.StaticText(self, wx.ID_ANY, "Result: ")
        self.sequentView = wx.TextCtrl(self, wx.ID_ANY, "", style=wx.TE_MULTILINE | wx.TE_READONLY | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        self.historyBox = wx.ComboBox(self, wx.ID_ANY, choices=["history"], style=wx.CB_READONLY)
        self.commandTextControl = wx.TextCtrl(self, wx.ID_ANY, "", style=wx.TE_MULTILINE)
        self.sequent = None
        self.initializeCommandList()
        self.history = []

        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        toolbar = wx.ToolBar(self, wx.ID_ANY, style=wx.TB_HORIZONTAL) # | wx.NO_BORDER)
        lb = wx.ComboBox(toolbar, wx.ID_ANY, pos=(50, 170), size=(150, -1), choices=self.commandList, style=wx.CB_READONLY)
        lb.SetToolTipString("My List of Commands")
        toolbar.AddControl(lb)
        
        undoButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('undo.gif'), shortHelpString="Undo the last command")
        quitButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('quit.png'), shortHelpString="Quit the prover")
        
        sizer_1.Add(toolbar)
        
        sizer_1.Add(self.actionLabel, 0, wx.ALL | wx.EXPAND, 5)
        sizer_1.Add(self.numberOfSubgoalsLabel, 0, wx.ALL | wx.EXPAND, 5)
        sizer_1.Add(self.labelLabel, 0, wx.ALL | wx.EXPAND, 5)
        sizer_1.Add(self.resultLabel, 0, wx.ALL | wx.EXPAND, 5)
        sizer_1.Add(self.sequentView, 2, wx.ALL | wx.EXPAND, 5)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_2.Add(wx.StaticText(self, wx.ID_ANY, "Enter Rule:"), 1, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 0)
        sizer_2.Add(self.historyBox, 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 0)
        sizer_1.Add(sizer_2, 0, wx.ALL | wx.EXPAND, 5)        
        sizer_1.Add(self.commandTextControl, 1, wx.ALL | wx.EXPAND, 5)
        self.SetSizer(sizer_1)
        self.Layout()
        
        toolbar.Realize()

        #self.Bind(wx.EVT_TEXT_ENTER, self.onCommandEntered, self.commandTextControl)
        self.Bind(wx.EVT_TEXT, self.onCommand, self.commandTextControl)
        lb.Bind(wx.EVT_COMBOBOX, self.OnSelectCommand)
        self.historyBox.Bind(wx.EVT_COMBOBOX, self.OnSelectHistory)
        self.Bind(wx.EVT_TOOL, self.OnUndoLastCommand, undoButton)
        self.Bind(wx.EVT_TOOL, self.OnQuitProver, quitButton)
        pub.subscribe(self.proofInformationReceived, constants.PUB_PROOFINFORMATIONRECEIVED)

    def initializeCommandList(self):
        self.commandList = config.PVSIDEConfiguration().proverCommands.keys()
        
    def OnUndoLastCommand(self, event):
        pvscomm.PVSCommandManager().proofCommand("(undo)")
        event.Skip()

    def OnQuitProver(self, event):
        pvscomm.PVSCommandManager().proofCommand("(quit)")
        event.Skip()


    def OnSelectHistory(self, event):
        selection = event.GetSelection()
        if selection > 0:
            command = self.historyBox.GetString(selection)
            self.commandTextControl.SetValue(command)
        elf.historyBox.SetSelection(0)
        event.Skip()

    def OnSelectCommand(self, event):
        item = event.GetSelection()
        command = self.commandList[item]
        self.commandTextControl.SetValue(config.PVSIDEConfiguration().proverCommands[command])

    def proofInformationReceived(self, information):
        assert information is not None
        logging.debug("information received: %s", information)
        result = information["result"] if "result" in information else None
        if result == "Q.E.D.":
            logging.info("Proof Completed")
            return
        elif result == "Unfinished":
            logging.info("Proof Unfinished")
            return
        action = information["action"] if "action" in information else None        
        nsubgoals = information["num_subgoals"] if "num_subgoals" in information else None
        jsequent = information["sequent"]
        self.sequent = Sequent(jsequent)
        label = information["label"]
        if action:
            self.actionLabel.SetLabel("Action: " + action)
        else:
            self.actionLabel.SetLabel("No Action")
        if result:
            self.resultLabel.SetLabel("Result: " + result)
        else:
            self.resultLabel.SetLabel("No Result")
        if nsubgoals:
            self.numberOfSubgoalsLabel.SetLabel("Number of Subgoals: " + str(nsubgoals))
        else:
            self.numberOfSubgoalsLabel.SetLabel("No Subgoals")
        self.labelLabel.SetLabel("Label: " + label)
        self.sequentView.SetValue(str(self.sequent))
        self.commandTextControl.SetValue("")

    def onCommandEntered(self, event):
        text = self.commandTextControl.GetValue()
        if text.endswith("\n"):
            command = text.strip()
            logging.info("Proof command entered: %s", command)
            pvscomm.PVSCommandManager().proofCommand(command)
            self.commandTextControl.SetValue("")
        event.Skip()

    def onCommand(self, event):
        text = self.commandTextControl.GetValue()
        if text.endswith("\n"):
            if util.isS_Expression(text):
                command = text.strip()
                self.executeCommand(command, True)
        event.Skip()
        
    def executeCommand(self, command, addToHistory=False):
        logging.info("Proof command entered: %s", command)
        pvscomm.PVSCommandManager().proofCommand(command)
        if addToHistory:
            self.history.append(command)
            self.historyBox.Insert(command, 1)
        self.commandTextControl.SetValue("")


class Sequent:
    ANTICIDENTS = "antecedents"
    SUCCEDENTS = "succedents"
    FORMULA = "formula"
    LABELS = "labels"
    CHANGED = "changed"
    SEPARATOR = "  |-------"
    
    def __init__(self, jsonSequent):
        self.value = jsonSequent
        self.anticidents = jsonSequent[Sequent.ANTICIDENTS] if Sequent.ANTICIDENTS in jsonSequent else []
        self.succedents = jsonSequent[Sequent.SUCCEDENTS] if Sequent.SUCCEDENTS in jsonSequent else []
        v = ""
        for ac in self.anticidents:
            v = v + self._getStringFor(ac)
        v = v + Sequent.SEPARATOR + "\n"
        for sc in self.succedents:
            v = v + self._getStringFor(sc)
        self.value = v
        
    def _getStringFor(self, item):
        formula = item[Sequent.FORMULA]
        labels = [str(i) for i in item[Sequent.LABELS]]
        changed = item[Sequent.CHANGED]
        slabels = ", ".join(labels)
        slabels = "[%s]"%slabels if changed else "{%s}"%slabels
        separator = "\n    " if len(labels) > 1 else "  "
        value = slabels + separator + str(formula) + "\n"
        return value   
        
        
    def getValue(self):
        return self.value
    
    def __str__(self):
        return str(self.value)
    
