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
        self.sequent = None
        self.initializeCommandList()
        self.history = []

        splitter  = wx.SplitterWindow(self, style = wx.SP_NOBORDER)
        splitter.SetMinimumPaneSize(70)

        mainSizer = wx.BoxSizer(wx.VERTICAL)
        toolbar = wx.ToolBar(self, wx.ID_ANY, style=wx.TB_HORIZONTAL) # | wx.NO_BORDER)
        lb = wx.ComboBox(toolbar, wx.ID_ANY, pos=(50, 170), size=(150, -1), choices=self.commandList, style=wx.CB_READONLY)
        lb.SetToolTipString("My List of Commands")
        toolbar.AddControl(lb)
        
        undoButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('undo.gif'), shortHelpString="Undo the last command")
        postponeButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('rightarrow.png'), shortHelpString="Postpone the current subgoal")
        commentaryButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('commentary.png'), shortHelpString="Display the commentary box")
        toolbar.AddSeparator()
        quitButton = toolbar.AddTool(wx.ID_ANY, ui.images.getBitmap('quit.png'), shortHelpString="Quit the prover")
        mainSizer.Add(toolbar)

        self.outputTextCtrl = wx.TextCtrl(splitter, wx.ID_ANY, style=wx.TE_MULTILINE | wx.TE_READONLY | wx.TE_RICH | wx.TE_RICH2)

        bottomPanel = wx.Panel(splitter)
        bottomSizer = wx.BoxSizer(wx.VERTICAL)
        horizontalSizer = wx.BoxSizer(wx.HORIZONTAL)
        self.historyBox = wx.ComboBox(bottomPanel, wx.ID_ANY, choices=["history"], style=wx.CB_READONLY)
        self.commandTextControl = wx.TextCtrl(bottomPanel, wx.ID_ANY, "", style=wx.TE_MULTILINE)
        horizontalSizer.Add(wx.StaticText(bottomPanel, wx.ID_ANY, "Enter Rule:"), 1, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 0)
        horizontalSizer.Add(self.historyBox, 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 0)
        bottomSizer.Add(horizontalSizer, 0, wx.ALL | wx.EXPAND, 5)
        bottomSizer.Add(self.commandTextControl, 1, wx.ALL | wx.EXPAND, 5)
        self.historyBox.SetSelection(0)
        bottomPanel.SetSizer(bottomSizer)
        
        splitter.SplitHorizontally(self.outputTextCtrl, bottomPanel)
        splitter.SetSashPosition(300)
        mainSizer.Add(splitter, 1, wx.ALL | wx.EXPAND, 0)        
        self.SetSizer(mainSizer)
        
        toolbar.Realize()
        self.commentaryDialog = ui.logdlg.PVSCommunicationLogDialog(util.getMainFrame(), "Proof Commentary", constants.COMMENTARYLOG)

        #self.Bind(wx.EVT_TEXT_ENTER, self.onCommandEntered, self.commandTextControl)
        self.Bind(wx.EVT_TEXT, self.onCommand, self.commandTextControl)
        lb.Bind(wx.EVT_COMBOBOX, self.OnSelectCommand)
        self.historyBox.Bind(wx.EVT_COMBOBOX, self.OnSelectHistory)
        self.Bind(wx.EVT_TOOL, self.OnUndoLastCommand, undoButton)
        self.Bind(wx.EVT_TOOL, self.OnPostponeCommand, postponeButton)
        self.Bind(wx.EVT_TOOL, self.OnCommentaryButtonClicked, commentaryButton)
        self.Bind(wx.EVT_TOOL, self.OnQuitProver, quitButton)
        pub.subscribe(self.proofInformationReceived, constants.PUB_PROOFINFORMATIONRECEIVED)
        self.Layout()


    def initializeCommandList(self):
        self.commandList = config.PVSIDEConfiguration().proverCommands.keys()
        self.commandList.sort()
        
    def OnUndoLastCommand(self, event):
        pvscomm.PVSCommandManager().proofCommand("(undo)")
        event.Skip()

    def OnPostponeCommand(self, event):
        pvscomm.PVSCommandManager().proofCommand("(postpone)")
        event.Skip()

    def OnCommentaryButtonClicked(self, event):
        self.commentaryDialog.Show()
        event.Skip()

    def OnQuitProver(self, event):
        pvscomm.PVSCommandManager().proofCommand("(quit)")
        event.Skip()


    def OnSelectHistory(self, event):
        selection = event.GetSelection()
        if selection > 0:
            command = self.historyBox.GetString(selection)
            self.commandTextControl.SetValue(command)
        self.historyBox.SetSelection(0)
        event.Skip()

    def OnSelectCommand(self, event):
        item = event.GetSelection()
        command = self.commandList[item]
        self.commandTextControl.SetValue(config.PVSIDEConfiguration().proverCommands[command])

    def proofInformationReceived(self, information):
        assert information is not None
        if isinstance(information, str) or isinstance(information, unicode):
            logging.warn("information should be a dictionary and not a string")
            information = json.loads(information)
        logging.debug("information received: %s", information)
        result = information["result"] if "result" in information else None
        if result == "Q.E.D.":
            logging.info("Proof Completed")
            return
        elif result == "Unfinished":
            logging.info("Proof Unfinished")
            return
        commentary = information["commentary"] if "commentary" in information else None        
        action = information["action"] if "action" in information else None        
        nsubgoals = information["num_subgoals"] if "num_subgoals" in information else None
        jsequent = information["sequent"]
        self.sequent = Sequent(jsequent)
        label = information["label"]
        self.outputTextCtrl.Clear()
        if action is not None:
            self._appendTextToOut(action)
        if nsubgoals is None:
            self._appendTextToOut("no subgoals")            
        elif nsubgoals == 0:
            self._appendTextToOut("no subgoals")
        elif nsubgoals == 1:
            self._appendTextToOut("this simplifies to:")
        else:
            self._appendTextToOut("this yields %d subgoals"%nsubgoals)
        self._appendTextToOut(constants.EMPTY_STRING, newLine=True)
        self._appendTextToOut(str(self.sequent))
            
        if commentary is not None:
            lg = pvscomm.PVSCommunicationLogger()
            lg.log(constants.COMMENTARYLOG, commentary)

        self.commandTextControl.Clear()
        self.Layout()
        
    def _appendTextToOut(self, text, color=wx.BLACK, newLine=True):
        self.outputTextCtrl.SetDefaultStyle(wx.TextAttr(color))
        self.outputTextCtrl.AppendText(text)
        if newLine:
            self.outputTextCtrl.AppendText(constants.NEWLINE)

    def onCommandEntered(self, event):
        text = self.commandTextControl.GetValue()
        if text.endswith(constants.NEWLINE):
            command = text.strip()
            logging.info("Proof command entered: %s", command)
            pvscomm.PVSCommandManager().proofCommand(command)
            self.commandTextControl.Clear()
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
        names_info = item["names-info"] #TODO: This should be used for tooltip stuff
        slabels = ", ".join(labels)
        slabels = "{%s}"%slabels if changed else "[%s]"%slabels
        separator = "\n    " if len(labels) > 1 else "  "
        value = slabels + separator + str(formula) + "\n"
        return value   
        
        
    def getValue(self):
        return self.value
    
    def __str__(self):
        return str(self.value)
    
