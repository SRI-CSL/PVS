# -*- coding: US-ASCII -*-
#
# This class represents the main frame of the editor
#GetTopWindow

import wx
import os.path
from constants import *
from wx.lib.pubsub import setupkwargs, pub 
from remgr import RichEditorManager
import logging
import evhdlr
import sys
from mmgr import MainFrameMenu
from preference import Preferences
import wx.lib.agw.aui as aui
from config import PVSIDEConfiguration
import wx.stc as stc
from pvscomm import PVSCommunicator, PVSCommandManager            

class MainFrame(wx.Frame):
    """The main frame of the application. It consists of a menu and a toolbar, a notebook for all the open
    files and buffers, and a console"""
    
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.ICONIZE | wx.CAPTION | wx.MINIMIZE | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.MAXIMIZE_BOX | wx.SYSTEM_MENU | wx.CLIP_CHILDREN | wx.RESIZE_BORDER
        wx.Frame.__init__(self, *args, **kwds)
        self.auiManager = aui.AuiManager()
        self.auiManager.SetManagedWindow(self)        
        
        preferences = Preferences()
        preferences.loadPreferences()
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        #self.Bind(wx.EVT_IDLE, self.OnIdle)
        self.auiManager.Bind(aui.EVT_AUI_PANE_CLOSE, self.OnPanelClose)
        
        self.statusbar = self.CreateStatusBar(2)
        self.statusbar.SetStatusWidths([-1, -1])

        # Menu Bar
        self.menubar = MainFrameMenu()
        self.SetMenuBar(self.menubar)
        # Menu Bar end
        
        self.mainPanel = wx.Panel(self, wx.ID_ANY)
        
        notebook = aui.AuiNotebook(self)
        notebook.SetArtProvider(aui.ChromeTabArt())        
        RichEditorManager().setNotebook(notebook)

        self.__do_layout()
        self.SetTitle(MAINFRAME)
        pub.sendMessage(PUB_CONSOLEINITIALIZE)
        pub.subscribe(self.handlePVSModeUpdated, PUB_UPDATEPVSMODE)
        pub.subscribe(self.handlePVSContextUpdated, PUB_UPDATEPVSCONTEXT)
        pub.subscribe(self.handleNumberOfOpenFilesChanged, PUB_NUMBEROFOPENFILESCHANGED)
        pub.subscribe(self.setStatusbarText, PUB_UPDATESTATUSBAR)
        #self.Connect(-1, -1, util.EVT_RESULT_ID, self.onPVSResult)

    def __do_layout(self):
        cfg = PVSIDEConfiguration()
        pref = Preferences()
        self.SetSize(pref.getLastFrameSize())
        self.SetMinSize(cfg.ideMinumumSize) # Setting the minimum size of the main frame
        self.auiManager.AddPane(RichEditorManager().notebook, aui.AuiPaneInfo().CenterPane())
        self.auiManager.Update()
        #self.Layout()
        self.Centre()
        
    def OnClose(self, event):
        """called when self.Close() is called"""
        rmgr = RichEditorManager()
        if rmgr.ensureFilesAreSavedToPoceed():
            preferences = Preferences()
            preferences.setLastFrameSize(self.GetSize())
            openFiles = rmgr.getOpenFileNames()
            preferences.setListOfOpenFiles(openFiles)
            preferences.savePreferences()
            self.auiManager.UnInit()
            PVSCommunicator().shutdown()            
            wx.GetApp().ExitMainLoop()

    def OnPanelClose(self, event):
        """called after the panel is added to the frame"""
        paneInfo = event.GetPane()
        name = paneInfo.name
        logging.info("Pane %s was closed", name)
        pub.sendMessage(PUB_SHOWPLUGIN, name=name, value=False)

    def restoreOpenFiles(self):
        """Load .pvseditor and open all the files that were open last time"""
        preferences = Preferences()
        fullnames = preferences.listOfOpenFiles()
        self.openFiles(fullnames)
        self.handleNumberOfOpenFilesChanged()
        
    def setStatusbarText(self, text, location=0):
        logging.info("Setting status bar[%d] to: %s"%(location, text))
        self.statusbar.SetStatusText(text, location)
        
    def openFiles(self, fullnames):
        for fullname in fullnames:
            if os.path.exists(fullname):
                pub.sendMessage(PUB_ADDFILE, fullname=fullname)
            else:
                logging.warning("File %s no longer exists", fullname)
        
    def handlePVSModeUpdated(self, pvsMode = PVS_MODE_OFF):
        params = {PVSMODE: pvsMode}
        pub.sendMessage(PUB_UPDATEMENUBAR, parameters=params)
        self.setStatusbarText("PVS Mode: " + pvsMode)

    def handlePVSContextUpdated(self):
        self.setStatusbarText("PVS Context: " + Preferences().getRecentContexts()[0], 1)

    def handleNumberOfOpenFilesChanged(self):
        params = {OPENFILES: RichEditorManager().getNumberOfOpenFiles()}
        pub.sendMessage(PUB_UPDATEMENUBAR, parameters=params)
        
    def handleUndoRequest(self):
        """handle the Undo request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None and textCtrl.CanUndo():
            textCtrl.Undo()
            return True
        return False
        
    def handleRedoRequest(self):
        """handle the Redo request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None and textCtrl.CanRedo():
            textCtrl.Redo()
            return True
        return False
                
    def handleCopyRequest(self):
        """handle the Copy request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None and textCtrl.CanCopy():
            textCtrl.Copy()
            return True
        return False
    
    def handleCutRequest(self):
        """handle the Cut request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None and textCtrl.CanCut():
            textCtrl.Cut()
            return True
        return False
    
    def handlePasteRequest(self):
        """handle the Paste request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None and textCtrl.CanPaste():
            textCtrl.Paste()
            return True
        return False
    
    def handleSelectAllRequest(self):
        """handle the Select All request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl is not None:
            textCtrl.SelectAll()
            return True
        return False
    
    
    # Dialog Boxes: 
    
    def showDialogBox(self, message, type=MESSAGE):
        if type==MESSAGE:
            self.showMessage(message)
        elif type==ERROR:
            self.showError(message)
        elif type==WARNING:
            self.showWarning(message)
        else:
            logging.error("Unknown message type, I will display it as a normal message")
            self.showMessage(message)
        
    def showError(self, message, title=ERROR):
        """Show a dialog for an error"""
        dlg = wx.MessageDialog(self, message, title, wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()
        
    def showWarning(self, message, title=WARNING):
        """Show a dialog for a warning"""
        dlg = wx.MessageDialog(self, message, title, wx.OK | wx.ICON_WARNING)
        dlg.ShowModal()
        dlg.Destroy()
        
    def showMessage(self, message, title=MESSAGE):
        """Show a dialog for a message"""
        dlg = wx.MessageDialog(self, message, title, wx.OK | wx.ICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()
        
    def askYesNoQuestion(self, question, title=EMPTY_STRING):
        """Show a dialog box to ask a question with two possible answers: Yes and No"""
        dlg = wx.MessageDialog(self, question, title, wx.YES_NO | wx.ICON_QUESTION)
        choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO
        dlg.Destroy()
        return choice
    
    def askYesNoCancelQuestion(self, question, title=EMPTY_STRING):
        """Show a dialog box to ask a question with three possible answers: Yes, No, and Cancel"""
        dlg = wx.MessageDialog(self, question, title, wx.YES_NO | wx.CANCEL | wx.ICON_QUESTION)
        choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO or wx.ID_CANCEL
        dlg.Destroy()
        return choice
    
    def chooseDirectory(self, message, defaultDirectory=EMPTY_STRING):
        """Show a dialog to choose a directory"""    
        dialog = wx.DirDialog (self, message = message, defaultPath=defaultDirectory)
        newPath = None
        if dialog.ShowModal() == wx.ID_OK:
            newPath = dialog.GetPath()
        dialog.Destroy()
        return newPath

    # Private methods:
    
    def _findFocusedTextCtrl(self):
        focus = self.FindFocus()
        if focus is not None and (isinstance(focus, wx.TextCtrl) or isinstance(focus, stc.StyledTextCtrl)):
            return focus
        return None

        