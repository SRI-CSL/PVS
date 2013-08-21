
# This file contains all the functions handling menu and toolbar events

import util
import logging
from ui.frmgr import FindReplaceManager
from pvscomm import PVSCommandManager, PVSCommandManager
from constants import *
import wx
from remgr import RichEditorManager
#import ui.tbmgr
from preference import Preferences
from pvscomm import PVSCommunicator
from ui.plugin import PluginManager
import wx.stc as stc
import constants
from wx.lib.pubsub import setupkwargs, pub 

def onChangeContext(event):
    """called to handle 'change context' request"""
    frame = util.getMainFrame()
    preferences = Preferences()
    newContext = frame.chooseDirectory("Select a directory", preferences.getRecentContexts()[0])
    if newContext is not None:
        if RichEditorManager().ensureFilesAreSavedToPoceed(): 
            if PVSCommandManager().pvsMode == PVS_MODE_LISP:
                PVSCommandManager().changeContext(newContext)
            frame.closeContext()
            preferences.setRecentContext(newContext)
            frame.menubar.prepareRecentContextsSubMenu()
            frame.loadContext()
            logging.info("New context is set to %s", newContext)

def onCreateNewFile(event):
    """called to handle 'create new file' request"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
    if dialogging.ShowModal() == wx.ID_OK:
        fullname = dialogging.GetPath()
        if not fullname.endswith(PVS_EXTENSION):
            fullname = fullname + PVS_EXTENSION
        logging.info("Creating new file %s", fullname)
        pub.sendMessage(PUB_ADDFILE, fullname=fullname)
    else:
        logging.info("Nothing was selected.")
    dialogging.Destroy()
    pub.sendMessage(PUB_NUMBEROFOPENFILESCHANGED)

def onOpenFile(event):
    """called to handle 'open file' request"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        logging.info("Opening file %s", fullname)
        pub.sendMessage(PUB_ADDFILE, fullname=fullname)
    else:
        logging.info("Nothing was selected.")
    dialog.Destroy()
    pub.sendMessage(PUB_NUMBEROFOPENFILESCHANGED)

def onCloseFile(event):
    """close an open file"""
    logging.debug("Starting")
    richEditor = RichEditorManager().getFocusedRichEditor()
    if richEditor is not None:
        canClose = True
        if richEditor.styledText.GetModify():
            choice = self.askYesNoCancelQuestion("This file has been modified. Save changes?")
            if choice == wx.ID_YES:
                richEditor.saveFile()
            elif choice == wx.ID_CANCEL:
                canClose = False
        if canClose:
            pub.sendMessage(PUB_CLOSEFILE, fullname=richEditor.getFilename())
            pub.sendMessage(PUB_NUMBEROFOPENFILESCHANGED)
    else:
        logging.warn("No rich editor is open")

def onSaveFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    logging.debug("Starting")
    richEditor = RichEditorManager().getFocusedRichEditor()
    if richEditor is not None:
        richEditor.saveFile()
        
def onSaveAsFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Save File As...", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
    if dialogging.ShowModal() == wx.ID_OK:
        fullname = dialogging.GetPath()
        if not fullname.endswith(PVS_EXTENSION):
            fullname = fullname + PVS_EXTENSION
        logging.info("Saving file as %s", fullname)
        richEditor = RichEditorManager().getFocusedRichEditor()
        if richEditor is not None:
            richEditor.saveFile(fullname)
    else:
        logging.info("Nothing was selected.")
    dialogging.Destroy()
    
        
def onSaveAllFiles(event):
    """save all the open files"""
    logging.debug("Starting")
    RichEditorManager().saveAllFiles()
    
def onQuitFrame(event):
    """called to handle 'quit application' request"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    frame.Close()
            
def onUndo(event):
    """called to handle 'undo' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handleUndoRequest():
        event.Skip()

def onRedo(event):
    """called to handle 'redo' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handleRedoRequest():
        event.Skip()

def onSelectAll(event):
    """called to handle 'select all' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handleSelectAllRequest():
        event.Skip()

def onCutText(event):
    """called to handle 'cut' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handleCutRequest():
        event.Skip()

def onCopyText(event):
    """called to handle 'copy' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handleCopyRequest():
        event.Skip()

def onPasteText(event):
    """called to handle 'paste' request"""
    logging.debug("Starting")
    if not util.getMainFrame().handlePasteRequest():
        event.Skip()

def onFindText(event):
    """called to handle 'find text' request"""
    logging.debug("Starting")
    #TODO: Fix Find/Replace in a good manner.
    focus = util.getMainFrame().FindFocus()
    #page = RichEditorManager().getFocusedRichEditor()
    if focus is not None and (isinstance(focus, wx.TextCtrl) or isinstance(focus, stc.StyledTextCtrl)):
        pass
    else:
        focus = RichEditorManager().getFocusedRichEditor()
        if focus is not None:
            focus = focus.styledText
    if focus is not None:
        selected = focus.GetSelectedText()
        if selected is None:
            selected = ""
        FindReplaceManager(None, selected, EMPTY_STRING).show()

def toggleTool(name):
    PluginManager().toggleTool(name)

def onToggleViewToolbar(event):
    """called to handle 'toggle viewing the toolbar' request"""
    logging.debug("Starting")
    preferences = Preferences()
    visibile = preferences.getVisibleToolbar()
    #TODO: fix this. toolbar is no longer in 
    frame = util.getMainFrame()
    if visibile:
        toolbarHeight = frame.toolbar.GetSize()[1]
        frameSize = frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] + toolbarHeight)
        frame.SetSize(newFrameSize)
        frame.toolbar.Hide()
    else:
        frame.toolbar.Show()        
        toolbarHeight = frame.toolbar.GetSize()[1]
        frameSize = frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] - toolbarHeight)
        frame.SetSize(newFrameSize)
    preferences.setVisibleToolbar(not visibile)
    #event.Skip()

def onTypecheck(event):
    """called to handle 'typecheck' request"""
    logging.debug("Starting")
    fullname = RichEditorManager().getFocusedRichEditor().getFilename()
    PVSCommandManager().typecheck(fullname)
    #event.Skip()

