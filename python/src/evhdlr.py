
# This file contains all the functions handling menu and toolbar events

import util
import logging
from ui.frmgr import FindReplaceManager
import pvscomm
from constants import *
import wx
import remgr
import ui.logdlg
#import ui.tbmgr
import preference
from ui.plugin import PluginManager
import wx.stc as stc
from wx.lib.pubsub import setupkwargs, pub 

def onChangeContext(event):
    """called to handle 'change context' request"""
    frame = util.getMainFrame()
    preferences = preference.Preferences()
    newContext = frame.chooseDirectory("Select a directory", preferences.getRecentContexts()[0])
    if newContext is not None:
        if remgr.RichEditorManager().ensureFilesAreSavedToPoceed(): 
            if pvscomm.PVSCommandManager().pvsMode == PVS_MODE_LISP:
                pvscomm.PVSCommandManager().changeContext(newContext)
            preferences.setRecentContext(newContext)
            pub.sendMessage(PUB_UPDATEPVSCONTEXT)
            frame.restoreOpenFiles()
            logging.info("New context is set to %s", newContext)

def onCreateNewFile(event):
    """called to handle 'create new file' request"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        if not fullname.endswith(PVS_EXTENSION):
            fullname = fullname + PVS_EXTENSION
        logging.info("Creating new file %s", fullname)
        util.openFile(fullname)
    else:
        logging.info("Nothing was selected.")
    dialog.Destroy()

def onOpenFile(event):
    """called to handle 'open file' request"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        logging.info("Opening file %s", fullname)
        util.openFile(fullname)
    else:
        logging.info("Nothing was selected.")
    dialog.Destroy()

def onCloseFile(event):
    """close an open file"""
    logging.debug("Starting")
    richEditor = remgr.RichEditorManager().getFocusedRichEditor()
    if richEditor is not None:
        canClose = True
        if richEditor.styledText.GetModify():
            choice = self.askYesNoCancelQuestion("This file has been modified. Save changes?")
            if choice == wx.ID_YES:
                richEditor.saveFile()
            elif choice == wx.ID_CANCEL:
                canClose = False
        if canClose:
            fullname = richEditor.getFilename()
            util.closeFile(fullname)
    else:
        logging.warn("No rich editor is open")

def onSaveFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    logging.debug("Starting")
    richEditor = remgr.RichEditorManager().getFocusedRichEditor()
    if richEditor is not None:
        richEditor.saveFile()
        
def onSaveAsFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    logging.debug("Starting")
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Save File As...", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        if not fullname.endswith(PVS_EXTENSION):
            fullname = fullname + PVS_EXTENSION
        logging.info("Saving file as %s", fullname)
        richEditor = remgr.RichEditorManager().getFocusedRichEditor()
        if richEditor is not None:
            richEditor.saveFile(fullname)
    else:
        logging.info("Nothing was selected.")
    dialog.Destroy()
    
        
def onSaveAllFiles(event):
    """save all the open files"""
    logging.debug("Starting")
    remgr.RichEditorManager().saveAllFiles()
    
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
    #page = remgr.RichEditorManager().getFocusedRichEditor()
    if focus is not None and (isinstance(focus, wx.TextCtrl) or isinstance(focus, stc.StyledTextCtrl)):
        pass
    else:
        focus = remgr.RichEditorManager().getFocusedRichEditor()
        if focus is not None:
            focus = focus.styledText
    if focus is not None:
        selected = focus.GetSelectedText()
        if selected is None:
            selected = ""
        FindReplaceManager(selected, EMPTY_STRING).show()

def toggleTool(name):
    PluginManager().toggleTool(name)


def onTypecheck(event):
    """called to handle 'typecheck' request"""
    logging.debug("Starting")
    fullname = remgr.RichEditorManager().getFocusedRichEditor().getFilename()
    pvscomm.PVSCommandManager().typecheck(fullname)
    #event.Skip()

def onShowPVSCommunicationLog(event):
    """called to handle 'pvs communication logs' request"""
    logging.debug("Starting")
    dlg = ui.logdlg.PVSCommunicationLogDialog(util.getMainFrame(), "PVS Communication Log", JSONLOG)
    dlg.Show()


