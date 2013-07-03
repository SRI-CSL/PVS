
# This file contains all the functions handling menu and toolbar events

import util
from ui.frmgr import FindReplaceManager
from pvscomm import PVSCommandManager, PVSCommandManager
from constants import *
import wx
from remgr import RichEditorManager
#import ui.tbmgr
from preference import Preferences
from pvscomm import PVSCommunicator
from wx.lib.pubsub import pub
from ui.plugin import PluginManager
import wx.stc as stc

log = util.getLogger(__name__)

def onChangeContext(event):
    """called to handle 'change context' request"""
    frame = util.getMainFrame()
    preferences = Preferences()
    newContext = frame.chooseDirectory("Select a directory", preferences.getContext())
    if newContext is not None:
        if RichEditorManager().ensureFilesAreSavedToPoceed(): 
            if PVSCommandManager().pvsMode == PVS_MODE_LISP:
                PVSCommandManager().changeContext(newContext)
            frame.closeContext()
            preferences.setContext(newContext)
            frame.loadContext()
            log.info("New context is set to %s", newContext)

def onCreateNewFile(event):
    """called to handle 'create new file' request"""
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        if not fullname.endswith(PVS_EXTENSION):
            fullname = fullname + PVS_EXTENSION
        log.info("Creating new file %s", fullname)
        pub.sendMessage(PUB_ADDFILE, fullname=fullname)
    else:
        log.info("Nothing was selected.")
    dialog.Destroy()
    frame.handleNumberOfOpenFilesChanged(openFiles = RichEditorManager().getNumberOfOpenFiles())

def onOpenFile(event):
    """called to handle 'open file' request"""
    frame = util.getMainFrame()
    filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
    dialog = wx.FileDialog (frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
    if dialog.ShowModal() == wx.ID_OK:
        fullname = dialog.GetPath()
        log.info("Opening file %s", fullname)
        pub.sendMessage(PUB_ADDFILE, fullname=fullname)
    else:
        log.info("Nothing was selected.")
    dialog.Destroy()
    frame.handleNumberOfOpenFilesChanged(openFiles = RichEditorManager().getNumberOfOpenFiles())

def onCloseFile(event):
    """close an open file"""
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
            util.getMainFrame().handleNumberOfOpenFilesChanged(openFiles = RichEditorManager().getNumberOfOpenFiles())
    else:
        log.warn("No rich editor is open")

def onSaveFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    RichEditorManager().getFocusedRichEditor().saveFile()
        
def onSaveAsFile(event):
    """save the active file (an active file is one whose tab is visible)"""
    #TODO: implement this, we need a dialog box to get the new file name
    RichEditorManager().getFocusedRichEditor().saveFile()  
        
def onSaveAllFiles(event):
    """save all the open files"""
    RichEditorManager().saveAllFiles()
    
def onQuitFrame(event):
    """called to handle 'quit application' request"""
    frame = util.getMainFrame()
    frame.Close()
            
def onUndo(event):
    """called to handle 'undo' request"""
    if not util.getMainFrame().handleUndoRequest():
        event.Skip()

def onRedo(event):
    """called to handle 'redo' request"""
    if not util.getMainFrame().handleRedoRequest():
        event.Skip()

def onSelectAll(event):
    """called to handle 'select all' request"""
    if not util.getMainFrame().handleSelectAllRequest():
        event.Skip()

def onCutText(event):
    """called to handle 'cut' request"""
    if not util.getMainFrame().handleCutRequest():
        event.Skip()

def onCopyText(event):
    """called to handle 'copy' request"""
    if not util.getMainFrame().handleCopyRequest():
        event.Skip()

def onPasteText(event):
    """called to handle 'paste' request"""
    if not util.getMainFrame().handlePasteRequest():
        event.Skip()

def onFindText(event):
    """called to handle 'find text' request"""
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


def onToggleViewFilesTree(event):
    """called to handle 'toggle viewing FilesTree view' request"""
    log.info("onToggleViewFilesTree was called")
    toggleTool(FILESTREE)

def onToggleViewProofTree(event):
    """called to handle 'toggle viewing ProofTree view' request"""
    log.info("onToggleViewProofTree was called")
    toggleTool(PROOFTREE)

def toggleTool(name):
    PluginManager().toggleTool(name)

def onToggleViewToolbar(event):
    """called to handle 'toggle viewing the toolbar' request"""
    log.info("onToggleViewToolbar was called")
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

def onContextPreferencesRestoredAutomatically(event):
    """called to handle 'toggle loading the context preferences automatically' request"""
    frame = util.getMainFrame()
    value = frame.menubar.restoreContextMenuItem.IsChecked()
    Preferences().setContextPreferencesRestoredAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onTypecheck(event):
    """called to handle 'typecheck' request"""
    fullname = RichEditorManager().getFocusedRichEditor().getFilename()
    PVSCommandManager().typecheck(fullname)
    #event.Skip()

def onSetPVSLocation(event):
    """called to handle 'setting pvs location' request"""
    preferences = Preferences()
    newLocation = util.getMainFrame().chooseDirectory("Select the PVS directory", preferences.getPVSLocation())
    if newLocation is not None:
        preferences.setPVSLocation(newLocation)
        log.info("New PVS location is set to %s", newLocation)
    #event.Skip()
