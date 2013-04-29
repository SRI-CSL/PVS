
# This file contains all the functions handling menu and toolbar events

import util
from ui.frmgr import FindReplaceManager
from cmdmgr import *
from constants import *
import wx.stc as stc
import wx
import gui
import preference
import runner

log = util.getLogger(__name__)

#TODO: Move most of the expected functionalities to gui manager. Let that API handle event requests.

class PVSResultEvent(wx.PyEvent):
    """Simple event to carry arbitrary result data."""
    
    def __init__(self, message, data):
        """Init Result Event."""
        wx.PyEvent.__init__(self)
        self.SetEventType(util.EVT_RESULT_ID)
        self.message = message
        self.data = data

def onCreateNewFile(event):
    """called to handle 'create new file' request"""
    gui.manager.createNewFile()
    #event.Skip()

def onOpenFile(event):
    """called to handle 'open file' request"""
    gui.manager.openFile()
    # event.Skip()

def onSaveFile(event):
    """called to handle 'save file' request"""
    gui.manager.saveFile()
    #event.Skip()

def onSaveAsFile(event):
    """called to handle 'save as...' request"""
    log.info("Event handler `onSaveAsFile' not implemented!")
    #event.Skip()

def onSaveAllFiles(event):
    """called to handle 'save all files' request"""
    gui.manager.saveAllFiles()

def onCloseFile(event):
    """called to handle 'close file' request"""
    log.info("onCloseFile event: %s", event)
    gui.manager.closeFile()
    
def onQuitFrame(event):
    """called to handle 'quit application' request"""
    gui.manager.closeMainFrame()

def onUndo(event):
    """called to handle 'undo' request"""
    gui.manager.notebook.undo()
    #event.Skip()

def onSelectAll(event):
    """called to handle 'select all' request"""
    gui.manager.notebook.selectAll()
    #event.Skip()

def onCutText(event):
    """called to handle 'cut' request"""
    #TODO: ensure that Cut/Copy/Paste calls are good and reliable
    x = gui.manager.notebook.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Cut()
    #event.Skip()

def onCopyText(event):
    """called to handle 'copy' request"""
    x = gui.manager.notebook.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Copy()
    #event.Skip()

def onPasteText(event):
    """called to handle 'paste' request"""
    x = gui.manager.notebook.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Paste()
    #event.Skip()

def onFindText(event):
    """called to handle 'find text' request"""
    page = gui.manager.notebook.getActivePage()
    selected = page.styledText.GetSelectedText()
    if selected == None:
        selected = ""
    FindReplaceManager(None, selected, EMPTY_STRING).show()

def onToggleViewFilesAndBuffersTrees(event):
    """called to handle 'toggle viewing FilesAndBufferTree view' request"""
    log.info("onToggleViewFilesAndBuffersTrees was called")
    visible = preference.manager.getVisibleFilesBuffersTrees()
    gui.manager.showFilesBuffersTreeFrame(visible)
    preference.manager.setVisibleFilesBuffersTrees(not visible)

def onToggleViewProofTree(event):
    """called to handle 'toggle viewing ProofTree view' request"""
    log.info("onToggleViewProofTree was called")
    visible = preference.manager.getVisibleProofTree()
    gui.manager.showProofTreeFrame(visible)
    preference.manager.setVisibleProofTree(not visible)
    #event.Skip()

def onToggleViewToolbar(event):
    """called to handle 'toggle viewing the toolbar' request"""
    log.info("onToggleViewToolbar was called")
    visibile = preference.manager.getVisibleToolbar()
    if visibile:
        toolbarHeight = gui.manager.toolbar.GetSize()[1]
        frameSize = gui.manager.frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] + toolbarHeight)
        gui.manager.frame.SetSize(newFrameSize)
        gui.manager.toolbar.Hide()
    else:
        gui.manager.toolbar.Show()        
        toolbarHeight = gui.manager.toolbar.GetSize()[1]
        frameSize = gui.manager.frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] - toolbarHeight)
        gui.manager.frame.SetSize(newFrameSize)
    preference.manager.setVisibleToolbar(not visibile)
    #event.Skip()

def onChangeContext(event):
    """called to handle 'change context' request"""
    newContext = gui.manager.chooseDirectory("Select a directory", preference.manager.getContext())
    if newContext != None:
        if gui.manager.ensureFilesAreSavedToPoceed():        
            if runner.manager != None and runner.manager.status == PVS_MODE_EDIT:
                changeContext(newContext)
            gui.manager.closeContext()
            preference.manager.setContext(newContext)
            gui.manager.loadContext()
            log.info("New context is set to %s", newContext)
    

def onContextPreferencesRestoredAutomatically(event):
    """called to handle 'toggle loading the context preferences automatically' request"""
    value = gui.manager.menubar.restoreContextMenuItem.IsChecked()
    preference.manager.setContextPreferencesRestoredAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onStartPVS(event):
    """called to handle 'start pvs' request"""
    if runner.manager == None:
        runner.PVSRunner()
        runner.manager.start()
    else:
        gui.manager.showError("PVS is already running")

def onStopPVS(event):
    """called to handle 'stop pvs' request"""
    if runner.manager != None:
        runner.manager.terminate()
        runner.manager = None
    else:
        gui.manager.showError("PVS is not running")

def onTypecheck(event):
    """called to handle 'typecheck' request"""
    filename = gui.manager.notebook.getActiveFilename()
    typecheck(filename)
    #event.Skip()

def onSetPVSLocation(event):
    """called to handle 'setting pvs location' request"""
    newLocation = gui.manager.chooseDirectory("Select the PVS directory", preference.manager.getPVSLocation())
    if newLocation != None:
        preference.manager.setPVSLocation(newLocation)
        log.info("New PVS location is set to %s", newLocation)
    #event.Skip()
