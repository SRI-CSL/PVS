
# This file contains all the functions handling menu and toolbar events

import util
from ui.frmgr import FindReplaceManager
import runr
from cmdmgr import *
from constants import *
import wx.stc as stc
import ui.dialogs
import wx

log = util.getLogger(__name__)

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
    util.filesBuffersManager.createNewFile()
    #event.Skip()

def onOpenFile(event):
    """called to handle 'open file' request"""
    util.filesBuffersManager.openFile()
    # event.Skip()

def onSaveFile(event):
    """called to handle 'save file' request"""
    util.filesBuffersManager.saveFile()
    #event.Skip()

def onSaveAsFile(event):
    """called to handle 'save as...' request"""
    log.info("Event handler `onSaveAsFile' not implemented!")
    #event.Skip()

def onSaveAllFiles(event):
    """called to handle 'save all files' request"""
    util.filesBuffersManager.saveAllFiles()

def onCloseFile(event):
    """called to handle 'close file' request"""
    log.info("onCloseFile event: %s", event)
    util.filesBuffersManager.closeFile()
    
def onQuitFrame(event):
    """called to handle 'quit application' request"""
    util.frame.Close()

def onUndo(event):
    """called to handle 'undo' request"""
    util.notebook.undo()
    #event.Skip()

def onSelectAll(event):
    """called to handle 'select all' request"""
    util.notebook.selectAll()
    #event.Skip()

def onCutText(event):
    """called to handle 'cut' request"""
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Cut()
    #event.Skip()

def onCopyText(event):
    """called to handle 'copy' request"""
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Copy()
    #event.Skip()

def onPasteText(event):
    """called to handle 'paste' request"""
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Paste()
    #event.Skip()

def onFindText(event):
    """called to handle 'find text' request"""
    page = util.notebook.getActivePage()
    selected = page.styledText.GetSelectedText()
    if selected == None:
        selected = ""
    FindReplaceManager(None, selected, EMPTY_STRING).show()

def onToggleViewFilesAndBuffersTrees(event):
    """called to handle 'toggle viewing FilesAndBufferTree view' request"""
    log.info("onToggleViewFilesAndBuffersTrees was called")
    visibile = util.preference.getVisibleFilesBuffersTrees()
    if visibile:
        util.filesBuffersManager.Hide()
    else:
        util.filesBuffersManager.Show()        
    util.preference.setVisibleFilesBuffersTrees(not visibile)

def onToggleViewProofTree(event):
    """called to handle 'toggle viewing ProofTree view' request"""
    log.info("onToggleViewProofTree was called")
    visibile = util.preference.getVisibleProofTree()
    if visibile:
        util.proofTreeManager.Hide()
    else:
        util.proofTreeManager.Show()        
    util.preference.setVisibleProofTree(not visibile)
    #event.Skip()

def onToggleViewToolbar(event):
    """called to handle 'toggle viewing the toolbar' request"""
    log.info("onToggleViewToolbar was called")
    visibile = util.preference.getVisibleToolbar()
    if visibile:
        toolbarHeight = util.toolbar.GetSize()[1]
        frameSize = util.frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] + toolbarHeight)
        util.frame.SetSize(newFrameSize)
        util.toolbar.Hide()
    else:
        util.toolbar.Show()        
        toolbarHeight = util.toolbar.GetSize()[1]
        frameSize = util.frame.GetSize()
        newFrameSize = (frameSize[0], frameSize[1] - toolbarHeight)
        util.frame.SetSize(newFrameSize)
    util.preference.setVisibleToolbar(not visibile)
    #event.Skip()

def onChangeContext(event):
    """called to handle 'change context' request"""
    if util.runner == None or util.runner.status != PVS_MODE_EDIT:
        ui.dialogs.showError("PVS is not running or it is in prover mode")
    else:
        newContext = ui.dialogs.chooseDirectory("Select a directory", util.preference.getContext())
        if newContext != None:
            changeContext(newContext)
            log.info("New context is set to %s", newContext)

def onContextPreferencesRestoredAutomatically(event):
    """called to handle 'toggle loading the context preferences automatically' request"""
    value = util.menubar.restoreContextMenuItem.IsChecked()
    util.preference.setContextPreferencesRestoredAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onStartPVS(event):
    """called to handle 'start pvs' request"""
    if util.runner == None:
        util.runner = runr.PVSRunner()
        util.runner.start()
    else:
        ui.dialogs.showError("PVS is already running")

def onStopPVS(event):
    """called to handle 'stop pvs' request"""
    if util.runner != None:
        util.runner.terminate()
        util.runner = None
    else:
        ui.dialogs.showError("PVS is not running")

def onTypecheck(event):
    """called to handle 'typecheck' request"""
    filename = util.notebook.getActiveFilename()
    typecheck(filename)
    #event.Skip()

def onSetPVSLocation(event):
    """called to handle 'setting pvs location' request"""
    newLocation = ui.dialogs.chooseDirectory("Select the PVS directory", util.preference.getPVSLocation())
    if newLocation != None:
        util.preference.setPVSLocation(newLocation)
        log.info("New PVS location is set to %s", newLocation)
    #event.Skip()
