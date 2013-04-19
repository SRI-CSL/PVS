
# This file contains all the functions handling menu and toolbar events

import util
from ui.findreplacemanager import FindReplaceManager
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

def onCreateNewFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.filesbuffermanager.createNewFile()
    #event.Skip()

def onOpenFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.filesbuffermanager.openFile()
    # event.Skip()

def onSaveFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.filesbuffermanager.saveFile()
    #event.Skip()

def onSaveAsFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveAsFile' not implemented!")
    #event.Skip()

def onCloseFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("onCloseFile event: %s", event)
    util.filesbuffermanager.closeFile()
    
def onQuitFrame(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.frame.Close()

def onUndo(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.notebook.undo()
    #event.Skip()

def onSelectAll(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.notebook.selectAll()
    #event.Skip()

def onCutText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Cut()
    #util.notebook.cut()
    #event.Skip()

def onCopyText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Copy()
    #util.notebook.copy()
    #event.Skip()

def onPasteText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = util.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Paste()
    #util.notebook.paste()
    #event.Skip()

def onFindText(event):  # wxGlade: PVSMainFrame.<event_handler>
    page = util.notebook.getActivePage()
    selected = page.styledText.GetSelectedText()
    if selected == None:
        selected = ""
    FindReplaceManager(None, selected, EMPTY_STRING).show()

    #util.notebook.find()
    #event.Skip()

def onViewFilesAndBuffersTrees(event):
    log.info("onViewFilesAndBuffersTrees was called")
    visibile = util.preference.visibleFilesBuffersTrees()
    if visibile:
        util.filesbuffermanager.Hide()
    else:
        util.filesbuffermanager.Show()        
    util.preference.setFilesBuffersTrees(not visibile)

def onViewProofTree(event):
    log.info("onViewProofTree was called")
    visibile = util.preference.visibleProofTree()
    if visibile:
        util.prooftreemanager.Hide()
    else:
        util.prooftreemanager.Show()        
    util.preference.setProofTree(not visibile)
    #event.Skip()

def onChangeContext(event):  # wxGlade: PVSMainFrame.<event_handler>
    if util.runner == None or util.runner.status != PVS_MODE_EDIT:
        ui.dialogs.showError("PVS is not running or it is in prover mode")
    else:
        newContext = ui.dialogs.chooseDirectory("Select a directory", util.preference.getContext())
        if newContext != None:
            changeContext(newContext)
            log.info("New context is set to %s", newContext)

def onRestoreContextAutomatically(event):  # wxGlade: PVSMainFrame.<event_handler>
    value = util.menubar.restoreContextMenuItem.IsChecked()
    util.preference.setRestoreContextAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onStartPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if util.runner == None:
        util.runner = runr.PVSRunner()
        util.runner.start()
    else:
        ui.dialogs.showError("PVS is already running")

def onStopPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if util.runner != None:
        util.runner.terminate()
        util.runner = None
    else:
        ui.dialogs.showError("PVS is not running")

def onTypecheck(event):  # wxGlade: PVSMainFrame.<event_handler>
    filename = util.notebook.getActiveFilename()
    typecheck(filename)
    #event.Skip()

def onSetPVSLocation(event):  # wxGlade: PVSMainFrame.<event_handler>
    newLocation = ui.dialogs.chooseDirectory("Select the PVS directory", util.preference.getPVSLocation())
    if newLocation != None:
        util.preference.setPVSLocation(newLocation)
        log.info("New PVS location is set to %s", newLocation)
    #event.Skip()

def onSaveFileAs(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveFileAs' not implemented!")
    #event.Skip()

def onCoptText(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onCoptText' not implemented!")
    #event.Skip()

def onSaveAllFiles(event):  # wxGlade: PVSMainFrame.<event_handler>
    util.filesbuffermanager.saveAllFiles()
