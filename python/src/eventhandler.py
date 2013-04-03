
# This file contains all the functions handling menu and toolbar events

import common
from ui.findreplacemanager import FindReplaceManager
from pvsrunner import PVSRunner
from commandmanager import *
from constants import *
import wx.stc as stc
import ui.dialogs
import wx

log = common.getLogger(__name__)

def onCreateNewFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.filesbuffermanager.createNewFile()
    #event.Skip()

def onOpenFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.filesbuffermanager.openFile()
    # event.Skip()

def onSaveFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.filesbuffermanager.saveFile()
    #event.Skip()

def onSaveAsFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveAsFile' not implemented!")
    #event.Skip()

def onCloseFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("onCloseFile event: %s", event)
    common.filesbuffermanager.closeFile()
    
def onQuitFrame(event):  # wxGlade: PVSMainFrame.<event_handler>
    if common.runner != None:
        common.runner.terminate()
        common.runner = None
    common.frame.Close()

def onUndo(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.notebook.undo()
    #event.Skip()

def onSelectAll(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.notebook.selectAll()
    #event.Skip()

def onCutText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = common.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Cut()
    #common.notebook.cut()
    #event.Skip()

def onCopyText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = common.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Copy()
    #common.notebook.copy()
    #event.Skip()

def onPasteText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = common.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Paste()
    #common.notebook.paste()
    #event.Skip()

def onFindText(event):  # wxGlade: PVSMainFrame.<event_handler>
    page = common.notebook.getActivePage()
    selected = page.styledText.GetSelectedText()
    if selected == None:
        selected = ""
    FindReplaceManager(None, selected, EMPTY_STRING).show()

    #common.notebook.find()
    #event.Skip()

def onViewFilesAndBuffersTrees(event):
    log.info("onViewFilesAndBuffersTrees was called")
    visibile = common.preference.visibleFilesBuffersTrees()
    if visibile:
        common.filesbuffermanager.Hide()
    else:
        common.filesbuffermanager.Show()        
    common.preference.setFilesBuffersTrees(not visibile)

def onViewProofTree(event):
    log.info("onViewProofTree was called")
    visibile = common.preference.visibleProofTree()
    if visibile:
        common.prooftreemanager.Hide()
    else:
        common.prooftreemanager.Show()        
    common.preference.setProofTree(not visibile)
    #event.Skip()

def onChangeContext(event):  # wxGlade: PVSMainFrame.<event_handler>
    if common.runner == None or common.runner.status != PVS_MODE_EDIT:
        ui.dialogs.showError("PVS is not running or it is in prover mode")
    else:
        newContext = ui.dialogs.chooseDirectory("Select a directory", common.preference.getContext())
        if newContext != None:
            changeContext(newContext)
            log.info("New context is set to %s", newContext)

def onRestoreContextAutomatically(event):  # wxGlade: PVSMainFrame.<event_handler>
    value = common.menubar.restoreContextMenuItem.IsChecked()
    common.preference.setRestoreContextAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onStartPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if common.runner == None:
        common.runner = PVSRunner()
        common.runner.start()
    else:
        ui.dialogs.showError("PVS is already running")

def onStopPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if common.runner != None:
        common.runner.terminate()
        common.runner = None
    else:
        ui.dialogs.showError("PVS is not running")

def onTypecheck(event):  # wxGlade: PVSMainFrame.<event_handler>
    filename = common.notebook.getActiveFilename()
    typecheck(filename)
    #event.Skip()

def onSetPVSLocation(event):  # wxGlade: PVSMainFrame.<event_handler>
    newLocation = ui.dialogs.chooseDirectory("Select the PVS directory", common.preference.getPVSLocation())
    if newLocation != None:
        common.preference.setPVSLocation(newLocation)
        log.info("New PVS location is set to %s", newLocation)
    #event.Skip()

def onSaveFileAs(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveFileAs' not implemented!")
    #event.Skip()

def onCoptText(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onCoptText' not implemented!")
    #event.Skip()

def onSaveAllFiles(event):  # wxGlade: PVSMainFrame.<event_handler>
    common.filesbuffermanager.saveAllFiles()
