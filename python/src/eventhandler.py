
# This file contains all the functions handling menu and toolbar events

import config
from findreplacemanager import FindReplaceManager
from pvsrunner import PVSRunner
from pvscommandmanager import *
from constants import *
import wx.stc as stc
import wx

log = config.getLogger(__name__)

def onCreateNewFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.filesbuffermanager.createNewFile()
    #event.Skip()

def onOpenFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.filesbuffermanager.openFile()
    # event.Skip()

def onSaveFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.filesbuffermanager.saveFile()
    #event.Skip()

def onSaveAsFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveAsFile' not implemented!")
    #event.Skip()

def onCloseFile(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.filesbuffermanager.closeFile()

def onQuitFrame(event):  # wxGlade: PVSMainFrame.<event_handler>
    if config.runner != None:
        config.runner.terminate()
        config.runner = None
    config.frame.Close()

def onUndo(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.undo()
    #event.Skip()

def onSelectAll(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.selectAll()
    #event.Skip()

def onCutText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = config.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Cut()
    #config.notebook.cut()
    #event.Skip()

def onCopyText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = config.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Copy()
    #config.notebook.copy()
    #event.Skip()

def onPasteText(event):  # wxGlade: PVSMainFrame.<event_handler>
    x = config.frame.FindFocus()
    if isinstance(x, wx.TextCtrl) or isinstance(x, stc.StyledTextCtrl):
        x.Paste()
    #config.notebook.paste()
    #event.Skip()

def onFindText(event):  # wxGlade: PVSMainFrame.<event_handler>
    page = config.notebook.getActivePage()
    selected = page.styledText.GetSelectedText()
    if selected == None:
        selected = ""
    FindReplaceManager(None, selected, EMPTY_STRING).show()

    #config.notebook.find()
    #event.Skip()

def onViewFilesAndBuffersTrees(event):
    log.info("onViewFilesAndBuffersTrees was called")
    visibile = config.preference.visibleFilesBuffersTrees()
    if visibile:
        config.filesbuffermanager.Hide()
    else:
        config.filesbuffermanager.Show()        
    config.preference.setFilesBuffersTrees(not visibile)

def onViewProofTree(event):
    log.info("onViewProofTree was called")
    visibile = config.preference.visibleProofTree()
    if visibile:
        config.prooftreemanager.Hide()
    else:
        config.prooftreemanager.Show()        
    config.preference.setProofTree(not visibile)
    #event.Skip()

def onChangeContext(event):  # wxGlade: PVSMainFrame.<event_handler>
    if config.runner == None or config.runner.status != PVS_MODE_EDIT:
        dialogs.showError("PVS is not running or it is in prover mode")
    else:
        newContext = dialogs.chooseDirectory("Select a directory", config.preference.getContext())
        if newContext != None:
            changeContext(newContext)
            log.info("New context is set to %s", newContext)

def onRestoreContextAutomatically(event):  # wxGlade: PVSMainFrame.<event_handler>
    value = config.menubar.restoreContextMenuItem.IsChecked()
    config.preference.setRestoreContextAutomatically(value)
    log.info("Setting RestoreContextAutomatically flag to %s", value)

def onStartPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if config.runner == None:
        config.runner = PVSRunner()
        config.runner.start()
    else:
        dialogs.showError("PVS is already running")

def onStopPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if config.runner != None:
        config.runner.terminate()
        config.runner = None
    else:
        dialogs.showError("PVS is not running")

def onTypecheck(event):  # wxGlade: PVSMainFrame.<event_handler>
    filename = config.notebook.getActiveFilename()
    typecheck(filename)
    #event.Skip()

def onSetPVSLocation(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSetPVSLocation' not implemented!")
    #event.Skip()

def onSaveFileAs(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onSaveFileAs' not implemented!")
    #event.Skip()

def onCoptText(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onCoptText' not implemented!")
    #event.Skip()

def onSaveAllFiles(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.filesbuffermanager.saveAllFiles()
