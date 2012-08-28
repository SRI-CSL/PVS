
# This file contains all the functions handling menu and toolbar events

import config
from findreplacemanager import FindReplaceManager
from pvsrunner import PVSRunner
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
    if config.pvsrunner != None:
        config.pvsrunner.terminate()
        config.pvsrunner = None
    config.frame.Close()

def onUndo(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.undo()
    #event.Skip()

def onSelectAll(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.selectAll()
    #event.Skip()

def onCutText(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.cut()
    #event.Skip()

def onCopyText(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.copy()
    #event.Skip()

def onPasteText(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.notebook.paste()
    #event.Skip()

def onFindText(event):  # wxGlade: PVSMainFrame.<event_handler>
    FindReplaceManager(None, "khar", "gav").show()

    #config.notebook.find()
    #event.Skip()

def onChangeContext(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onChangeContext' not implemented!")
    #event.Skip()

def onRestoreContextAutomatically(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onRestoreContextAutomatically' not implemented!")
    #event.Skip()

def onStartPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    config.pvsrunner = PVSRunner()
    config.pvsrunner.start()
    #event.Skip()

def onStopPVS(event):  # wxGlade: PVSMainFrame.<event_handler>
    if config.pvsrunner != None:
        config.pvsrunner.terminate()
        config.pvsrunner = None

def onTypecheck(event):  # wxGlade: PVSMainFrame.<event_handler>
    log.info("Event handler `onTypecheck' not implemented!")
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
