# -*- coding: US-ASCII -*-
#
# This class represents the main frame of the editor
#

import wx
import os.path
import constants
from fbmgr import FilesAndBuffersManager
from nbmgr import NotebookManager
from console import PVSConsole
import util
from mmgr import MainFrameMenu
from tbmgr import ToolbarManager
from pfmgr import PreferenceManager
from ptmgr import ProofTreeManager

log = util.getLogger(__name__)

class MainFrame(wx.Frame):
    """The main frame of the application. It consists of a menu and a toolbar, a notebook for all the open
    files and buffers, and a console"""
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.ICONIZE | wx.CAPTION | wx.MINIMIZE | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.MAXIMIZE_BOX | wx.SYSTEM_MENU | wx.RESIZE_BORDER | wx.CLIP_CHILDREN
        wx.Frame.__init__(self, *args, **kwds)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        util.frame = self
        util.preference = PreferenceManager()

        # Menu Bar
        util.menubar = MainFrameMenu()
        self.SetMenuBar(util.menubar)
        # Menu Bar end
        util.statusbar = self.CreateStatusBar(1, 0)
        
        # Tool Bar
        util.toolbar = ToolbarManager(self, wx.ID_ANY)
        self.SetToolBar(util.toolbar)
        # Tool Bar end

        util.filesBuffersManager = FilesAndBuffersManager()
        if util.preference.getVisibleFilesBuffersTrees():
            util.filesBuffersManager.Show()
        util.proofTreeManager = ProofTreeManager()
        if util.preference.getVisibleProofTree():
            util.proofTreeManager.Show()
        
        self.mainPanel = wx.Panel(self, wx.ID_ANY)
        
        util.notebook = NotebookManager(self.mainPanel, wx.ID_ANY, style=0)
        
        util.console = PVSConsole()
        util.console.Show()

        self.__do_layout()
        self.__set_properties()
        
        self.configMenuToolbar(0)
        self.Connect(-1, -1, util.EVT_RESULT_ID, self.onPVSResult)
        self.loadContext()

    def __set_properties(self):
        self.SetTitle(constants.FRAME_TITLE)
        util.statusbar.SetStatusWidths([-1])
        util.console.initializeConsole()
        util.toolbar.Realize()
        if util.preference.getVisibleProofTree() and util.preference.getVisibleFilesBuffersTrees():
            position = self.GetPosition()
            sz = util.filesBuffersManager.GetSize()
            fbPosition = (position[0]-sz[0]-2, position[1])
            util.filesBuffersManager.SetPosition(fbPosition)
            proofPosition = (position[0]-sz[0]-2, position[1] +sz[1] + 2)
            util.proofTreeManager.SetPosition(proofPosition)

    def __do_layout(self):
        self.SetSize((700, 400))
        frameSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainPanelSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainPanelSizer.Add(util.notebook, 1, wx.EXPAND, 0)
        self.mainPanel.SetSizer(mainPanelSizer)        
        frameSizer.Add(self.mainPanel, 3, wx.EXPAND, 0)
        self.SetSizer(frameSizer)
        self.Layout()
        self.Centre()
        
    def enableToolbarButton(self, ID, value = True):
        util.toolbar.Enable(ID, value)
        
    def enableMenuButton(self, ID, value = True):
        util.menubar.Enable(ID, value)

    def setStatusbarText(self, text, location=0):
        log.info("Setting status bar[%d] to: %s"%(location, text))
        util.statusbar.SetStatusText(text, location)
        
    def onPVSResult(self, event):
        """called whenever a PVSResultEvent is received"""
        data = event.data
        message = event.message
        log.info("Event from PVSRunner. Message: %s, Data: %s", message, data)
        if message == constants.MESSAGE_INITIALIZE_CONSOLE:
            util.console.initializeConsole()
        elif message == constants.MESSAGE_PVS_STATUS:
            self.updateFrame(data)
        elif message == constants.MESSAGE_CONSOLE_WRITE_LINE:
            util.console.writeLine(data)
        elif message == constants.MESSAGE_CONSOLE_WRITE_PROMPT:
            util.console.writePrompt(data)            
        else:
            log.warn("Unhandled PVSmessage: %d", message)
            
    def updateFrame(self, status = constants.PVS_MODE_UNKNOWN):
        log.info("Updating frame with PVS status as %s", status)
        self.setStatusbarText(constants.PVS_MODE + status)
        if status == constants.PVS_MODE_OFF:
            log.debug("Disabling pvsin")
            util.console.clearIn()
            util.console.pvsin.SetEditable(False)
            util.toolbar.enableStopPVS(False)
            util.toolbar.enableStartPVS(True)
        else:
            log.debug("Enabling pvsin")
            util.console.pvsin.SetEditable(True)
            util.toolbar.enableStopPVS(True)
            util.toolbar.enableStartPVS(False)

    def loadContext(self):
        """Load .pvseditor and open all the files that were open last time"""
        util.preference.loadContextPreferences()
        openFiles = util.preference.listOfOpenFiles()
        fullnames = []
        context = util.preference.getContext()
        for fn in openFiles:
            fullnames.append(os.path.join(context, fn))
        util.filesBuffersManager.openFiles(fullnames)
        self.configMenuToolbar(openFiles)
        
    def closeContext(self):
        """save .pvseditor and close all the open files"""
        util.preference.saveContextPreferences()
        util.filesBuffersManager.closeAll()

    def configMenuToolbar(self, openFiles):
        mb = util.menubar
        tb = util.toolbar
        value = (openFiles > 0)
        mb.enableCloseFile(value)
        mb.enableUndo(value)
        mb.enableCut(value)
        mb.enableCopy(value)
        mb.enablePaste(value)
        mb.enableSelectAll(value)
        mb.enableFind(value)
        tb.enableSave(value)
        tb.enableSaveAll(value)
        tb.enableCut(value)
        tb.enableCopy(value)
        tb.enablePaste(value)
        #util.toolbar.enableStartPVS(False)
            
    def OnClose(self, event):
        """called when self.Close() is called"""
        if util.ensureFilesAreSavedToPoceed():
            if util.runner != None:
                util.runner.terminate()
                util.runner = None
            util.preference.saveContextPreferences()
            util.preference.saveGlobalPreferences()
            event.Skip()

        