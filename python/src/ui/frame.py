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
import preference
from ptmgr import ProofTreeManager
import runner
import gui

log = util.getLogger(__name__)

class MainFrame(wx.Frame):
    """The main frame of the application. It consists of a menu and a toolbar, a notebook for all the open
    files and buffers, and a console"""
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.ICONIZE | wx.CAPTION | wx.MINIMIZE | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.MAXIMIZE_BOX | wx.SYSTEM_MENU | wx.RESIZE_BORDER | wx.CLIP_CHILDREN
        wx.Frame.__init__(self, *args, **kwds)
        preference.PreferenceManager()
        gui.manager.setMainFrame(self)
        self.Bind(wx.EVT_CLOSE, self.OnClose)

        # Menu Bar
        menubar = MainFrameMenu()
        gui.manager.setMenubar(menubar)
        self.SetMenuBar(menubar)
        # Menu Bar end
        statusbar = self.CreateStatusBar(1, 0)
        gui.manager.setStatusbar(statusbar)
        
        # Tool Bar
        toolbar = ToolbarManager(self, wx.ID_ANY)
        gui.manager.setToolbar(toolbar)
        self.SetToolBar(toolbar)
        # Tool Bar end

        filesBuffersManager = FilesAndBuffersManager()
        gui.manager.setFilesBuffersManager(filesBuffersManager)
        if preference.manager.getVisibleFilesBuffersTrees():
            filesBuffersManager.Show()
        proofTreeManager = ProofTreeManager()
        gui.manager.setProofTreeManager(proofTreeManager)
        if preference.manager.getVisibleProofTree():
            proofTreeManager.Show()
        
        self.mainPanel = wx.Panel(self, wx.ID_ANY)
        
        notebook = NotebookManager(self.mainPanel, wx.ID_ANY, style=0)
        gui.manager.setNotebook(notebook)
        
        console = PVSConsole()
        gui.manager.setConsole(console)
        console.Show()

        self.__do_layout()
        self.__set_properties()
        
        gui.manager.configMenuToolbar(0)
        
        self.Connect(-1, -1, util.EVT_RESULT_ID, self.onPVSResult)
        gui.manager.loadContext()

    def __set_properties(self):
        self.SetTitle(constants.FRAME_TITLE)
        gui.manager.statusbar.SetStatusWidths([-1])
        gui.manager.console.initializeConsole()
        gui.manager.toolbar.Realize()
        if preference.manager.getVisibleProofTree() and preference.manager.getVisibleFilesBuffersTrees():
            position = self.GetPosition()
            sz = gui.manager.filesBuffersManager.GetSize()
            fbPosition = (position[0]-sz[0]-2, position[1])
            gui.manager.filesBuffersManager.SetPosition(fbPosition)
            proofPosition = (position[0]-sz[0]-2, position[1] +sz[1] + 2)
            gui.manager.proofTreeManager.SetPosition(proofPosition)

    def __do_layout(self):
        self.SetSize((700, 400))
        frameSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainPanelSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainPanelSizer.Add(gui.manager.notebook, 1, wx.EXPAND, 0)
        self.mainPanel.SetSizer(mainPanelSizer)        
        frameSizer.Add(self.mainPanel, 3, wx.EXPAND, 0)
        self.SetSizer(frameSizer)
        self.Layout()
        self.Centre()
         
    def onPVSResult(self, event):
        """called whenever a PVSResultEvent is received"""
        data = event.data
        message = event.message
        log.info("Event from PVSRunner. Message: %s, Data: %s", message, data)
        if message == constants.MESSAGE_INITIALIZE_CONSOLE:
            gui.manager.console.initializeConsole()
        elif message == constants.MESSAGE_PVS_STATUS:
            gui.manager.updateFrame(data)
        elif message == constants.MESSAGE_CONSOLE_WRITE_LINE:
            gui.manager.console.writeLine(data)
        elif message == constants.MESSAGE_CONSOLE_WRITE_PROMPT:
            gui.manager.console.writePrompt(data)            
        else:
            log.warn("Unhandled PVSmessage: %d", message)
                     
                    
    def OnClose(self, event):
        """called when self.Close() is called"""
        if gui.manager.ensureFilesAreSavedToPoceed():
            if runner.manager != None:
                runner.manager.terminate()
                runner.manager = None
            preference.manager.saveContextPreferences()
            preference.manager.saveGlobalPreferences()
            event.Skip()

        