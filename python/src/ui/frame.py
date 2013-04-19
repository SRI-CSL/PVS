# -*- coding: US-ASCII -*-
#
# This class represents the main frame of the editor
#

import wx
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

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
# end wxGlade

class MainFrame(wx.Frame):
    """The main fram of the application. It consists of a menu and a toolbar, a notebook for all the open
    files and buffers, and a console"""
    def __init__(self, *args, **kwds):
        # begin wxGlade: PVSMainFrame.__init__
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

        util.filesbuffermanager = FilesAndBuffersManager()
        if util.preference.visibleFilesBuffersTrees():
            util.filesbuffermanager.Show()
        util.prooftreemanager = ProofTreeManager()
        if util.preference.visibleProofTree():
            util.prooftreemanager.Show()
        
        self.panel_2 = wx.Panel(self, wx.ID_ANY)
        
        util.notebook = NotebookManager(self.panel_2, wx.ID_ANY, style=0)
        ##self.panel_3 = wx.Panel(util.notebook, wx.ID_ANY)
        ##self.pvseditor = PVSRichEditor(self.panel_3, wx.ID_ANY, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2, markers = True)
        
        #self.pvseditor = wx.TextCtrl(util.notebook, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        self.panel_1 = wx.Panel(self, wx.ID_ANY)
        self.label_1 = wx.StaticText(self.panel_1, wx.ID_ANY, constants.LABEL_PVS_CONSOLE)
        util.console = PVSConsole(self.panel_1, wx.ID_ANY)
        pvsout = wx.TextCtrl(util.console, wx.ID_ANY, constants.EMPTY_STRING, style=wx.TE_MULTILINE | wx.TE_READONLY)
        pvsin = wx.TextCtrl(util.console, wx.ID_ANY, constants.EMPTY_STRING, style=wx.TE_PROCESS_ENTER)
        util.console.setPVSOut(pvsout)
        util.console.setPVSIn(pvsin)
        util.console.setBidnings()
        #util.console = wx.TextCtrl(self.panel_4, wx.ID_ANY, EMPTY_STRING, style=wx.TE_PROCESS_ENTER | wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH)

        self.__do_layout()
        self.__set_properties()
        # end wxGlade
        
        
        self.configMenuToolbar(0)
        util.toolbar.enableSave(False)
        self.Connect(-1, -1, util.EVT_RESULT_ID, self.onPVSResult)

    def __set_properties(self):
        # begin wxGlade: PVSMainFrame.__set_properties
        self.SetTitle(constants.FRAME_TITLE)
        util.statusbar.SetStatusWidths([-1])
        # statusbar fields
        util.console.initializeConsole()
        util.toolbar.Realize()
        if util.preference.visibleProofTree() and util.preference.visibleFilesBuffersTrees():
            position = self.GetPosition()
            #fbPosition = util.filesbuffermanager.GetPosition()
            sz = util.filesbuffermanager.GetSize()
            fbPosition = (position[0]-sz[0]-2, position[1])
            util.filesbuffermanager.SetPosition(fbPosition)
            proofPosition = (position[0]-sz[0]-2, position[1] +sz[1] + 2)
            util.prooftreemanager.SetPosition(proofPosition)
            #self.SetPosition((position[0] + sz[0]+2, position[1]))
        
        # end wxGlade

    def __do_layout(self):
        self.SetSize((700, 600))
        # begin wxGlade: PVSMainFrame.__do_layout
        sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_2 = wx.BoxSizer(wx.VERTICAL)
        sizer_7 = wx.BoxSizer(wx.VERTICAL)
        ##sizer_8 = wx.BoxSizer(wx.VERTICAL)
        sizer_8 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_10 = wx.BoxSizer(wx.VERTICAL)
        #sizer_1.Add(self.window_1, 1, wx.EXPAND, 0)
        
        ##util.notebook.AddPage(self.panel_3, "tab1")
        sizer_8.Add(util.notebook, 1, wx.EXPAND, 0)
        self.panel_2.SetSizer(sizer_8)
        
        sizer_2.Add(self.panel_2, 3, wx.EXPAND, 0)
        sizer_7.Add(self.label_1, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        #sizer_7.Add(util.console, 1, wx.EXPAND, 0)
        sizer_7.Add(util.console, 1, wx.EXPAND, 0)
        self.panel_1.SetSizer(sizer_7)
        ###sizer_9.Add(self.pvseditor, 1, wx.EXPAND, 0)
        sizer_10.Add(util.console.pvsout, 1, wx.EXPAND, 0)
        sizer_10.Add(util.console.pvsin, 0, wx.EXPAND, 0)       
        util.console.SetSizer(sizer_10)        
        ###self.panel_3.SetSizer(sizer_9) 
        sizer_2.Add(self.panel_1, 1, wx.EXPAND, 0)
        sizer_1.Add(sizer_2, 3, wx.EXPAND, 0)
        self.SetSizer(sizer_1)
        self.Layout()
        self.Centre()
        # end wxGlade
        
    def enableToolbarButton(self, ID, value = True):
        util.toolbar.Enable(ID, value)
        
    def enableMenuButton(self, ID, value = True):
        util.menubar.Enable(ID, value)

    def setStatusbarText(self, text, location=0):
        log.info("Setting status bar[%d] to: %s"%(location, text))
        util.statusbar.SetStatusText(text, location)
        
    def onPVSResult(self, event):
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


    def configMenuToolbar(self, openFiles):
        mb = util.menubar
        if openFiles == 0:
            mb.enableCloseFile(False)
            mb.enableUndo(False)
            mb.enableCut(False)
            mb.enableCopy(False)
            mb.enablePaste(False)
            mb.enableSelectAll(False)
            mb.enableFind(False)
            #util.toolbar.enableStartPVS(False)
        elif openFiles > 0:
            mb.enableCloseFile()
            mb.enableUndo()
            mb.enableCut()
            mb.enableCopy()
            mb.enablePaste()
            mb.enableSelectAll()
            mb.enableFind()
        else:
            log.error("openFiles is a negative number: %d", openFiles)
            
    def OnClose(self, event):
        # TODO: if they are unsaved files, ask the user to save them
        if util.runner != None:
            util.runner.terminate()
            util.runner = None
        util.preference.saveContextPreferences()
        util.preference.saveGlobalPreferences()
        event.Skip()

        