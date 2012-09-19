# -*- coding: US-ASCII -*-
#
# This class represents the main frame of the editor
#

import wx
from constants import *
from filesbuffersmanager import FilesAndBuffersManager
from filestreemanager import FilesTreeManager
from editornotebook import PVSNotebookManager
from pvsconsole import PVSConsole
from eventhandler import *
import config
from mainmenumanager import MainFrameMenu
from toolbarmanager import ToolbarManager
from preferencemanager import PVSIDEPreferenceManager

log = config.getLogger(__name__)

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
# end wxGlade

class PVSMainFrame(wx.Frame):
    def __init__(self, *args, **kwds):
        # begin wxGlade: PVSMainFrame.__init__
        kwds["style"] = wx.ICONIZE | wx.CAPTION | wx.MINIMIZE | wx.CLOSE_BOX | wx.MINIMIZE_BOX | wx.MAXIMIZE_BOX | wx.SYSTEM_MENU | wx.RESIZE_BORDER | wx.CLIP_CHILDREN
        wx.Frame.__init__(self, *args, **kwds)
        config.frame = self

        # Menu Bar
        config.menubar = MainFrameMenu()
        self.SetMenuBar(config.menubar)
        # Menu Bar end
        config.statusbar = self.CreateStatusBar(1, 0)
        
        # Tool Bar
        config.toolbar = ToolbarManager(self, wx.ID_ANY)
        self.SetToolBar(config.toolbar)
        # Tool Bar end
        self.window_1 = wx.SplitterWindow(self, wx.ID_ANY, style=wx.SP_3D | wx.SP_BORDER)
        self.window_1_pane_1 = wx.Panel(self.window_1, wx.ID_ANY)
        self.notebook_4 = wx.Notebook(self.window_1_pane_1, wx.ID_ANY, style=0)
        self.notebook_4_pane_1 = wx.Panel(self.notebook_4, wx.ID_ANY)
        self.filestree = wx.TreeCtrl(self.notebook_4_pane_1, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.notebook_4_pane_2 = wx.Panel(self.notebook_4, wx.ID_ANY)
        self.bufferstree = wx.TreeCtrl(self.notebook_4_pane_2, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.window_1_pane_2 = wx.Panel(self.window_1, wx.ID_ANY)
        self.prooflabel = wx.StaticText(self.window_1_pane_2, wx.ID_ANY, LABEL_PROOF_PANEL)
        self.prooftree = wx.TreeCtrl(self.window_1_pane_2, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.panel_2 = wx.Panel(self, wx.ID_ANY)
        
        config.notebook = PVSNotebookManager(self.panel_2, wx.ID_ANY, style=0)
        ##self.panel_3 = wx.Panel(config.notebook, wx.ID_ANY)
        ##self.pvseditor = PVSRichEditor(self.panel_3, wx.ID_ANY, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2, markers = True)
        
        #self.pvseditor = wx.TextCtrl(config.notebook, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        self.panel_1 = wx.Panel(self, wx.ID_ANY)
        self.label_1 = wx.StaticText(self.panel_1, wx.ID_ANY, LABEL_PVS_CONSOLE)
        config.console = PVSConsole(self.panel_1, wx.ID_ANY)
        pvsout = wx.TextCtrl(config.console, wx.ID_ANY, EMPTY_STRING, style=wx.TE_MULTILINE | wx.TE_READONLY)
        pvsin = wx.TextCtrl(config.console, wx.ID_ANY, EMPTY_STRING, style=wx.TE_PROCESS_ENTER)
        config.console.setPVSOut(pvsout)
        config.console.setPVSIn(pvsin)
        config.console.setBidnings()
        #config.console = wx.TextCtrl(self.panel_4, wx.ID_ANY, EMPTY_STRING, style=wx.TE_PROCESS_ENTER | wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH)

        self.__set_properties()
        self.__do_layout()
        # end wxGlade
        
        config.filesbuffermanager = FilesAndBuffersManager()
        config.filestreemanager = FilesTreeManager(self.filestree)
        self.configMenuToolbar(0)
        config.toolbar.enableSave(False)
        self.Connect(-1, -1, config.EVT_RESULT_ID, self.onPVSResult)
        config.preference = PVSIDEPreferenceManager()

    def __set_properties(self):
        # begin wxGlade: PVSMainFrame.__set_properties
        self.SetTitle(FRAME_TITLE)
        self.SetSize((1000, 800))
        config.statusbar.SetStatusWidths([-1])
        # statusbar fields
        config.console.initializeConsole()
        config.toolbar.Realize()
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: PVSMainFrame.__do_layout
        sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_2 = wx.BoxSizer(wx.VERTICAL)
        sizer_7 = wx.BoxSizer(wx.VERTICAL)
        ##sizer_8 = wx.BoxSizer(wx.VERTICAL)
        sizer_6 = wx.BoxSizer(wx.VERTICAL)
        sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_8 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_10 = wx.BoxSizer(wx.VERTICAL)
        sizer_4.Add(self.filestree, 1, wx.EXPAND, 0)
        self.notebook_4_pane_1.SetSizer(sizer_4)
        sizer_5.Add(self.bufferstree, 1, wx.EXPAND, 0)
        self.notebook_4_pane_2.SetSizer(sizer_5)
        self.notebook_4.AddPage(self.notebook_4_pane_1, TAB_FILES)
        self.notebook_4.AddPage(self.notebook_4_pane_2, TAB_BUFFERS)
        sizer_3.Add(self.notebook_4, 1, wx.EXPAND, 0)
        self.window_1_pane_1.SetSizer(sizer_3)
        sizer_6.Add(self.prooflabel, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_6.Add(self.prooftree, 1, wx.EXPAND, 0)
        self.window_1_pane_2.SetSizer(sizer_6)
        self.window_1.SplitHorizontally(self.window_1_pane_1, self.window_1_pane_2)
        sizer_1.Add(self.window_1, 1, wx.EXPAND, 0)
        
        ##config.notebook.AddPage(self.panel_3, "tab1")
        sizer_8.Add(config.notebook, 1, wx.EXPAND, 0)
        self.panel_2.SetSizer(sizer_8)
        
        sizer_2.Add(self.panel_2, 3, wx.EXPAND, 0)
        sizer_7.Add(self.label_1, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        #sizer_7.Add(config.console, 1, wx.EXPAND, 0)
        sizer_7.Add(config.console, 1, wx.EXPAND, 0)
        self.panel_1.SetSizer(sizer_7)
        ###sizer_9.Add(self.pvseditor, 1, wx.EXPAND, 0)
        sizer_10.Add(config.console.pvsout, 1, wx.EXPAND, 0)
        sizer_10.Add(config.console.pvsin, 0, wx.EXPAND, 0)       
        config.console.SetSizer(sizer_10)        
        ###self.panel_3.SetSizer(sizer_9) 
        sizer_2.Add(self.panel_1, 1, wx.EXPAND, 0)
        sizer_1.Add(sizer_2, 3, wx.EXPAND, 0)
        self.SetSizer(sizer_1)
        self.Layout()
        self.Centre()
        # end wxGlade
        
    def enableToolbarButton(self, ID, value = True):
        config.toolbar.Enable(ID, value)
        
    def enableMenuButton(self, ID, value = True):
        config.menubar.Enable(ID, value)

    def setStatusbarText(self, text, location=0):
        log.info("Setting status bar[%d] to: %s"%(location, text))
        config.statusbar.SetStatusText(text, location)
        
    def onPVSResult(self, event):
        data = event.data
        message = event.message
        log.info("Event from PVSRunner. Message: %s, Data: %s", message, data)
        if message == MESSAGE_INITIALIZE_CONSOLE:
            config.console.initializeConsole()
        elif message == MESSAGE_PVS_STATUS:
            self.updateFrame(data)
        elif message == MESSAGE_CONSOLE_WRITE_LINE:
            config.console.writeLine(data)
        elif message == MESSAGE_CONSOLE_WRITE_PROMPT:
            config.console.writePrompt(data)            
        else:
            log.warn("Unhandled PVSmessage: %d", message)
            
    def updateFrame(self, status = PVS_MODE_UNKNOWN):
        log.info("Updating frame with PVS status as %s", status)
        self.setStatusbarText(PVS_MODE + status)
        if status == PVS_MODE_OFF:
            log.debug("Disabling pvsin")
            config.console.clearIn()
            config.console.pvsin.SetEditable(False)
            config.toolbar.enableStopPVS(False)
            config.toolbar.enableStartPVS(True)
        else:
            log.debug("Enabling pvsin")
            config.console.pvsin.SetEditable(True)
            config.toolbar.enableStopPVS(True)
            config.toolbar.enableStartPVS(False)


    def configMenuToolbar(self, openFiles):
        if openFiles == 0:
            config.menubar.enableCloseFile(False)
            config.menubar.enableUndo(False)
            config.menubar.enableCut(False)
            config.menubar.enableCopy(False)
            config.menubar.enablePaste(False)
            config.menubar.enableSelectAll(False)
            config.menubar.enableFind(False)
            #config.toolbar.enableStartPVS(False)
        elif openFiles > 0:
            config.menubar.enableCloseFile()
            config.menubar.enableUndo()
            config.menubar.enableCut()
            config.menubar.enableCopy()
            config.menubar.enablePaste()
            config.menubar.enableSelectAll()
            config.menubar.enableFind()
        else:
            log.error("openFiles is a negative number: %d", openFiles)
        