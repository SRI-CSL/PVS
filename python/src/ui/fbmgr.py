
# This class manages how buffers and files are opened and closed,
# by managing the tree representation of files and buffers
# and the tabs that contain open files and editor.

import wx, os.path
from constants import PVS_EXTENSION, TAB_FILES, TAB_BUFFERS, LABEL_FILES_BUFFERS
import preference
import gui
import util
from ftmgr import FilesTreeManager

log = util.getLogger(__name__)

class FilesAndBuffersManager(wx.Frame):
    """This class implements a frame with two tabes, one to show a tree of open files and their theories,
    and the other is a list of buffers sent from PVS"""
    
    def __init__(self):
        wx.Frame.__init__(self, wx.GetApp().TopWindow, title=LABEL_FILES_BUFFERS)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        mainPanel = wx.Panel(self, wx.ID_ANY)
        self.notebook = wx.Notebook(mainPanel, wx.ID_ANY, style=0)
        self.filePanel = wx.Panel(self.notebook, wx.ID_ANY)
        self.filesTree = wx.TreeCtrl(self.filePanel, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.bufferPanel = wx.Panel(self.notebook, wx.ID_ANY)
        self.buffersTree = wx.TreeCtrl(self.bufferPanel, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        
        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        bufferSizer = wx.BoxSizer(wx.HORIZONTAL)
        fileSizer = wx.BoxSizer(wx.HORIZONTAL)
        fileSizer.Add(self.filesTree, 1, wx.EXPAND, 0)
        self.filePanel.SetSizer(fileSizer)
        bufferSizer.Add(self.buffersTree, 1, wx.EXPAND, 0)
        self.bufferPanel.SetSizer(bufferSizer)
        self.notebook.AddPage(self.filePanel, TAB_FILES)
        self.notebook.AddPage(self.bufferPanel, TAB_BUFFERS)
        mainSizer.Add(self.notebook, 1, wx.EXPAND, 0)
        mainPanel.SetSizer(mainSizer)
        filesTreeManager = FilesTreeManager(self.filesTree)
        gui.manager.setFilesTreeManager(filesTreeManager)
        gui.manager.setBuffersTree(self.buffersTree) #TODO: Change this to a manager object later
        self.SetSize((200, 300))
        
        
    def OnClose(self, event):
        """is called when the user clicks on the close icon on top of the frame"""
        if event.CanVeto():
            self.Hide()
            preference.manager.setVisibleFilesBuffersTrees(False)
            gui.manager.menubar.filesAndBuffersTrees.Check(False)
            event.Veto()
        else:
            #if we don't veto it we allow the event to propogate
            event.Skip() 