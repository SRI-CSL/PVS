
# This class manages how buffers and files are opened and closed,
# by managing the tree representation of files and buffers
# and the tabs that contain open files and editor.

import wx, os.path
from constants import PVS_EXTENSION, TAB_FILES, TAB_BUFFERS, LABEL_FILES_BUFFERS
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
        self.filestree = wx.TreeCtrl(self.filePanel, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.bufferPanel = wx.Panel(self.notebook, wx.ID_ANY)
        self.buffersTree = wx.TreeCtrl(self.bufferPanel, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        
        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        bufferSizer = wx.BoxSizer(wx.HORIZONTAL)
        fileSizer = wx.BoxSizer(wx.HORIZONTAL)
        fileSizer.Add(self.filestree, 1, wx.EXPAND, 0)
        self.filePanel.SetSizer(fileSizer)
        bufferSizer.Add(self.buffersTree, 1, wx.EXPAND, 0)
        self.bufferPanel.SetSizer(bufferSizer)
        self.notebook.AddPage(self.filePanel, TAB_FILES)
        self.notebook.AddPage(self.bufferPanel, TAB_BUFFERS)
        mainSizer.Add(self.notebook, 1, wx.EXPAND, 0)
        mainPanel.SetSizer(mainSizer)
        util.filesTreeManager = FilesTreeManager(self.filestree)
        self.SetSize((200, 300))
        
        self.files = {}
        self.buffers = {}
        
    def addBuffer(self, buffer):
        """add a new PVSBuffer"""
        pass
        
    def addFile(self, fullname):
        """add a new PVSFile, add an entry to the file list, open the file content in the editor, and set it as the active file"""
        log.info("Adding file %s", fullname)
        if not fullname in self.files.keys():
            util.filesTreeManager.addFile(fullname)
            richEditor = util.notebook.addFile(fullname)
            self.files[fullname] = richEditor
        else:
            log.info("File %s is already open", fullname)
        #self.mainFrame.pvseditor.addRedMarker(2)
        self.setFileAsActive(fullname)
        
    def setFileAsActive(self, fullname):
        """make the tab visible for a given file"""
        log.info("Setting file %s as active", fullname)
        util.notebook.showTabForFile(fullname)
        
    def createNewFile(self):
        """create a new file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (util.frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            if not fullname.endswith(PVS_EXTENSION):
                fullname = fullname + PVS_EXTENSION
            log.info("Creating new file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        util.frame.configMenuToolbar(len(self.files))

    def openFile(self):
        """open an existing PVS file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (util.frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            log.info("Opening file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        util.frame.configMenuToolbar(len(self.files))

    def closeFile(self, fullname=None):
        """close an open file"""
        if fullname == None:
            fullname = util.notebook.getActiveFilename()
        log.info("Closing file %s", fullname)
        if fullname in self.files.keys():
            #TODO: First check if the file needs be saved before closing.
            del self.files[fullname]
            util.notebook.closeTabForFile(fullname)
            util.filesTreeManager.removeFile(fullname)
        else:
            log.warning("The file %s is not in %s", fullname, self.files.keys())
        util.frame.configMenuToolbar(len(self.files))
            
    def saveFile(self, fullname=None):
        """save the active file (an active file is one whose tab is visible)"""
        if fullname == None:
            fullname = util.notebook.getActiveFilename()
        else:
            if not fullname in self.files.keys():
                log.error("%s is not even open to be saved.")
                return 
        log.info("Saving file %s", fullname)
        if util.notebook.pages[fullname].styledText.GetModify():
            util.notebook.pages[fullname].styledText.SaveFile(fullname)
        util.filesTreeManager.removeTheoriesFromFile(fullname)
            
    def saveAllFiles(self):
        """save all the open files"""
        log.info("Saving all files")
        for fullname, richEditor in self.files.items():
            if richEditor.styledText.GetModify():
                richEditor.styledText.SaveFile(fullname)
            util.filesTreeManager.removeTheoriesFromFile(fullname)

    def OnClose(self, event):
        """is called when the user clicks on the close icon on top of the frame"""
        if event.CanVeto():
            self.Hide()
            util.preference.setVisibleFilesBuffersTrees(False)
            util.menubar.filesAndBuffersTrees.Check(False)
            event.Veto()
        else:
            #if we don't veto it we allow the event to propogate
            event.Skip() 