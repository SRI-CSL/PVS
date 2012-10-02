
# This class manages how buffers and files are opened and closed,
# by managing the tree represenation of files and buffers
# and the tabs that contain open files and editor.

from pvsfile import PVSFile
from pvsbuffer import PVSBuffer
import wx, os.path
from constants import PVS_EXTENSION, TAB_FILES, TAB_BUFFERS
import common
from filestreemanager import FilesTreeManager

log = common.getLogger(__name__)

class FilesAndBuffersManager(wx.Frame):
    """This class implements a frame with two tabes, one to show a tree of open files and their theories,
    and the other is a list of buffers sent from PVS"""
    
    title = "Files and Buffers"
    
    def __init__(self):
        wx.Frame.__init__(self, wx.GetApp().TopWindow, title=self.title)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        panel = wx.Panel(self, wx.ID_ANY)
        self.notebook_4 = wx.Notebook(panel, wx.ID_ANY, style=0)
        self.notebook_4_pane_1 = wx.Panel(self.notebook_4, wx.ID_ANY)
        self.filestree = wx.TreeCtrl(self.notebook_4_pane_1, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        self.notebook_4_pane_2 = wx.Panel(self.notebook_4, wx.ID_ANY)
        self.bufferstree = wx.TreeCtrl(self.notebook_4_pane_2, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        
        sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_4.Add(self.filestree, 1, wx.EXPAND, 0)
        self.notebook_4_pane_1.SetSizer(sizer_4)
        sizer_5.Add(self.bufferstree, 1, wx.EXPAND, 0)
        self.notebook_4_pane_2.SetSizer(sizer_5)
        self.notebook_4.AddPage(self.notebook_4_pane_1, TAB_FILES)
        self.notebook_4.AddPage(self.notebook_4_pane_2, TAB_BUFFERS)
        sizer_3.Add(self.notebook_4, 1, wx.EXPAND, 0)
        panel.SetSizer(sizer_3)
        common.filestreemanager = FilesTreeManager(self.filestree)
        self.SetSize((200, 300))
        
        self.files = {}
        self.buffers = {}
        
    def addBuffer(self, buffer):
        """add a new PVSBuffer"""
        pass
        
    def addFile(self, fullname):
        """add a new PVSFile"""
        log.info("Adding file %s", fullname)
        if not self.files.has_key(fullname):
            f = PVSFile(fullname)
            self.files[fullname] = f
            common.filestreemanager.addFile(f)
            common.notebook.addFile(f)
        else:
            log.info("File %s is already open", fullname)
        #self.mainFrame.pvseditor.addRedMarker(2)
        self.setFileAsActive(fullname)
        
    def setFileAsActive(self, fullname):
        """make the tab visible for a given file"""
        log.info("Setting file %s as active", fullname)
        common.notebook.showTabForFile(fullname)
        
    def createNewFile(self):
        """create a new file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (common.frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            if not fullname.endswith(PVS_EXTENSION):
                fullname = fullname + PVS_EXTENSION
            log.info("Creating new file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        common.frame.configMenuToolbar(len(self.files))

    def openFile(self):
        """open an existing PVS file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (common.frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            log.info("Opening file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        common.frame.configMenuToolbar(len(self.files))

    def closeFile(self, fullname=None):
        """close an open file"""
        if fullname == None:
            fullname = common.notebook.getActiveFilename()
        log.info("Closing file %s", fullname)
        if self.files.has_key(fullname):
            f = self.files[fullname]
            f.close()
            del self.files[fullname]
            common.notebook.closeTabForFile(fullname)
            common.filestreemanager.removeFile(fullname)
        else:
            log.warning("The file %s is not in %s", fullname, self.files.keys())
        common.frame.configMenuToolbar(len(self.files))
            
    def saveFile(self):
        """save the active file (an active file is one whose tab is visible)"""
        fullname = common.notebook.getActiveFilename()
        log.info("Saving file %s", fullname)
        f = self.files[fullname]
        f.save()
        common.filestreemanager.removeTheoriesFromFile(fullname)
            
    def saveAllFiles(self):
        """save all the open files"""
        log.info("Saving all files")
        for fullname, f in self.files.items():
            f.save()
            common.filestreemanager.removeTheoriesFromFile(fullname)

    def OnClose(self, event):
        """is called when the user clicks on the close icon on top of the frame"""
        if event.CanVeto():
            self.Hide()
            common.preference.setFilesBuffersTrees(False)
            common.menubar.filesAndBuffersTrees.Check(False)
            event.Veto()
        else:
            #if we don't veto it we allow the event to propogate
            event.Skip() 