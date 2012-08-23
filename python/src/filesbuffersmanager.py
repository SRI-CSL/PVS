
# This class manages how buffers and files are opened and closed,
# by managing the tree represenation of files and buffers
# and the tabs that contain open files and editor.

from pvsfile import PVSFile
from pvsbuffer import PVSBuffer
import wx, os.path
from constants import PVS_EXTENSION
import config

log = config.getLogger(__name__)

class FilesAndBuffersManager:
    
    def __init__(self):
        self.files = {}
        self.buffers = {}
        
    def addBuffer(self, buffer):
        pass
        
    def addFile(self, fullname):
        log.info("Adding file %s", fullname)
        if not self.files.has_key(fullname):
            f = PVSFile(fullname)
            self.files[fullname] = f
            config.filestreemanager.addFile(f)
            config.notebook.addFile(f)
        else:
            log.info("File %s is already open", fullname)
        #self.mainFrame.pvseditor.addRedMarker(2)
        self.setFileAsActive(fullname)
        
    def setFileAsActive(self, fullname):
        log.info("Setting file %s as active", fullname)
        config.notebook.showTabForFile(fullname)
        
    def createNewFile(self):
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (config.frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            if not fullname.endswith(PVS_EXTENSION):
                fullname = fullname + PVS_EXTENSION
            log.info("Creating new file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        config.frame.configMenuToolbar(len(self.files))

    def openFile(self):
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (config.frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            log.info("Opening file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        config.frame.configMenuToolbar(len(self.files))

    def closeFile(self):
        fullname = config.notebook.getActiveFilename()
        log.info("Closing file %s", fullname)
        if self.files.has_key(fullname):
            f = self.files[fullname]
            f.close()
            del self.files[fullname]
            config.notebook.closeTabForFile(fullname)
            config.filestreemanager.removeFile(fullname)
        else:
            log.warning("The file %s is not in %s", fullname, self.files.keys())
        config.frame.configMenuToolbar(len(self.files))
            
    def saveFile(self):
        fullname = config.notebook.getActiveFilename()
        log.info("Saving file %s", fullname)
        f = self.files[fullname]
        f.save()
            
    def saveAllFiles(self):
        log.info("Saving all files")
        for f in self.files.values():
            f.save()
