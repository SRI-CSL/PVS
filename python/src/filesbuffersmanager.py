from pvsfile import PVSFile
from pvsbuffer import PVSBuffer
import wx, os.path

class FilesAndBuffersManager:
    def __init__(self, frame):
        self.files = {}
        self.buffers = {}
        self.mainFrame = frame
        
    def addBuffer(self, buffer):
        pass
        
    def addFile(self, filename):
        f = PVSFile(filename)
        self.files[filename] = f
        self.mainFrame.filesTreeManager.addFile(filename)
        self.mainFrame.pvsNotebookManager.addFile(f)
        #self.mainFrame.pvseditor.addRedMarker(2)
        
    def createNewFile(self):
        extension = ".pvs"
        filters = "PVS files (*" + extension + ")|*" + extension
        dialog = wx.FileDialog ( None, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
        if dialog.ShowModal() == wx.ID_OK:
            filename = dialog.GetPath()
            if not filename.endswith(extension):
                filename = filename + extension
            self.addFile(filename)
        else:
            print 'Nothing was selected.'
        dialog.Destroy()

    def openFile(self):
        extension = ".pvs"
        filters = "PVS files (*" + extension + ")|*" + extension
        dialog = wx.FileDialog ( None, "Open PVS file", wildcard = filters, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            filename = dialog.GetPath()
            self.addFile(filename)
        else:
            print 'Nothing was selected.'
        dialog.Destroy()

    def closeFile(self, filename):
        if self.files.has_key(filename):
            f = self.files[filename]
            f.close()
            del self.files[filename]
            
    def saveAllFiles(self):
        for f in self.files.values():
            f.save()
