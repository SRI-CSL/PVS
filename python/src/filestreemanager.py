import wx, os.path
from images import getFolderImage, getPVSLogo
import config

log = config.getLogger(__name__)

class FilesTreeManager:
    def __init__(self, tree):
        imageList = wx.ImageList(16, 16)
        imageList.Add(getFolderImage())
        imageList.Add(getPVSLogo())
        self.tree = tree
        self.tree.AssignImageList(imageList)
        self.root = self.tree.AddRoot("", 0, -1, wx.TreeItemData({}))
        self.children = {}
                
    def addFile(self, f):
        log.info("Adding file %s", f)
        c = self.tree.AppendItem(self.root, f.filename, 1, -1, wx.TreeItemData({"fullpath": f.fullname}))
        self.children[f.fullname] = c
        self.tree.Expand(self.root)
        
    def removeFile(self, fullname):
        log.info("Removing file %s", fullname)
        c = self.children[fullname]
        del self.children[fullname]
        self.tree.Delete(c)
