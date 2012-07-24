import wx, os.path
from images import getFolderImage, getPVSLogo
        
class FilesTreeManager:
    def __init__(self, frame):
        self.tree = frame.filestree
        imageList = wx.ImageList(16, 16)
        imageList.Add(getFolderImage())
        imageList.Add(getPVSLogo())
        self.tree.AssignImageList(imageList)
        self.root = self.tree.AddRoot("", 0, -1, wx.TreeItemData({}))
                
    def addFile(self, filename):
        name = os.path.split(filename)
        self.tree.AppendItem(self.root, name[1], 1, -1, wx.TreeItemData({"fullpath": filename}))
        self.tree.Expand(self.root)
        
