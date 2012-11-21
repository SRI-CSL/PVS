
# This class manages the files tree view

import wx, os.path
from images import getFolderImage, getPVSLogo, getTheoryImage, getFormulaImage
from constants import FULLNAME, ID_L, THEORIES, DECLARATIONS, KIND, FORMULA_DECLARATION
import config

log = config.getLogger(__name__)

class FilesTreeManager:
    def __init__(self, tree):
        imageList = wx.ImageList(16, 16)
        imageList.Add(getFolderImage())
        imageList.Add(getPVSLogo())
        imageList.Add(getTheoryImage())
        imageList.Add(getFormulaImage())        
        self.tree = tree
        self.tree.AssignImageList(imageList)
        self.tree.AddRoot("", 0, -1, wx.TreeItemData({}))
                
    def addFile(self, f):
        log.info("Adding file %s", f)
        root = self.tree.GetRootItem()
        c = self.tree.AppendItem(root, f.filename, 1, -1, wx.TreeItemData({FULLNAME: f.fullname}))
        self.tree.Expand(root)
        
    def removeFile(self, fullname):
        log.info("Removing file %s", fullname)
        fileNode = self.getFileNode(fullname)
        self.tree.Delete(fileNode)
        
    def getFileNode(self, fullname):
        item, cookie = self.tree.GetFirstChild(self.tree.GetRootItem())
        while item.IsOk():
            data = self.tree.GetItemPyData(item)
            if data.has_key(FULLNAME):
                nodeFullename = data[FULLNAME]
                if fullname == nodeFullename:
                    return item
            item = self.tree.GetNextSibling(item)
        log.error("There is no %s in the filetree", fullname)
        return None
        

    def addTheoriesToFile(self, fullname, result):
        fileNode = self.getFileNode(fullname)
        #root = self.tree.GetRootItem()
        if result.has_key(THEORIES):
            theories = result[THEORIES]
            for theory in theories:
                log.info("Adding theory %s to %s", theory[ID_L], fullname)
                theoryNode = self.tree.AppendItem(fileNode, theory[ID_L], 2, -1, wx.TreeItemData(theory))
                declarations = theory[DECLARATIONS]
                for declaration in declarations:
                    kind = declaration[KIND]
                    if kind == FORMULA_DECLARATION:
                        log.info("Adding formula %s to theory %s", declaration[ID_L], theory[ID_L])
                        self.tree.AppendItem(theoryNode, declaration[ID_L], 3, -1, wx.TreeItemData(declaration))
            self.tree.ExpandAllChildren(fileNode)

    
    def removeTheoriesFromFile(self, fullname):
        fileNode = self.getFileNode(fullname)
        self.tree.DeleteChildren(fileNode)
        
