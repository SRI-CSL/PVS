
# This class manages the files tree view

import wx, os.path
from images import getFolderImage, getPVSLogo, getTheoryImage, getFormulaImage
from constants import FULLNAME, ID_L, THEORIES, DECLARATIONS, KIND, FORMULA_DECLARATION, FILE
from constants import THEORY, FORMULA, ROOT, EMPTY_STRING, LABEL_CLOSEFILE, LABEL_TYPECHECK
import common, eventhandler
import commandmanager

log = common.getLogger(__name__)

class FilesTreeManager:
    """This class provides an api to the files tree that is inside FilesAndBuffersManager"""
    
    def __init__(self, tree):
        imageList = wx.ImageList(16, 16)
        imageList.Add(getFolderImage())
        imageList.Add(getPVSLogo())
        imageList.Add(getTheoryImage())
        imageList.Add(getFormulaImage())        
        self.tree = tree
        self.tree.AssignImageList(imageList)
        self.tree.AddRoot("", 0, -1, wx.TreeItemData({KIND: ROOT}))
        self.tree.Bind(wx.EVT_TREE_ITEM_MENU, self.showContextMenu)
                
    def addFile(self, f):
        """add a file to the tree"""
        log.info("Adding file %s", f)
        root = self.tree.GetRootItem()
        c = self.tree.AppendItem(root, f.filename, 1, -1, wx.TreeItemData({FULLNAME: f.fullname, KIND: FILE}))
        self.tree.Expand(root)
        
    def removeFile(self, fullname):
        """remove a file from the tree"""
        log.info("Removing file %s", fullname)
        fileNode = self.getFileNode(fullname)
        self.tree.Delete(fileNode)
        
    def getFileNode(self, fullname):
        """return the tree node for a given file"""
        item = self.tree.GetFirstChild(self.tree.GetRootItem())[0]
        while item.IsOk():
            data = self.tree.GetItemPyData(item)
            if data.has_key(FULLNAME):
                nodeFullname = data[FULLNAME]
                if fullname == nodeFullname:
                    return item
            item = self.tree.GetNextSibling(item)
        log.error("There is no %s in the filetree", fullname)
        return None
        

    def addTheoriesToFile(self, fullname, result):
        """addTheoriesToFile is called after typechecking a file and asking for the declarations in that file"""
        fileNode = self.getFileNode(fullname)
        #root = self.tree.GetRootItem()
        if result.has_key(THEORIES):
            theories = result[THEORIES]
            for theory in theories:
                log.info("Adding theory %s to %s", theory[ID_L], fullname)
                theory[KIND] = THEORY
                theoryNode = self.tree.AppendItem(fileNode, theory[ID_L], 2, -1, wx.TreeItemData(theory))
                declarations = theory[DECLARATIONS]
                for declaration in declarations:
                    kind = declaration[KIND]
                    if kind == FORMULA_DECLARATION:
                        log.info("Adding formula %s to theory %s", declaration[ID_L], theory[ID_L])
                        declaration[KIND] = FORMULA
                        self.tree.AppendItem(theoryNode, declaration[ID_L], 3, -1, wx.TreeItemData(declaration))
            self.tree.ExpandAllChildren(fileNode)

    
    def removeTheoriesFromFile(self, fullname):
        """remove the theories nodes from a file node"""
        fileNode = self.getFileNode(fullname)
        self.tree.DeleteChildren(fileNode)
        
    def showContextMenu(self, event):
        """display a relevant context menu when the user right-clicks on a node"""
        item = event.GetItem()
        data = self.tree.GetItemPyData(item)
        log.info("Event data: %s", data)
        kind = data[KIND]
        menu = wx.Menu()
        if kind == ROOT:
            pass
        elif kind == FILE:
            ID1 = wx.ID_ANY
            menu.Append(ID1, LABEL_TYPECHECK, EMPTY_STRING, wx.ITEM_NORMAL)
            wx.EVT_MENU(menu, ID1, self.onTypecheckFile)
            ID2 = wx.ID_ANY
            menu.Append(ID2, LABEL_CLOSEFILE, EMPTY_STRING, wx.ITEM_NORMAL)
            wx.EVT_MENU(menu, ID2, self.onCloseFile)
        elif kind == THEORY:
            pass
        elif kind == FORMULA:
            pass
        else:
            log.error("Unknown type: %s", kind)
        common.filesbuffermanager.PopupMenu(menu, event.GetPoint())
        menu.Destroy()
        
    def getSelectedNodeData(self):
        """return the node that is currently selected"""
        node = self.tree.GetSelection()
        data = self.tree.GetItemPyData(node)
        return data
        
    def onCloseFile(self, event):
        """onCloseFile is called when the user selects Close in the context menu"""
        nodeFullname = self.getSelectedNodeData()[FULLNAME]
        common.filesbuffermanager.closeFile(nodeFullname)
        
    def onTypecheckFile(self, event):
        """onTypecheckFile is called when the user selects Typecheck in the context menu"""
        nodeFullname = self.getSelectedNodeData()[FULLNAME]
        commandmanager.typecheck(nodeFullname)
        
