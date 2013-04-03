
# This class manages the files tree view

import wx
from images import getFolderImage, getPVSLogo, getTheoryImage, getFormulaImage
from constants import *
import common
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
        self.tree.AppendItem(root, f.filename, 1, -1, wx.TreeItemData({FULLNAME: f.fullname, KIND: FILE}))
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
                theoryName = theory[ID_L]
                theoryNode = self.tree.AppendItem(fileNode, theoryName, 2, -1, wx.TreeItemData(theory))
                declarations = theory[DECLARATIONS]
                for declaration in declarations:
                    kind = declaration[KIND]
                    if kind == FORMULA_DECLARATION:
                        formulaName = declaration[ID_L]
                        log.info("Adding formula %s to theory %s", formulaName, theoryName)
                        declaration[KIND] = FORMULA
                        declaration[THEORY] = theoryName
                        self.tree.AppendItem(theoryNode, formulaName, 3, -1, wx.TreeItemData(declaration))
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
        status = PVS_MODE_OFF if common.runner == None else common.runner.status
        items = self.getContextMenuItems(status, kind) # each item should be a pair of a label and a callback funtion.
        for label, callback in items:
            ID = wx.ID_ANY
            menu.Append(ID, label, EMPTY_STRING, wx.ITEM_NORMAL)
            wx.EVT_MENU(menu, ID, callback)
        common.filesbuffermanager.PopupMenu(menu, event.GetPoint())
        menu.Destroy()
        
    def getContextMenuItems(self, status, kind):
        items = []
        if status == PVS_MODE_OFF:
            if kind == ROOT:
                pass
            elif kind == FILE:
                items.append((LABEL_CLOSEFILE, self.onCloseFile))
            else:
                log.error("A node with kind %s should not be visible in %s mode", kind, status)
        elif status == PVS_MODE_EDIT:
            if kind == ROOT:
                pass
            elif kind == FILE:
                items.append((LABEL_TYPECHECK, self.onTypecheckFile))
                items.append((LABEL_CLOSEFILE, self.onCloseFile))
            elif kind == THEORY:
                pass
            elif kind == FORMULA:
                items.append((LABEL_PROVE_FORMULA, self.onStartProver))
            else:
                log.error("Unknown kind: %s", kind)
        elif status == PVS_MODE_PROVER:
            if kind == ROOT:
                pass
            elif kind == FILE:
                items.append((LABEL_CLOSEFILE, self.onCloseFile))
            elif kind == THEORY:
                pass
            elif kind == FORMULA:
                pass
            else:
                log.error("Unknown kind: %s", kind)
        else:
            log.error("Unknown mode: %s", status)
        return items
            
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
        
    def onStartProver(self, event):
        """onTypecheckFile is called when the user selects Typecheck in the context menu"""
        data = self.getSelectedNodeData()
        theoryName = data[THEORY]
        formulaName = data[ID_L]        
        commandmanager.startProver(theoryName, formulaName)
        
    
