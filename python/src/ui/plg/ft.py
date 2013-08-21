import wx
import util
import logging
import pvscomm
from constants import *
from wx.lib.pubsub import setupkwargs, pub 
from ui.plugin import PluginPanel
from ui.images import getFolderImage, getPVSLogo, getTheoryImage, getFormulaImage
from preference import Preferences
from remgr import RichEditorManager

class FilesTreePlugin(PluginPanel):
    """This class provides an API for the files tree that is inside FilesTreeFrame"""
    
    def __init__(self, parent, definition):
        PluginPanel.__init__(self, parent, definition)
        self.tree = wx.TreeCtrl(self, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_LINES_AT_ROOT | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        
        mainSizer = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.tree, 1, wx.EXPAND, 0)

        self.SetSizer(mainSizer)
        
        imageList = wx.ImageList(16, 16)
        imageList.Add(getFolderImage())
        imageList.Add(getPVSLogo())
        imageList.Add(getTheoryImage())
        imageList.Add(getFormulaImage())
        self.tree.AssignImageList(imageList)
        self.tree.Bind(wx.EVT_TREE_ITEM_MENU, self.showContextMenu)
        self.clear()
        pub.subscribe(self.addFile, PUB_ADDFILE)
        pub.subscribe(self.clear, PUB_CLOSEALLFILES)
        pub.subscribe(self.removeFile, PUB_CLOSEFILE)
        pub.subscribe(self.onFileSaved, PUB_FILESAVED)
        pub.subscribe(self.addTheoriesToFileTree, PUB_FILETYPECHECKED)
        
    def clear(self):
        self.tree.DeleteAllItems()
        self.tree.AddRoot("", 0, -1, wx.TreeItemData({KIND: ROOT}))
                
    def addFile(self, fullname):
        """add a file to the tree"""
        logging.info("Adding file %s", fullname)
        root = self.tree.GetRootItem()
        if self.getFileNode(fullname) is None:
            self.tree.AppendItem(root, util.getFilenameFromFullPath(fullname), 1, -1, wx.TreeItemData({FULLNAME: fullname, KIND: FILE}))
        self.tree.Expand(root)
        
    def removeFile(self, fullname):
        """remove a file from the tree"""
        logging.info("Removing file %s", fullname)
        fileNode = self.getFileNode(fullname)
        self.tree.Delete(fileNode)
        
    def getFileNode(self, fullname):
        """return the tree node for a given file"""
        item = self.tree.GetFirstChild(self.tree.GetRootItem())[0]
        while item.IsOk():
            data = self.tree.GetItemPyData(item)
            if FULLNAME in data:
                nodeFullname = data[FULLNAME]
                if fullname == nodeFullname:
                    return item
            item = self.tree.GetNextSibling(item)
        logging.info("There is no %s in the filetree", fullname)
        return None
                
    def showContextMenu(self, event):
        """display a relevant context menu when the user right-clicks on a node"""
        item = event.GetItem()
        data = self.tree.GetItemPyData(item)
        logging.info("Event data: %s", data)
        kind = data[KIND]
        menu = wx.Menu()
        status = pvscomm.PVSCommandManager().pvsMode
        items = self.getContextMenuItems(status, kind) # each item should be a pair of a label and a callback function.
        for label, callback in items:
            if isinstance(callback, wx.Menu):
                menu.AppendMenu(wx.ID_ANY, label, callback)
            else:
                ID = wx.ID_ANY
                menu.Append(ID, label, EMPTY_STRING, wx.ITEM_NORMAL)
                wx.EVT_MENU(menu, ID, callback)
        self.tree.GetParent().PopupMenu(menu, event.GetPoint())
        menu.Destroy()
        
    def getContextMenuItems(self, status, kind):
        items = []
        items.append((LABEL_CLOSEFILE, self.onCloseFile))
        if status == PVS_MODE_OFF:
            if kind == ROOT:
                pass
            elif kind == FILE:
                pass
            else:
                logging.error("A node with kind %s should not be visible in %s mode", kind, status)
        elif status == PVS_MODE_UNKNOWN:
            if kind == ROOT:
                pass
        elif status == PVS_MODE_LISP:
            if kind == ROOT:
                pass
            elif kind == FILE:
                items.append((LABEL_TYPECHECK, self.onTypecheckFile))
                items.append((LABEL_CLOSEFILE, self.onCloseFile))
            elif kind == THEORY.lower():
                pass
            elif kind == FORMULA.lower():
                items.append((LABEL_PROVE_FORMULA, self.onStartProver))
            else:
                logging.error("Unknown kind: %s", kind)
        elif status == PVS_MODE_PROVER:
            if kind == ROOT:
                pass
            elif kind == FILE:
                items.append((LABEL_CLOSEFILE, self.onCloseFile))
            elif kind == THEORY:
                pass
            elif kind == FORMULA.lower():
                pass
            else:
                logging.error("Unknown kind: %s", kind)
        else:
            logging.error("Unknown mode: %s", status)
        return items
            
    def getSelectedNodeData(self):
        """return the node that is currently selected"""
        node = self.tree.GetSelection()
        data = self.tree.GetItemPyData(node)
        return data
    
    def onCloseFile(self, event):
        """onCloseFile is called when the user selects Close in the context menu"""
        nodeFullname = self.getSelectedNodeData()[FULLNAME]
        RichEditorManager().handleCloseFileRequest(nodeFullname)
        
    def onTypecheckFile(self, event):
        """onTypecheckFile is called when the user selects Typecheck in the context menu"""
        nodeFullname = self.getSelectedNodeData()[FULLNAME]
        pvscomm.PVSCommandManager().typecheck(nodeFullname)
        
    def onStartProver(self, event):
        """onTypecheckFile is called when the user selects Typecheck in the context menu"""
        data = self.getSelectedNodeData()
        theoryName = data[THEORY.lower()]
        formulaName = data[ID_L]        
        pvscomm.PVSCommandManager().startProver(theoryName, formulaName)
        
    def addTheoriesToFileTree(self, fullname, result):
        """addTheoriesToFileTree is called after typechecking a file and asking for the declarations in that file"""
        fileNode = self.getFileNode(fullname)
        for item in result :
            if "theory" in item:
                theory = item["theory"]
                theoryName = theory["id"]
                logging.info("Adding theory %s to %s", theoryName, fullname)
                declarations = theory["decls"]
                theoryNode = self.tree.AppendItem(fileNode, theoryName, 2, -1, wx.TreeItemData(theory))
                for declaration in declarations:
                    kind = declaration["kind"]
                    if kind == "formula":
                        place = declaration["place"]
                        formulaName = declaration["id"]
                        declaration["theory"] = theoryName
                        self.tree.AppendItem(theoryNode, formulaName, 3, -1, wx.TreeItemData(declaration))
        self.tree.ExpandAllChildren(fileNode)        
        
    def onFileSaved(self, fullname, oldname=None):
        """remove the theories nodes from a file node"""
        fileNode = self.getFileNode(fullname) if oldname is None else self.getFileNode(oldname)
        self.tree.DeleteChildren(fileNode)
        if oldname is not None:
            self.tree.SetItemPyData(fileNode, wx.TreeItemData({FULLNAME: fullname, KIND: FILE}))
            self.tree.SetItemText(fileNode, util.getFilenameFromFullPath(fullname))
            
                                    