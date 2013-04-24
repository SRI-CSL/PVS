
# This class manages all the menu items in the main menu of the editor

import wx
from constants import *
from evhdlr import *
import util

log = util.getLogger(__name__)

class MainFrameMenu(wx.MenuBar):
    """The class implementing and managing the main menu bar in the application"""
    
    def __init__(self):
        wx.MenuBar.__init__(self)
        self.addFileMenu()
        self.addEditMenu()
        self.addViewMenu()
        self.addPVSMenu()
        self.setBindings()
        
    def addFileMenu(self):
        """Adding menu items to File menu"""
        fileMenu = wx.Menu()
        self.newFileMenuItem = fileMenu.Append(wx.ID_NEW, LABEL_NEW + DOTDOTDOT + "\tCtrl-N", EMPTY_STRING, wx.ITEM_NORMAL)
        self.openFileMenuItem = fileMenu.Append(wx.ID_OPEN, LABEL_OPEN + DOTDOTDOT + "\tCtrl-O", EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileMenuItem = fileMenu.Append(wx.ID_SAVE, LABEL_SAVE + "\tCtrl-S", EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileAsMenuItem = fileMenu.Append(wx.ID_SAVEAS, LABEL_SAVEAS + DOTDOTDOT, EMPTY_STRING, wx.ITEM_NORMAL)
        self.closeFileMenuItem = fileMenu.Append(wx.ID_CLOSE, LABEL_CLOSEFILE + "\tCtrl-W", EMPTY_STRING, wx.ITEM_NORMAL)
        fileMenu.AppendSeparator()
        self.quitMenuItem = fileMenu.Append(wx.ID_EXIT, LABEL_QUIT + "\tCtrl-Q", EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(fileMenu, LABEL_FILE)
 
    def addEditMenu(self):
        """Adding menu items to Edit menu"""
        editMenu = wx.Menu()
        self.undoMenuItem = editMenu.Append(wx.ID_UNDO, LABEL_UNDO + "\tCtrl-Z", EMPTY_STRING, wx.ITEM_NORMAL)
        editMenu.AppendSeparator()
        self.cutMenuItem = editMenu.Append(wx.ID_CUT, LABEL_CUT + "\tCtrl-X", EMPTY_STRING, wx.ITEM_NORMAL)
        self.copyMenuItem = editMenu.Append(wx.ID_COPY, LABEL_COPY + "\tCtrl-C", EMPTY_STRING, wx.ITEM_NORMAL)
        self.pasteMenuItem = editMenu.Append(wx.ID_PASTE, LABEL_PASTE + "\tCtrl-V", EMPTY_STRING, wx.ITEM_NORMAL)
        self.selectAllMenuItem = editMenu.Append(wx.ID_SELECTALL, LABEL_SELECTALL + "\tCtrl-A", EMPTY_STRING, wx.ITEM_NORMAL)
        editMenu.AppendSeparator()
        self.findMenuItem = editMenu.Append(wx.ID_FIND, LABEL_FIND + "\tCtrl-F", EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(editMenu, LABEL_EDIT)

    def addViewMenu(self):
        """Adding menu items to View menu"""
        viewMenu = wx.Menu()
        self.filesAndBuffersTrees = viewMenu.Append(wx.ID_ANY, "Files and Buffers Trees", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.filesAndBuffersTrees.GetId(), util.preference.getVisibleFilesBuffersTrees())
        
        self.proofTree = viewMenu.Append(wx.ID_ANY, "Proof Tree", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.proofTree.GetId(), util.preference.getVisibleProofTree())

        self.toolbar = viewMenu.Append(wx.ID_ANY, "Toolbar", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.toolbar.GetId(), util.preference.getVisibleToolbar())
        self.Append(viewMenu, LABEL_VIEW)

    def addPVSMenu(self):
        """Adding menu items to PVS menu"""
        pvsMenu = wx.Menu()
        self.changeContextMenuItem =  pvsMenu.Append(wx.ID_ANY, "Change Context...", EMPTY_STRING, wx.ITEM_NORMAL)
        self.restoreContextMenuItem = pvsMenu.Append(wx.ID_ANY, "Restore Context Automatically", EMPTY_STRING, wx.ITEM_CHECK)
        pvsMenu.Check(self.restoreContextMenuItem.GetId(), util.preference.getContextPreferencesRestoredAutomatically())
        pvsMenu.AppendSeparator()
        self.startPVSMenuItem = pvsMenu.Append(wx.ID_ANY, LABEL_STARTPVS, EMPTY_STRING, wx.ITEM_NORMAL)
        self.typecheckMenuItem = pvsMenu.Append(wx.ID_ANY, LABEL_TYPECHECK, EMPTY_STRING, wx.ITEM_NORMAL)
        pvsMenu.AppendSeparator()
        self.setPVSLocationMenuItem = pvsMenu.Append(wx.ID_ANY, "Set PVS Location...", EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(pvsMenu, PVS_U)
        
    def setBindings(self):
        util.frame.Bind(wx.EVT_MENU, onCreateNewFile, self.newFileMenuItem)
        util.frame.Bind(wx.EVT_MENU, onOpenFile, self.openFileMenuItem)
        util.frame.Bind(wx.EVT_MENU, onSaveFile, self.saveFileMenuItem)
        util.frame.Bind(wx.EVT_MENU, onSaveAsFile, self.saveFileAsMenuItem)
        util.frame.Bind(wx.EVT_MENU, onCloseFile, self.closeFileMenuItem)
        util.frame.Bind(wx.EVT_MENU, onQuitFrame, self.quitMenuItem)
        
        util.frame.Bind(wx.EVT_MENU, onUndo, self.undoMenuItem)
        util.frame.Bind(wx.EVT_MENU, onSelectAll, self.selectAllMenuItem)
        util.frame.Bind(wx.EVT_MENU, onCutText, self.cutMenuItem)
        util.frame.Bind(wx.EVT_MENU, onCopyText, self.copyMenuItem)
        util.frame.Bind(wx.EVT_MENU, onPasteText, self.pasteMenuItem)
        util.frame.Bind(wx.EVT_MENU, onFindText, self.findMenuItem)
        
        util.frame.Bind(wx.EVT_MENU, onToggleViewFilesAndBuffersTrees, self.filesAndBuffersTrees)
        util.frame.Bind(wx.EVT_MENU, onToggleViewProofTree, self.proofTree)
        util.frame.Bind(wx.EVT_MENU, onToggleViewToolbar, self.toolbar)
        
        util.frame.Bind(wx.EVT_MENU, onChangeContext, self.changeContextMenuItem)
        util.frame.Bind(wx.EVT_MENU, onContextPreferencesRestoredAutomatically, self.restoreContextMenuItem)
        util.frame.Bind(wx.EVT_MENU, onStartPVS, self.startPVSMenuItem)
        util.frame.Bind(wx.EVT_MENU, onTypecheck, self.typecheckMenuItem)
        util.frame.Bind(wx.EVT_MENU, onSetPVSLocation, self.setPVSLocationMenuItem)
        
    def enableCloseFile(self, value = True):
        self.closeFileMenuItem.Enable(value)

    def enableUndo(self, value = True):
        self.undoMenuItem.Enable(value)

    def enableCut(self, value = True):
        self.cutMenuItem.Enable(value)

    def enableCopy(self, value = True):
        self.copyMenuItem.Enable(value)

    def enablePaste(self, value = True):
        self.pasteMenuItem.Enable(value)

    def enableSelectAll(self, value = True):
        self.selectAllMenuItem.Enable(value)

    def enableFind(self, value = True):
        self.findMenuItem.Enable(value)



