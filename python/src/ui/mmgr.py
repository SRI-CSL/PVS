
# This class manages all the menu items in the main menu of the editor

import wx
from constants import *
from evhdlr import *
import util
import preference
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
        self.newFileMenuItem = fileMenu.Append(wx.ID_NEW, self._makeLabel(LABEL_NEW, "N", True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.openFileMenuItem = fileMenu.Append(wx.ID_OPEN, self._makeLabel(LABEL_OPEN, "O", True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileMenuItem = fileMenu.Append(wx.ID_SAVE, self._makeLabel(LABEL_SAVE, "S"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileAsMenuItem = fileMenu.Append(wx.ID_SAVEAS, self._makeLabel(LABEL_SAVEAS, None, True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.closeFileMenuItem = fileMenu.Append(wx.ID_CLOSE, self._makeLabel(LABEL_CLOSEFILE, "W"), EMPTY_STRING, wx.ITEM_NORMAL)
        fileMenu.AppendSeparator()
        self.quitMenuItem = fileMenu.Append(wx.ID_EXIT, self._makeLabel(LABEL_QUIT, "Q"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(fileMenu, LABEL_FILE)
 
    def addEditMenu(self):
        """Adding menu items to Edit menu"""
        editMenu = wx.Menu()
        self.undoMenuItem = editMenu.Append(wx.ID_UNDO, self._makeLabel(LABEL_UNDO, "U"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.redoMenuItem = editMenu.Append(wx.ID_UNDO, self._makeLabel(LABEL_REDO, SHIFT + "-Z"), EMPTY_STRING, wx.ITEM_NORMAL)
        editMenu.AppendSeparator()
        self.cutMenuItem = editMenu.Append(wx.ID_CUT, self._makeLabel(LABEL_CUT, "X"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.copyMenuItem = editMenu.Append(wx.ID_COPY, self._makeLabel(LABEL_COPY, "C"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.pasteMenuItem = editMenu.Append(wx.ID_PASTE, self._makeLabel(LABEL_PASTE, "V"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.selectAllMenuItem = editMenu.Append(wx.ID_SELECTALL, self._makeLabel(LABEL_SELECTALL, "A"), EMPTY_STRING, wx.ITEM_NORMAL)
        editMenu.AppendSeparator()
        self.findMenuItem = editMenu.Append(wx.ID_FIND, self._makeLabel(LABEL_FIND, "F"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(editMenu, LABEL_EDIT)

    def addViewMenu(self):
        """Adding menu items to View menu"""
        viewMenu = wx.Menu()
        self.filesAndBuffersTrees = viewMenu.Append(wx.ID_ANY, "Files and Buffers Trees", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.filesAndBuffersTrees.GetId(), preference.manager.getVisibleFilesBuffersTrees())
        
        self.proofTree = viewMenu.Append(wx.ID_ANY, "Proof Tree", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.proofTree.GetId(), preference.manager.getVisibleProofTree())

        self.toolbar = viewMenu.Append(wx.ID_ANY, "Toolbar", EMPTY_STRING, wx.ITEM_CHECK)
        viewMenu.Check(self.toolbar.GetId(), preference.manager.getVisibleToolbar())
        self.Append(viewMenu, LABEL_VIEW)

    def addPVSMenu(self):
        """Adding menu items to PVS menu"""
        pvsMenu = wx.Menu()
        self.changeContextMenuItem =  pvsMenu.Append(wx.ID_ANY, self._makeLabel("Change Context", None, True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.restoreContextMenuItem = pvsMenu.Append(wx.ID_ANY, "Restore Context Automatically", EMPTY_STRING, wx.ITEM_CHECK)
        pvsMenu.Check(self.restoreContextMenuItem.GetId(), preference.manager.getContextPreferencesRestoredAutomatically())
        pvsMenu.AppendSeparator()
        self.startPVSMenuItem = pvsMenu.Append(wx.ID_ANY, LABEL_STARTPVS, EMPTY_STRING, wx.ITEM_NORMAL)
        self.typecheckMenuItem = pvsMenu.Append(wx.ID_ANY, LABEL_TYPECHECK, EMPTY_STRING, wx.ITEM_NORMAL)
        pvsMenu.AppendSeparator()
        self.setPVSLocationMenuItem = pvsMenu.Append(wx.ID_ANY, self._makeLabel("Set PVS Location", None, True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(pvsMenu, PVS_U)
        
    def _makeLabel(self, name, shortcut=None, addDots = False):
        if addDots:
            name = name + DOTDOTDOT
        return name if shortcut == None else "%s\t%s-%s"%(name, CONTROL, shortcut)
        
    def setBindings(self):
        gui.manager.frame.Bind(wx.EVT_MENU, onCreateNewFile, self.newFileMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onOpenFile, self.openFileMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onSaveFile, self.saveFileMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onSaveAsFile, self.saveFileAsMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onCloseFile, self.closeFileMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onQuitFrame, self.quitMenuItem)
        
        gui.manager.frame.Bind(wx.EVT_MENU, onUndo, self.undoMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onRedo, self.redoMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onSelectAll, self.selectAllMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onCutText, self.cutMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onCopyText, self.copyMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onPasteText, self.pasteMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onFindText, self.findMenuItem)
        
        gui.manager.frame.Bind(wx.EVT_MENU, onToggleViewFilesAndBuffersTrees, self.filesAndBuffersTrees)
        gui.manager.frame.Bind(wx.EVT_MENU, onToggleViewProofTree, self.proofTree)
        gui.manager.frame.Bind(wx.EVT_MENU, onToggleViewToolbar, self.toolbar)
        
        gui.manager.frame.Bind(wx.EVT_MENU, onChangeContext, self.changeContextMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onContextPreferencesRestoredAutomatically, self.restoreContextMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onStartPVS, self.startPVSMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onTypecheck, self.typecheckMenuItem)
        gui.manager.frame.Bind(wx.EVT_MENU, onSetPVSLocation, self.setPVSLocationMenuItem)
        
    def enableCloseFile(self, value=True):
        self.closeFileMenuItem.Enable(value)

    def enableUndo(self, value=True):
        self.undoMenuItem.Enable(value)

    def enableRedo(self, value=True):
        self.redoMenuItem.Enable(value)

    def enableCut(self, value=True):
        self.cutMenuItem.Enable(value)

    def enableCopy(self, value=True):
        self.copyMenuItem.Enable(value)

    def enablePaste(self, value=True):
        self.pasteMenuItem.Enable(value)

    def enableSelectAll(self, value=True):
        self.selectAllMenuItem.Enable(value)

    def enableFind(self, value=True):
        self.findMenuItem.Enable(value)



