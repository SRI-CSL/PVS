
# This class manages all the menu items in the main menu of the editor

import wx
from constants import *
from evhdlr import *
import util
import logging
from preference import Preferences
from wx.lib.pubsub import setupkwargs, pub 

class MainFrameMenu(wx.MenuBar):
    """The class implementing and managing the main menu bar in the application"""
    
    def __init__(self):
        wx.MenuBar.__init__(self)
        self.plugins = {}
        self.toolbars = {}
        self._recentContexts = {}
        self.addFileMenu()
        self.addEditMenu()
        self.addViewMenu()
        self.addPVSMenu()
        self.setBindings()
        pub.subscribe(self.update, PUB_UPDATEMENUBAR)
        pub.subscribe(self.showPlugin, PUB_SHOWPLUGIN)
        pub.subscribe(self.showToolbar, PUB_SHOWTOOLBAR)
        pub.subscribe(self.addPluginToViewMenu, PUB_ADDITEMTOVIEWMENU)
        pub.subscribe(self.prepareRecentContextsSubMenu, PUB_UPDATEPVSCONTEXT)
        
    def addFileMenu(self):
        """Adding menu items to File menu"""
        fileMenu = wx.Menu()
        self.newFileMenuItem = fileMenu.Append(wx.ID_NEW, self._makeLabel(LABEL_NEW, "N", True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.openFileMenuItem = fileMenu.Append(wx.ID_OPEN, self._makeLabel(LABEL_OPEN, "O", True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileMenuItem = fileMenu.Append(wx.ID_SAVE, self._makeLabel(LABEL_SAVE, "S"), EMPTY_STRING, wx.ITEM_NORMAL)
        self.saveFileAsMenuItem = fileMenu.Append(wx.ID_SAVEAS, self._makeLabel(LABEL_SAVEAS, None, True), EMPTY_STRING, wx.ITEM_NORMAL)
        self.closeFileMenuItem = fileMenu.Append(wx.ID_CLOSE, self._makeLabel(LABEL_CLOSEFILE, "W"), EMPTY_STRING, wx.ITEM_NORMAL)
        fileMenu.AppendSeparator()
        self.quitMenuItem = fileMenu.Append(wx.ID_ANY, self._makeLabel(LABEL_QUIT, "Q"), EMPTY_STRING, wx.ITEM_NORMAL)
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
        self.viewMenu = wx.Menu()
        preferences = Preferences()
        
        self.pluginMenu = wx.Menu()
        self.viewMenu.AppendMenu(wx.ID_ANY, 'Plugins', self.pluginMenu)
        # Add View Menu to the menu bar:
        self.Append(self.viewMenu, LABEL_VIEW)
        
    def addPVSMenu(self):
        """Adding menu items to PVS menu"""
        pvsMenu = wx.Menu()
        self.changeContextMenuItem =  pvsMenu.Append(wx.ID_ANY, self._makeLabel("Change Context", None, True), EMPTY_STRING, wx.ITEM_NORMAL)

        self.recentContextsMenu = wx.Menu()
        self.prepareRecentContextsSubMenu()
        pvsMenu.AppendMenu(wx.ID_ANY, "Recent Contexts", self.recentContextsMenu)
        
        pvsMenu.AppendSeparator()
        self.typecheckMenuItem = pvsMenu.Append(wx.ID_ANY, LABEL_TYPECHECK, EMPTY_STRING, wx.ITEM_NORMAL)
        self.Append(pvsMenu, PVS_U)
        
    def prepareRecentContextsSubMenu(self):
        try:
            while True: #TODO: Find out if there is a better way to remove all the items from a menu
                item = self.recentContextsMenu.FindItemByPosition(0)
                self.recentContextsMenu.RemoveItem(item)
        except:
            pass
        self._recentContexts = {}
        preferences = Preferences()
        recentContexts = preferences.getRecentContexts()
        logging.debug("Recent Contexts: %s", recentContexts)
        frame = util.getMainFrame()
        for cxt in recentContexts:
            item = self.recentContextsMenu.Append(wx.ID_ANY, cxt, EMPTY_STRING, wx.ITEM_NORMAL)
            self._recentContexts[item.GetId()] = cxt
            frame.Bind(wx.EVT_MENU, self.onRecentContextSelected, item)
            
    def onRecentContextSelected(self, event):
        context = self._recentContexts[event.GetId()]
        PVSCommandManager().changeContext(context)
        
    def addPluginToViewMenu(self, name, callBackFunction):
        logging.debug("Name: %s", name)
        frame = util.getMainFrame()
        item = self.pluginMenu.Append(wx.ID_ANY, name, EMPTY_STRING, wx.ITEM_CHECK)
        self.plugins[name] = item
        self.pluginMenu.Check(item.GetId(), PluginManager().shouldPluginBeVisible(name, PVSCommandManager().pvsMode))
        frame.Bind(wx.EVT_MENU, callBackFunction, item)

    def _makeLabel(self, name, shortcut=None, addDots = False):
        if addDots:
            name = name + DOTDOTDOT
        return name if shortcut is None else "%s\t%s-%s"%(name, CONTROL, shortcut)
        
    def setBindings(self):
        frame = util.getMainFrame()
        frame.Bind(wx.EVT_MENU, onCreateNewFile, self.newFileMenuItem)
        frame.Bind(wx.EVT_MENU, onOpenFile, self.openFileMenuItem)
        frame.Bind(wx.EVT_MENU, onSaveFile, self.saveFileMenuItem)
        frame.Bind(wx.EVT_MENU, onSaveAsFile, self.saveFileAsMenuItem)
        frame.Bind(wx.EVT_MENU, onCloseFile, self.closeFileMenuItem)
        frame.Bind(wx.EVT_MENU, onQuitFrame, self.quitMenuItem)
        
        frame.Bind(wx.EVT_MENU, onUndo, self.undoMenuItem)
        frame.Bind(wx.EVT_MENU, onRedo, self.redoMenuItem)
        frame.Bind(wx.EVT_MENU, onSelectAll, self.selectAllMenuItem)
        frame.Bind(wx.EVT_MENU, onCutText, self.cutMenuItem)
        frame.Bind(wx.EVT_MENU, onCopyText, self.copyMenuItem)
        frame.Bind(wx.EVT_MENU, onPasteText, self.pasteMenuItem)
        frame.Bind(wx.EVT_MENU, onFindText, self.findMenuItem)
        
        frame.Bind(wx.EVT_MENU, onChangeContext, self.changeContextMenuItem)
        frame.Bind(wx.EVT_MENU, onTypecheck, self.typecheckMenuItem)
        
    def update(self, parameters):
        if OPENFILES in parameters:
            value = parameters[OPENFILES] > 0
            self.closeFileMenuItem.Enable(value)
            self.cutMenuItem.Enable(value)
            self.copyMenuItem.Enable(value)
            self.pasteMenuItem.Enable(value)
            self.selectAllMenuItem.Enable(value)
            self.findMenuItem.Enable(value)
            self.undoMenuItem.Enable(value)
            self.redoMenuItem.Enable(value)
        
        if PVSMODE in parameters:
            pvsMode = parameters[PVSMODE]
            if pvsMode == PVS_MODE_OFF:
                pass
            elif pvsMode == PVS_MODE_LISP:
                pass
            elif pvsMode == PVS_MODE_PROVER:
                pass
            elif pvsMode == PVS_MODE_UNKNOWN:
                pass
            else:
                logging.error("pvsMode %s is not recognized", pvsMode)
                
    def showPlugin(self, name, value):
        if name in self.plugins:
            logging.info("Changing the visibility of %s to %s", name, value)
            item = self.plugins[name]
            item.Check(value)
        else:
            logging.warn("No menu option for plugin %s", name)
            
    def showToolbar(self, name, value=True):
        if name in self.toolbars:
            logging.info("Changing the visibility of %s to %s", name, value)
            item = self.toolbars[name]
            item.Check(value)
        else:
            logging.warn("No menu option for toolbar %s", name)
            
