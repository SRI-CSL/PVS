
# This class manages the toolbar in the main frame

import wx
import util
from constants import *
from images import *
from evhdlr import *

log = util.getLogger(__name__)

class ToolbarManager(wx.ToolBar):
    """This class represents and manages the toolbar in the application"""
    
    def __init__(self, parent, ID):
        wx.ToolBar.__init__(self, parent, ID)
        #self.SetToolBitmapSize((16, 16))
        self.addButtons()
        self.setBindings()
        pub.subscribe(self.update, PUB_UPDATETOOLBAR)
    
    def addButtons(self):
        self.createNewFileToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_NEW, getNewImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Create a new pvs file", EMPTY_STRING)
        self.openFileToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_OPEN, getOpenImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Open a pvs file", EMPTY_STRING)
        self.saveFileToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_SAVE, getSaveImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Save the file", EMPTY_STRING)
        self.saveallFileToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_SAVEALL, getSaveAllImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Save All Files", EMPTY_STRING)
        self.AddSeparator()
        self.cutToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_CUT, getCutImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Cut text", EMPTY_STRING)
        self.copyToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_COPY, getCopyImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Copy text", EMPTY_STRING)
        self.pasteToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_PASTE, getPasteImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Paste text here", EMPTY_STRING)
        self.AddSeparator()
        self.typecheckToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_TYPECHECK, getTypecheckImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Parse and typecheck file", EMPTY_STRING)

    def setBindings(self):
        frame = util.getMainFrame()
        frame.Bind(wx.EVT_TOOL, onCreateNewFile, self.createNewFileToolbarItem)
        frame.Bind(wx.EVT_TOOL, onOpenFile, self.openFileToolbarItem)
        frame.Bind(wx.EVT_TOOL, onSaveFile, self.saveFileToolbarItem)
        frame.Bind(wx.EVT_TOOL, onSaveAllFiles, self.saveallFileToolbarItem)
        frame.Bind(wx.EVT_TOOL, onCutText, self.cutToolbarItem)
        frame.Bind(wx.EVT_TOOL, onCopyText, self.copyToolbarItem)
        frame.Bind(wx.EVT_TOOL, onPasteText, self.pasteToolbarItem)
        frame.Bind(wx.EVT_TOOL, onTypecheck, self.typecheckToolbarItem)
        
    def enableTypecheck(self, value = True):
        self.typecheckToolbarItem.Enable(value)
        
    def update(self, parameters):
        if OPENFILES in parameters:
            value = parameters[OPENFILES] > 0
            self.saveFileToolbarItem.Enable(value)
            self.saveallFileToolbarItem.Enable(value)
            self.cutToolbarItem.Enable(value)
            self.copyToolbarItem.Enable(value)
            self.pasteToolbarItem.Enable(value)
        if PVSMODE in parameters:
            self.typecheckToolbarItem.Enable(parameters[PVSMODE] == PVS_MODE_LISP)
        
        
            

        