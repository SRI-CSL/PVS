
# This class manages the toolbar in the main frame

import wx
import common
from constants import *
from images import *
from eventhandler import *

log = common.getLogger(__name__)

class ToolbarManager(wx.ToolBar):
    """This class represents and manages the toolbar in the application"""
    
    def __init__(self, parent, ID):
        wx.ToolBar.__init__(self, parent, ID)
        self.addButtons()
        self.setBindings()
    
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
        self.stopPVSToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_STOPPVS, getStopPVSImage(), getStopPVSImage(False), wx.ITEM_NORMAL, "Stop pvs", EMPTY_STRING)
        self.startPVSToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_STARTPVS, getStartPVSImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Start pvs", EMPTY_STRING)
        self.typecheckToolbarItem = self.AddLabelTool(wx.ID_ANY, LABEL_TYPECHECK, getTypecheckImage(), wx.NullBitmap, wx.ITEM_NORMAL, "Parse and typecheck file", EMPTY_STRING)

    def setBindings(self):
        common.frame.Bind(wx.EVT_TOOL, onCreateNewFile, self.createNewFileToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onOpenFile, self.openFileToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onSaveFile, self.saveFileToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onSaveAllFiles, self.saveallFileToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onCutText, self.cutToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onCoptText, self.copyToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onPasteText, self.pasteToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onStopPVS, self.stopPVSToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onStartPVS, self.startPVSToolbarItem)
        common.frame.Bind(wx.EVT_TOOL, onTypecheck, self.typecheckToolbarItem)

    def enableSave(self, value = True):
        self.saveFileToolbarItem.Enable(value)
        
    def enableSaveAll(self, value = True):
        self.saveallFileToolbarItem.Enable(value)
        
    def enableCut(self, value = True):
        self.cutToolbarItem.Enable(value)
        
    def enableCopy(self, value = True):
        self.copyToolbarItem.Enable(value)
        
    def enablePaste(self, value = True):
        self.pasteToolbarItem.Enable(value)
        
    def enableStartPVS(self, value = True):
        self.startPVSToolbarItem.Enable(value)
        
    def enableStopPVS(self, value = True):
        self.stopPVSToolbarItem.Enable(value)
        
    def enableTypecheck(self, value = True):
        self.pasteToolbarItem.Enable(value)

        