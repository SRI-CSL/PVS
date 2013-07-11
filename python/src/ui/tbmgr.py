
# This class manages the toolbar in the main frame

import wx
import util
from constants import *
from images import *
from evhdlr import *
import wx.lib.agw.aui as aui

log = util.getLogger(__name__)

class ToolbarManager():
    """This class represents and manages the toolbar in the application"""
    EDITTOOLBAR = "editToolbar"
    PVSTOOLBAR = "pvsToolbar"
    
    __shared_state = {}
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "toolbars" in self.__dict__:
            self.toolbars = []
            
    def initializeEditToolbar(self):
        if not ToolbarManager.EDITTOOLBAR in self.toolbars:
            frame = util.getMainFrame()
            editToolbar = aui.AuiToolBar(frame, wx.ID_ANY)
            editToolbar.SetToolBitmapSize((16, 16))
            editToolbar.createNewFileToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_NEW, getNewImage(), short_help_string="Create a new pvs file")
            editToolbar.openFileToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_OPEN, getOpenImage(), short_help_string="Open a pvs file")
            editToolbar.saveFileToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_SAVE, getSaveImage(), short_help_string="Save the file")
            editToolbar.saveallFileToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_SAVEALL, getSaveAllImage(), short_help_string="Save All Files")
            editToolbar.AddSeparator()
            editToolbar.cutToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_CUT, getCutImage(), short_help_string="Cut text")
            editToolbar.copyToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_COPY, getCopyImage(), short_help_string="Copy text")
            editToolbar.pasteToolbarItem = editToolbar.AddSimpleTool(wx.ID_ANY, LABEL_PASTE, getPasteImage(), short_help_string="Paste text here")
            frame.Bind(wx.EVT_TOOL, onCreateNewFile, editToolbar.createNewFileToolbarItem)
            frame.Bind(wx.EVT_TOOL, onOpenFile, editToolbar.openFileToolbarItem)
            frame.Bind(wx.EVT_TOOL, onSaveFile, editToolbar.saveFileToolbarItem)
            frame.Bind(wx.EVT_TOOL, onSaveAllFiles, editToolbar.saveallFileToolbarItem)
            frame.Bind(wx.EVT_TOOL, onCutText, editToolbar.cutToolbarItem)
            frame.Bind(wx.EVT_TOOL, onCopyText, editToolbar.copyToolbarItem)
            frame.Bind(wx.EVT_TOOL, onPasteText, editToolbar.pasteToolbarItem)
            self.toolbars.append(ToolbarManager.EDITTOOLBAR)
            return editToolbar
        else:
            return self.getToolbar(ToolbarManager.EDITTOOLBAR)

    def initializePVSToolbar(self):
        if not ToolbarManager.PVSTOOLBAR in self.toolbars:
            frame = util.getMainFrame()
            pvsToolbar = aui.AuiToolBar(frame, wx.ID_ANY)
            pvsToolbar.SetToolBitmapSize((16, 16))
            pvsToolbar.typecheckToolbarItem = pvsToolbar.AddSimpleTool(wx.ID_ANY, LABEL_TYPECHECK, getTypecheckImage(), short_help_string="Parse and typecheck file")
            frame.Bind(wx.EVT_TOOL, onTypecheck, pvsToolbar.typecheckToolbarItem)
            self.toolbars.append(ToolbarManager.PVSTOOLBAR)          
            return pvsToolbar
        else:
            return self.getToolbar(ToolbarManager.PVSTOOLBAR)

    def toggleToolbar(self, name):
        toolbar = self.getToolbar(name)
        toolbar = util.auiManager().GetPane(name)
        self.showToolbar(name, not toolbar.IsShown())

    def showToolbar(self, name, visible=True):
        log.debug("showToolbar was called for %s and %s", name, visible)
        frame = util.getMainFrame()
        toolbar = util.auiManager().GetPane(name)
        toolbar.Show(visible)
        frame.auiManager.Update()

    def getToolbar(self, name):
        return util.auiManager().GetPane(name)

    def update(self, parameters):
        return #TODO: implement this better
        if OPENFILES in parameters:
            value = parameters[OPENFILES] > 0
            editorToolbar = self.getToolbar(ToolbarManager.EDITTOOLBAR)
            editorToolbar.saveFileToolbarItem.SetActive(value)
            editorToolbar.saveallFileToolbarItem.SetActive(value)
            editorToolbar.cutToolbarItem.SetActive(value)
            editorToolbar.copyToolbarItem.SetActive(value)
            editorToolbar.pasteToolbarItem.SetActive(value)
        if PVSMODE in parameters:
            self.getToolbar(ToolbarManager.PVSTOOLBAR).typecheckToolbarItem.SetActive(parameters[PVSMODE] == PVS_MODE_LISP)
        
        
            

        