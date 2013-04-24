
# This class controls and manages all the tabs that contain open files and buffers for editing

import wx
from ui.rchedtr import RichEditor
import util

log = util.getLogger(__name__)

class NotebookManager(wx.Notebook):
    """NotebookManager manages the open tabs in the main frame. Each tab corresponds to a file or a buffer"""
    
    PVSFILE = "pvsFile"
    
    def __init__(self, parent, ID, style=0):
        wx.Notebook.__init__(self, parent, ID)
        self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGED, self.OnPageChanged)
        #self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGING, self.OnPageChanging)
        #self.Bind(wx.EVT_AUINOTEBOOK_PAGE_CLOSED, self.close)
        
    def addFile(self, fullname):
        log.info("Opening a new editor tab for %s", fullname) 
        editor = RichEditor(self, wx.ID_ANY, {NotebookManager.PVSFILE: fullname})
        self.AddPage(editor, util.getFilenameFromFullPath(fullname))
        editor.styledText.LoadFile(fullname)
        return editor
        
    def OnPageChanged(self, event):
        log.debug("Active Tab Index: %d", event.GetSelection())
        event.Skip()
        
    def OnPageChanging(self, event):
        event.Skip()

    def showTabForFile(self, fullname):
        log.info("Showing tab for %s", fullname) 
        for i in range(self.PageCount):
            richEditor = self.GetPage(i)
            if richEditor.data[NotebookManager.PVSFILE] == fullname:
                self.SetSelection(i)
                log.debug("Setting tab %d as active", i)
                return
        log.warning("Did not find the file %s", fullname) 
            
    def getActiveFilename(self):
        richEditor = self.getActivePage()
        if richEditor != None:
            fullname = richEditor.data[NotebookManager.PVSFILE]
            log.info("Active file name is %s", fullname)
            return fullname
        return None
    
    def getActivePage(self):
        ap = self.GetSelection()
        if ap > -1:
            richEditor = self.GetPage(ap)
            log.info("Active page is %d", ap)
            return richEditor
        log.warning("No file is open")
        return None
    
    def closeTabForFile(self, fullname):
        log.info("Closing tab for %s", fullname)
        for i in range(self.GetPageCount()):
            richEditor = self.GetPage(i)
            if isinstance(richEditor, RichEditor):
                if richEditor.data.has_key(NotebookManager.PVSFILE):
                    richEditorFilename = richEditor.data[NotebookManager.PVSFILE]
                    if fullname == richEditorFilename:
                        log.debug("Editor tab %d is being closed", i)
                        richEditor.Destroy()
                        self.RemovePage(i)
                        #self.SetSelection(i)
                        self.Refresh()
                        return
        log.error("No tab was found for the file %s", fullname)

    def selectAll(self):
        richEditor = self.getActivePage()
        richEditor.selectAll()

    def undo(self):
        richEditor = self.getActivePage()
        richEditor.undo()

    def find(self):
        #TODO: What is this?
        richEditor = self.getActivePage()

