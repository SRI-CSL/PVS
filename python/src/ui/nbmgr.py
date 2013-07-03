
# This class controls and manages all the tabs that contain open files and buffers for editing

import wx
from ui.rchedtr import RichEditor
from wx.lib.pubsub import pub
from constants import *
import wx.lib.agw.aui as aui
import os.path
import util
from images import getPVSLogo

log = util.getLogger(__name__)

class NotebookManager(aui.AuiNotebook):
    """NotebookManager manages the open tabs in the main frame. Each tab corresponds to a file or a buffer"""

    def __init__(self, parent, style=0):
        aui.AuiNotebook.__init__(self, parent)
        self.SetArtProvider( aui.ChromeTabArt() )
        self.auiManager = self.GetAuiManager()
        self.Bind(aui.EVT_AUINOTEBOOK_END_DRAG, self.onEndDrag)
        self.Bind(aui.EVT_AUINOTEBOOK_DRAG_MOTION, self.onDragMotion)
        self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGED, self.OnPageChanged)
        pub.subscribe(self.addFile, PUB_ADDFILE)
        pub.subscribe(self.showRichEditorForFile, PUB_SHOWTABFORFILE) # TODO: This is not seem to be used
        pub.subscribe(self.DeleteAllPages, PUB_CLOSEALLFILES)
        pub.subscribe(self.closeRichEditorForFile, PUB_CLOSEFILE)
        pub.subscribe(self.saveFile, PUB_SAVEFILE)
        pub.subscribe(self.saveAllFiles, PUB_SAVEALLFILES)
        
        #self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(aui.EVT_AUINOTEBOOK_PAGE_CLOSE, self.OnPageClose)
        
    def DeleteAllPages(self):
        for i in range(self.GetPageCount()):
            self.DeletePage(self.GetPage(i))
    
    def OnPageClose(self, event):
        selection = event.GetSelection()
        richEditor = self.GetPage(selection)
        frame = util.getMainFrame()
        frame.handleCloseFileRequest(richEditor=richEditor)
        event.Veto()
    
            
    def saveFile(self, fullname=None):
        richEditor = self.getActiveRichEditor() if fullname is None else self.getRichEditorForFile(fullname)
        richEditor.saveFile()
        pub.sendMessage(PUB_REMOVETHEORIESFROMFILETREE, fullname=richEditor.getFilename())
    
    def saveAllFiles(self):
        for i in range(self.GetPageCount()):
            richEditor = self.GetPage(i)
            richEditor.saveFile()
            pub.sendMessage(PUB_REMOVETHEORIESFROMFILETREE, fullname=richEditor.getFilename())
    
    def addFile(self, fullname):
        if self.getRichEditorForFile(fullname) is None:
            log.info("Opening a new editor tab for %s", fullname) 
            editor = RichEditor(self, wx.ID_ANY, fullname)
            self.AddPage(editor, util.getFilenameFromFullPath(fullname), True, self.getProperBitmap())
            if os.path.exists(fullname):
                editor.styledText.LoadFile(fullname)
                editor.styledText.SetSelection(0, 0)
        self.showRichEditorForFile(fullname)
        
    def OnPageChanged(self, event):
        log.debug("Active Tab Index: %d", event.GetSelection())
        event.Skip()
        
    def OnPageChanging(self, event):
        event.Skip()

    def showRichEditorForFile(self, fullname):
        log.info("Showing tab for %s", fullname) 
        for i in range(self.GetPageCount()):
            richEditor = self.GetPage(i)
            if richEditor.getFilename() == fullname:
                self.SetSelection(i)
                log.debug("Setting tab %d as active", i)
                return
        log.warning("Did not find the file %s", fullname) 
            
    def closeRichEditorForFile(self, fullname):
        log.info("Closing tab for %s", fullname) 
        for i in range(self.GetPageCount()):
            richEditor = self.GetPage(i)
            if richEditor.getFilename() == fullname:
                self.DeletePage(i)
                return
        log.warning("Did not find the file %s", fullname) 
            
    def getActiveFilename(self):
        richEditor = self.getActiveRichEditor()
        if richEditor is not None:
            fullname = richEditor.getFilename()
            log.info("Active file name is %s", fullname)
            return fullname
        return None
    
    def getActiveRichEditor(self):
        ap = self.GetSelection()
        if ap > -1:
            richEditor = self.GetPage(ap)
            log.info("Active page is %d", ap)
            return richEditor
        log.warning("No file is open")
        return None
    
    def getRichEditorForFile(self, fullname):
        for i in range(self.GetPageCount()):
            richEditor = self.GetPage(i)
            if richEditor.getFilename() == fullname:
                return richEditor
        log.info("No RichEditor was found for %s", fullname)
        return None
    
    def getNumberOfOpenFiles(self):
        return self.GetPageCount()
    
    def getProperBitmap(self, type="FILE"):
        #TODO: Later when we show buffers too we want to return different bitmaps for files and for buffers
        return getPVSLogo()
    
    # The following methods are to make the page to be float-able:
    def onDragMotion(self, event):
        log.debug("onDragMotion was called")
        self.auiManager.HideHint()
        if self.IsMouseWellOutsideWindow():
            x, y = wx.GetMousePosition()
            hintRect = wx.Rect(x, y, 400, 300)
            # Use CallAfter so we overwrite the hint that might be 
            # shown by our superclass:
            wx.CallAfter(self.auiManager.ShowHint, hintRect)
        event.Skip()

    def onEndDrag(self, event):
        log.debug("onEndDrag was called")
        self.auiManager.HideHint()
        if self.IsMouseWellOutsideWindow():
            # Use CallAfter so we our superclass can deal with the event first
            wx.CallAfter(self.FloatPage, self.GetSelection())
        event.Skip()

    def IsMouseWellOutsideWindow(self):
        log.debug("IsMouseWellOutsideWindow was called")
        screenRect = self.GetScreenRect()
        screenRect.Inflate(50, 50)
        return not screenRect.Contains(wx.GetMousePosition())

    def FloatPage(self, pageIndex):
        log.debug("FloatPage was called")
        pageTitle = self.GetPageText(pageIndex)
        frame = wx.Frame(self, title=pageTitle, style=wx.DEFAULT_FRAME_STYLE|wx.FRAME_TOOL_WINDOW)
        richEditor = self.GetPage(pageIndex)
        richEditor.Reparent(frame)
        self.RemovePage(pageIndex)
        frame.Bind(wx.EVT_CLOSE, self.onCloseFloatingPage)
        frame.Move(wx.GetMousePosition())
        frame.Show()

    def onCloseFloatingPage(self, event):
        log.debug("onCloseFloatingPage was called")
        event.Skip()
        frame = event.GetEventObject()
        pageTitle = frame.GetTitle()
        richEditor = frame.GetChildren()[0]
        richEditor.Reparent(self)
        self.AddPage(richEditor, pageTitle, True, self.getProperBitmap())

