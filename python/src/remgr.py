
# This class controls and manages all the tabs that contain open files and buffers for editing

import wx
import ui.rchedtr
from constants import *
import wx.lib.agw.aui as aui
import os.path
import util
import logging
from ui.images import getPVSLogo
from wx.lib.pubsub import setupkwargs, pub
from preference import Preferences

class RichEditorManager:
    """NotebookManager manages the open tabs in the main frame. Each tab corresponds to a file or a buffer"""
    __shared_state = {}

    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "editors" in self.__dict__:
            self.editors = {}
            pub.subscribe(self.addFile, PUB_ADDFILE)
            pub.subscribe(self.removeFile, PUB_CLOSEFILE)
            pub.subscribe(self.onFileSaved, PUB_FILESAVED)
            pub.subscribe(self.onErrorLocation, PUB_ERRORLOCATION)
            pub.subscribe(self.clearAnnotations, PUB_REMOVEANNOTATIONS)
            pub.subscribe(self.applyNamesInformation, PUB_NAMESINFOUPDATE)
            
            #self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGING, self.OnPageChanging)
        
    def __setitem__(self, name, re):
        self.editors[name] = re
        
    def __getitem__(self, name):
        return self.editors[name]
    
    def __delitem__(self, name):
        del self.editors[name]
        
    def setNotebook(self, nb):
        self.notebook = nb
        self.auiManager = self.notebook.GetAuiManager()
        self.notebook.Bind(aui.EVT_AUINOTEBOOK_END_DRAG, self.onEndDrag)
        self.notebook.Bind(aui.EVT_AUINOTEBOOK_DRAG_MOTION, self.onDragMotion)
        self.notebook.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGED, self.OnPageChanged)
        self.notebook.Bind(aui.EVT_AUINOTEBOOK_PAGE_CLOSE, self.OnPageClose)
        
        
    def saveAllFiles(self, files=None):
        richEditors = self.getOpenRichEditors() if files is None else [self.editors[fullname] for fullname in files]
        for richEditor in richEditors:
            richEditor.saveFile()
            
    def getOpenRichEditors(self):
        return self.editors.values()
        
    def getOpenFileNames(self):
        return self.editors.keys()
    
    def removeAllFiles(self):
        for fullname in self.editors.keys():
            self.removeFile(fullname)
            
    def removeFile(self, fullname):
        richEditor = self[fullname]
        logging.info("Closing RichEditor for %s", fullname)
        del self[fullname]
        frame = self._getWidgetFrame(richEditor)
        if "richEditor" in frame.__dict__:
            frame.attachRichEditorBackToNotebook = False # Close frame and the editor both
            frame.Close()
            return
        elif frame == util.getMainFrame():
            for i in range(self.notebook.GetPageCount()):
                page = self.notebook.GetPage(i)
                if fullname == page.getFilename():
                    self.notebook.DeletePage(i)
                    return
            logging.warning("Did not find the file %s", fullname) 
        else:
            logging.error("The code should not reach here for %s", fullname)
            
    def OnPageClose(self, event):
        richEditor = self._getSelectedPage()
        self.handleCloseFileRequest(richEditor.getFilename())
        event.Veto()
        
    def ensureFilesAreSavedToPoceed(self, files=None):
        """Ensure that all files are saved before closing them all"""
        filesAreSaved = True
        richEditors = self.getOpenRichEditors() if files is None else [self.editors[fullname] for fullname in files]
        for richEditor in richEditors:
            if richEditor.styledText.GetModify():
                filesAreSaved = False
                break    
        safeToProceed = True
        numberOfFiles = len(richEditors)
        if not filesAreSaved and numberOfFiles>0:
            if numberOfFiles == 1:
                message = "%s has been modified. Save changes?"%richEditors[0].getFilename()
            else:
                message = "Some files have been modified. Save changes?"
            choice = util.getMainFrame().askYesNoCancelQuestion(message)
            if choice == wx.ID_YES:
                self.saveAllFiles()
            elif choice == wx.ID_CANCEL:
                safeToProceed = False
        return safeToProceed        
    
    def handleCloseFileRequest(self, fullname):
        """called to close an open file"""
        if self.ensureFilesAreSavedToPoceed((fullname,)):
            Preferences().setRecentFile(fullname)
            pub.sendMessage(PUB_CLOSEFILE, fullname=fullname)
            pub.sendMessage(PUB_NUMBEROFOPENFILESCHANGED)
    
    def addFile(self, fullname):
        if not fullname in self.editors:
            logging.info("Opening a new editor tab for %s", fullname) 
            editor = ui.rchedtr.RichEditor(self.notebook, wx.ID_ANY, fullname)
            self.notebook.AddPage(editor, util.getFilenameFromFullPath(fullname), True, self.getProperBitmap())
            if os.path.exists(fullname):
                editor.styledText.LoadFile(fullname)
                editor.styledText.SetSelection(0, 0)
                self[fullname] = editor
        self.showRichEditorForFile(fullname)
        
    def getNumberOfOpenFiles(self):
        return len(self.editors)
    
    def getProperBitmap(self, type="FILE"):
        #TODO: Later when we show buffers too we want to return different bitmaps for files and for buffers
        return getPVSLogo()
    
    def _getWidgetFrame(self, w):
        while not isinstance(w, wx.Frame):
            w = w.GetParent()
        return w
    
    def getFocusedRichEditor(self):
        focus = wx.Window.FindFocus()
        focus = self._getWidgetFrame(focus)
        if "richEditor" in focus.__dict__:
            return focus.richEditor # A Floating Frame
        elif focus == util.getMainFrame():
            return self._getSelectedPage()
        return None
    
    def onErrorLocation(self, begin, end):
        richEditor = self.getFocusedRichEditor()
        richEditor.addRedMarker(begin[0])
        
    def clearAnnotations(self):
        richEditor = self.getFocusedRichEditor()
        richEditor.removeRedMarkers()
        richEditor.applyNamesInformation([])
    
    #TODO: Check the following functions and see if they are redundant or something.
    
    def OnPageChanged(self, event):
        logging.debug("Active Tab Index: %d", event.GetSelection())
        event.Skip()
        
    def OnPageChanging(self, event):
        event.Skip()

    def showRichEditorForFile(self, fullname):
        logging.info("Showing richEditor for %s", fullname)
        richEditor = self[fullname]
        frame = self._getWidgetFrame(richEditor)
        if frame == util.getMainFrame():
            self.notebook.SetSelection(self._getPageIndex(richEditor))
        else:
            frame.Raise()
            
    def applyNamesInformation(self, fullname, information):
        if fullname in self.editors:
            richEditor = self[fullname]
            richEditor.applyNamesInformation(information)
        else:
            logging.warn("information received for %s, but no richEditor is opened for it", fullname)
    
    def _getPageIndex(self, richEditor):
        for i in range(self.notebook.GetPageCount()):
            if richEditor == self.notebook.GetPage(i):
                return i
        return None
    
    def _getSelectedPage(self):
        """Return the selected page in the notebook"""
        ap = self.notebook.GetSelection()
        if ap > -1:
            richEditor = self.notebook.GetPage(ap)
            logging.info("Active page is %d", ap)
            return richEditor
        logging.warning("No file is open")
        return None
    
    def onFileSaved(self, fullname, oldname=None):
        """called when a file is saved"""
        if oldname is not None:
            richEditor = self[oldname]
            del self[oldname]
            self[fullname] = richEditor
            newName = util.getFilenameFromFullPath(fullname)
            self.notebook.SetPageText(self._getPageIndex(richEditor), newName)    
    
    # The following methods are to make the page to be float-able:
    
    def onDragMotion(self, event):
        self.auiManager.HideHint()
        if self.notebook.IsMouseWellOutsideWindow():
            x, y = wx.GetMousePosition()
            hintRect = wx.Rect(x, y, 400, 300)
            # Use CallAfter so we overwrite the hint that might be 
            # shown by our superclass:
            wx.CallAfter(self.auiManager.ShowHint, hintRect)
        event.Skip()

    def onEndDrag(self, event):
        self.auiManager.HideHint()
        if self.notebook.IsMouseWellOutsideWindow():
            # Use CallAfter so we our superclass can deal with the event first
            wx.CallAfter(self.FloatPage, self.notebook.GetSelection())
        event.Skip()

    def IsMouseWellOutsideWindow(self):
        screenRect = self.notebook.GetScreenRect()
        screenRect.Inflate(50, 50)
        return not screenRect.Contains(wx.GetMousePosition())
    
    def FloatPage(self, pageIndex):
        pageTitle = self.notebook.GetPageText(pageIndex)
        logging.debug("Float Page: %s", pageTitle)
        frame = wx.Frame(self.notebook, title=pageTitle, style=wx.DEFAULT_FRAME_STYLE|wx.FRAME_TOOL_WINDOW)
        frame.attachRichEditorBackToNotebook = True
        frame.richEditor = self.notebook.GetPage(pageIndex)
        frame.richEditor.Reparent(frame)
        self.notebook.RemovePage(pageIndex)
        frame.Bind(wx.EVT_CLOSE, self.closeFloatingRichEditor)
        #frame.Bind(EVT_CUSTOM_CLOSE_FRAME, self.onCloseRichEditor)
        frame.Move(wx.GetMousePosition())
        frame.Show()
        
    def onCloseRichEditor(self, event):
        event.Skip()

    def closeFloatingRichEditor(self, event):
        frame = event.GetEventObject()
        if frame.attachRichEditorBackToNotebook:
            pageTitle = frame.GetTitle()
            logging.debug("Attaching %s back to the main frame", pageTitle)
            richEditor = frame.GetChildren()[0]
            richEditor.Reparent(self.notebook)
            self.notebook.AddPage(richEditor, pageTitle, True, self.getProperBitmap())
        event.Skip()

