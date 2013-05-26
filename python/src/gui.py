from constants import *
import util
import os, os.path
import preference
import ui.frame
import wx
import wx.stc as stc

manager = None
log = util.getLogger(__name__)

class GuiManager:
    """The main API for interacting with the GUI"""
    
    def __init__(self):
        global manager
        manager = self
        self.frame = None
        self.application = None
        self.menubar = None
        self.toolbar = None
        self.statusbar = None
        self.notebook = None
        self.console = None
        self.filesTreeManager = None
        self.buffersTree = None
        self.filesBuffersManager = None
        self.proofTreeManager = None
        self.files = {}
        self.buffers = {}

    # Setters:    
    def setMainFrame(self, frame):
        self.frame = frame
        
    def setApplication(self, application):
        self.application = application
        
    def setMenubar(self, mb):
        self.menubar = mb
        
    def setToolbar(self, tb):
        self.toolbar = tb
        
    def setStatusbar(self, sb):
        self.statusbar = sb
        
    def setNotebook(self, notebook):
        self.notebook = notebook
        
    def setConsole(self, console):
        self.console = console
        
    def setFilesTreeManager(self, ftm):
        self.filesTreeManager = ftm
        
    def setBuffersTree(self, bt): #TODO: change this to a manager class
        self.buffersTree = bt
        
    def setFilesBuffersManager(self, fbm):
        self.filesBuffersManager = fbm
        
    def setProofTreeManager(self, ptm):
        self.proofTreeManager = ptm
        
    # Dialog Boxes: 
        
    def showError(self, message):
        """Show a dialog for an error"""
        dlg = wx.MessageDialog(self.frame, message, ERROR, wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()
        
    def showWarning(self, message):
        """Show a dialog for a warning"""
        dlg = wx.MessageDialog(self.frame, message, WARNING, wx.OK | wx.ICON_WARNING)
        dlg.ShowModal()
        dlg.Destroy()
        
    def showMessage(self, message):
        """Show a dialog for a message"""
        dlg = wx.MessageDialog(self.frame, message, MESSAGE, wx.OK | wx.ICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()
        
    def askYesNoQuestion(self, question, title=EMPTY_STRING):
        """Show a dialog box to ask a question with two possible answers: Yes and No"""
        dlg = wx.MessageDialog(self.frame, question, title, wx.YES_NO | wx.ICON_QUESTION)
        choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO
        dlg.Destroy()
        return choice
    
    def askYesNoCancelQuestion(self, question, title=EMPTY_STRING):
        """Show a dialog box to ask a question with three possible answers: Yes, No, and Cancel"""
        dlg = wx.MessageDialog(self.frame, question, title, wx.YES_NO | wx.CANCEL | wx.ICON_QUESTION)
        choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO or wx.ID_CANCEL
        dlg.Destroy()
        return choice
    
    def chooseDirectory(self, message, defaultDirectory=EMPTY_STRING):
        """Show a dialog to choose a directory"""    
        dialog = wx.DirDialog (self.frame, message = message, defaultPath=defaultDirectory)
        newPath = None
        if dialog.ShowModal() == wx.ID_OK:
            newPath = dialog.GetPath()
        dialog.Destroy()
        return newPath
        
    # API:
    
    def createMainFrame(self):
        self.frame = ui.frame.MainFrame(None, wx.ID_ANY, "")
        return self.frame
    
    def configMenuToolbar(self, openFiles):
        """Enable/Disable menu options based on the situation"""
        mb = self.menubar
        tb = self.toolbar
        value = (openFiles > 0)
        mb.enableCloseFile(value)
        mb.enableUndo(value)
        mb.enableCut(value)
        mb.enableCopy(value)
        mb.enablePaste(value)
        mb.enableSelectAll(value)
        mb.enableFind(value)
        tb.enableSave(value)
        tb.enableSaveAll(value)
        tb.enableCut(value)
        tb.enableCopy(value)
        tb.enablePaste(value)
        #tb.enableStartPVS(False)

    def enableToolbarButton(self, ID, value = True):
        self.toolbar.Enable(ID, value)
        
    def enableMenuButton(self, ID, value = True):
        self.menubar.Enable(ID, value)

    def setStatusbarText(self, text, location=0):
        log.info("Setting status bar[%d] to: %s"%(location, text))
        self.statusbar.SetStatusText(text, location)
        
    def updateFrame(self, status = PVS_MODE_UNKNOWN):
        log.info("Updating frame with PVS status as %s", status)
        self.setStatusbarText(PVS_MODE + status)
        if status == PVS_MODE_OFF:
            log.debug("Disabling pvsin")
            self.console.clearIn()
            self.console.pvsin.SetEditable(False)
            self.toolbar.enableStopPVS(False)
            self.toolbar.enableStartPVS(True)
        else:
            log.debug("Enabling pvsin")
            self.console.pvsin.SetEditable(True)
            self.toolbar.enableStopPVS(True)
            self.toolbar.enableStartPVS(False)

    def loadContext(self):
        """Load .pvseditor and open all the files that were open last time"""
        preference.manager.loadContextPreferences()
        openFiles = preference.manager.listOfOpenFiles()
        fullnames = []
        context = preference.manager.getContext()
        for fn in openFiles:
            fullnames.append(os.path.join(context, fn))
        self.openFiles(fullnames)
        self.configMenuToolbar(openFiles)
        
    def closeContext(self):
        """Save .pvseditor and close all the open files"""
        preference.manager.saveContextPreferences()
        self.closeAll()
        
    def closeMainFrame(self):
        """Send a signal to the main frame to close"""
        self.frame.Close()

    def showFilesBuffersTreeFrame(self, visible):
        if visible:
            self.filesBuffersManager.Hide()
        else:
            self.filesBuffersManager.Show()        
        
    def showProofTreeFrame(self, visible):
        if visible:
            self.proofTreeManager.Hide()
        else:
            self.proofTreeManager.Show()        

    def addBuffer(self, buffer):
        """add a new PVSBuffer"""
        pass
        
    def addFile(self, fullname):
        """add a new PVSFile, add an entry to the file list, open the file content in the editor, and set it as the active file"""
        log.info("Adding file %s", fullname)
        if not fullname in self.files.keys():
            self.filesTreeManager.addFile(fullname)
            richEditor = self.notebook.addFile(fullname)
            self.files[fullname] = richEditor
        else:
            log.info("File %s is already open", fullname)
        #self.mainFrame.pvseditor.addRedMarker(2)
        self.setFileAsActive(fullname)
        
    def openFiles(self, fullnames):
        for fullname in fullnames:
            if os.path.exists(fullname):
                self.addFile(fullname)
            else:
                log.warning("File %s no longer exists", fullname)
        
    def closeAll(self):
        """Close all tabs in the notebook, remove all files and buffers from """
        self.filesTreeManager.clear()
        self.buffersTree.DeleteAllItems() # TODO: When a BufferTreeManager is created, replace this with a line similar to above line
        self.notebook.DeleteAllPages()
        self.files = {}
        self.buffers = {}
        
    def setFileAsActive(self, fullname):
        """make the tab visible for a given file"""
        log.info("Setting file %s as active", fullname)
        self.notebook.showTabForFile(fullname)
        
    def createNewFile(self):
        """create a new file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (self.frame, "Create a new PVS file", wildcard = filters, style = wx.SAVE | wx.OVERWRITE_PROMPT )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            if not fullname.endswith(PVS_EXTENSION):
                fullname = fullname + PVS_EXTENSION
            log.info("Creating new file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        self.configMenuToolbar(len(self.files))

    def openFile(self):
        """open an existing PVS file"""
        filters = "PVS files (*" + PVS_EXTENSION + ")|*" + PVS_EXTENSION
        dialog = wx.FileDialog (self.frame, "Open PVS file", wildcard = filters, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            fullname = dialog.GetPath()
            log.info("Opening file %s", fullname)
            self.addFile(fullname)
        else:
            log.info("Nothing was selected.")
        dialog.Destroy()
        self.configMenuToolbar(len(self.files))

    def closeFile(self, fullname=None):
        """close an open file"""
        if fullname == None:
            fullname = self.notebook.getActiveFilename()
        log.info("Closing file %s", fullname)
        if fullname in self.files.keys():
            richEditor = self.files[fullname]
            canClose = True
            if richEditor.styledText.GetModify():
                choice = self.askYesNoCancelQuestion("This file has been modified. Save changes?")
                if choice == wx.ID_YES:
                    self.saveFile(fullname)
                elif choice == wx.ID_CANCEL:
                    canClose = False
            if canClose:
                del self.files[fullname]
                self.notebook.closeTabForFile(fullname)
                self.filesTreeManager.removeFile(fullname)
        else:
            log.warning("The file %s is not in %s", fullname, self.files.keys())
        self.configMenuToolbar(len(self.files))
            
    def saveFile(self, fullname=None):
        """save the active file (an active file is one whose tab is visible)"""
        if fullname == None:
            fullname = self.notebook.getActiveFilename()
        else:
            if not fullname in self.files.keys():
                log.error("%s is not even open to be saved.")
                return 
        log.info("Saving file %s", fullname)
        if self.files[fullname].styledText.GetModify():
            self.files[fullname].styledText.SaveFile(fullname)
            self.filesTreeManager.removeTheoriesFromFileTree(fullname)
            
    def saveAllFiles(self):
        """save all the open files"""
        log.info("Saving all files")
        for fullname, richEditor in self.files.items():
            if richEditor.styledText.GetModify():
                richEditor.styledText.SaveFile(fullname)
                self.filesTreeManager.removeTheoriesFromFileTree(fullname)

        
    def ensureFilesAreSavedToPoceed(self):
        """Ensure that all files are saved before closing them all"""
        richEditors = self.files.values()
        filesAreSaved = True
        for richEditor in richEditors:
            if richEditor.styledText.GetModify():
                filesAreSaved = False
                break
    
        safeToProceed = True
        if not filesAreSaved:
            choice = self.askYesNoCancelQuestion("Some files have been modified. Save changes?")
            if choice == wx.ID_YES:
                self.saveAllFiles()
            elif choice == wx.ID_CANCEL:
                safeToProceed = False
        return safeToProceed
        
    def addTheoriesToFileTree(self, fullname, result):
        """addTheoriesToFileTree is called after typechecking a file and asking for the declarations in that file"""
        fileNode = self.filesTreeManager.getFileNode(fullname)
        #root = self.tree.GetRootItem()
        if result.has_key(THEORIES):
            theories = result[THEORIES]
            for theory in theories:
                log.info("Adding theory %s to %s", theory[ID_L], fullname)
                theory[KIND] = THEORY
                theoryName = theory[ID_L]
                theoryNode = self.filesTreeManager.tree.AppendItem(fileNode, theoryName, 2, -1, wx.TreeItemData(theory))
                declarations = theory[DECLARATIONS]
                for declaration in declarations:
                    kind = declaration[KIND]
                    if kind == FORMULA_DECLARATION:
                        formulaName = declaration[ID_L]
                        log.info("Adding formula %s to theory %s", formulaName, theoryName)
                        declaration[KIND] = FORMULA
                        declaration[THEORY] = theoryName
                        self.filesTreeManager.tree.AppendItem(theoryNode, formulaName, 3, -1, wx.TreeItemData(declaration))
            self.filesTreeManager.tree.ExpandAllChildren(fileNode)

    def removeTheoriesFromFileTree(self, fullname):
        """remove the theories nodes from a file node"""
        fileNode = self.filesTreeManager.getFileNode(fullname)
        self.filesTreeManager.tree.DeleteChildren(fileNode)
        
    def handleUndoRequest(self):
        """handle the Undo request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl != None and textCtrl.CanUndo():
            textCtrl.Undo()
            return True
        return False
        
    def handleRedoRequest(self):
        """handle the Redo request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl != None and textCtrl.CanRedo():
            textCtrl.Redo()
            return True
        return False
                
    def handleCopyRequest(self):
        """handle the Copy request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl != None and textCtrl.CanCopy():
            textCtrl.Copy()
            return True
        return False
    
    def handleCutRequest(self):
        """handle the Cut request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl != None and textCtrl.CanCut():
            textCtrl.Cut()
            return True
        return False
    
    def handlePasteRequest(self):
        """handle the Paste request and return true if succeeded."""
        textCtrl = self._findFocusedTextCtrl()
        if textCtrl != None and textCtrl.CanPaste():
            textCtrl.Paste()
            return True
        return False
    
    def handleSelectAllRequest(self):
        top = self.application.GetTopWindow()
        if top != None:
            focus = top.FindFocus()
            if focus != None and isinstance(focus, stc.StyledTextCtrl):
                focus.SelectAll()
                return True
        return False
        
    # Private methods:
    
    def _findFocusedTextCtrl(self):
        top = self.application.GetTopWindow()
        if top != None:
            focus = top.FindFocus()
            if focus != None and (isinstance(focus, wx.TextCtrl) or isinstance(focus, stc.StyledTextCtrl)):
                return focus
        return None
