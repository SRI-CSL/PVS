
import wx
from pvsricheditor import PVSRichEditor
import config

log = config.getLogger(__name__)

class PVSNotebookManager(wx.Notebook):
    PVSFILE = "pvsFile"
    
    def __init__(self, parent, ID, style=0):
        wx.Notebook.__init__(self, parent, ID)
        self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGED, self.OnPageChanged)
        #self.Bind(wx.EVT_NOTEBOOK_PAGE_CHANGING, self.OnPageChanging)
        #self.Bind(wx.EVT_AUINOTEBOOK_PAGE_CLOSED, self.close)
        self.pages = {}
        
    def addFile(self, pvsFile):
        log.info("Opening a new editor tab for %s", pvsFile) 

        editor = PVSRichEditor(self, wx.ID_ANY, {PVSNotebookManager.PVSFILE: pvsFile})
        self.AddPage(editor, pvsFile.filename)
        pvsFile.setEditor(editor)
        editor.setText(pvsFile.content)
        self.pages[pvsFile.fullname] = pvsFile
        
    def OnPageChanged(self, event):
        log.debug("Active Tab Index: %d", event.GetSelection())
        event.Skip()
        
    def OnPageChanging(self, event):
        event.Skip()

    def showTabForFile(self, fullname):
        log.info("Showing tab for %s", fullname) 
        for i in range(self.PageCount):
            page = self.GetPage(i)
            if page.data[PVSNotebookManager.PVSFILE].fullname == fullname:
                self.SetSelection(i)
                log.debug("Setting tab %d as active", i)
                return
        log.warning("Did not find the file %s", fullname) 
            
    def getActiveFilename(self):
        fn = self.getActivePage().data[PVSNotebookManager.PVSFILE].fullname
        log.info("Active file name is %s", fn)
        return fn
    
    def getActivePage(self):
        ap = self.GetSelection()
        page = self.GetPage(ap)
        log.info("Active page is %d", ap)
        return page
    
    def closeTabForFile(self, fullname):
        log.info("Closing tab for %s", fullname)
        for i in range(self.GetPageCount()):
            page = self.GetPage(i)
            if isinstance(page, PVSRichEditor):
                if page.data.has_key(PVSNotebookManager.PVSFILE):
                    pvsFile = page.data[PVSNotebookManager.PVSFILE]
                    if fullname == pvsFile.fullname:
                        log.debug("Editor tab %d is being closed", i)
                        page.Destroy()
                        self.RemovePage(i)
                        #self.SetSelection(i)
                        self.Refresh()
                        del self.pages[fullname]
                        return
        log.error("No tab was found for the file %s", fullname)



    def selectAll(self):
        page = self.getActivePage()
        page.selectAll()
        
    def copy(self):
        page = self.getActivePage()
        page.copy()     

    def paste(self):
        page = self.getActivePage()
        page.paste()     

    def cut(self):
        page = self.getActivePage()
        page.cut()

    def undo(self):
        page = self.getActivePage()
        page.undo()

    def find(self):
        page = self.getActivePage()

