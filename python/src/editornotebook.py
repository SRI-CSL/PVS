
import wx
from pvsricheditor import PVSRichEditor
from config import getLogger

log = getLogger(__name__)

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

        editor.setText(pvsFile.getContent())
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
        ap = self.GetSelection()
        page = self.GetPage(ap)
        fn = page.data[PVSNotebookManager.PVSFILE].fullname
        #fn = self.pages.keys()[ap]
        log.info("Active page is %d and the file name is %s", ap, fn)
        return fn
    
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




