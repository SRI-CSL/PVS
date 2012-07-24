
import wx
from pvsricheditor import PVSRichEditor


class PVSNotebookManager(wx.Notebook):
    def __init__(self, parent, ID, style=0):
        wx.Notebook.__init__(self, parent, ID)
        self.pages = {}
        
    def addFile(self, pvsFile):
        editor = PVSRichEditor(self, wx.ID_ANY)
        self.AddPage(editor, pvsFile.filename)

        editor.setText(pvsFile.getContent())
        self.pages[pvsFile.filename] = pvsFile
        
        
        