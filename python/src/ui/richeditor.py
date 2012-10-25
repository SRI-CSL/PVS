
# This class corresponds to the editor envoronment that displays the content of a file or a buffer
# This class is also responsible for syntax highlighting

import wx
import codecs
import common
from pvsstyledtext import PVSStyledText


log = common.getLogger(__name__)

class RichEditor(wx.Panel):
    """RichEditor corresponds to an open file or buffer. It provides syntax highlighting, 
    as well as other functionalities like Copy, Paste, etc"""
    
    def __init__(self, parent, ID, data):
        wx.Panel.__init__(self, parent, ID)
        sizer = wx.BoxSizer()        
        self.styledText = PVSStyledText(self)
        sizer.Add(self.styledText, 1, wx.EXPAND, 0)
        self.SetSizer(sizer)
        
        self.data = data
        self.decode = codecs.lookup("utf-8")[1]
        

    def addRedMarker(self, lineN):
        self.MarkerAdd(lineN, 1)
    
    def OnDestroy(self, evt):
        # This is how the clipboard contents can be preserved after
        # the app has exited.
        wx.TheClipboard.Flush()
        evt.Skip()
        
    def setText(self, text):
        log.info("Setting content of the rich editor")
        if wx.USE_UNICODE:
            unitext = self.decode(text)[0]
            self.styledText.SetText(unitext)
        else:
            self.styledText.SetText(text)

    def getText(self):
        return self.styledText.GetText()

    def selectAll(self):
        self.styledText.SelectAll()  

    def undo(self):
        self.styledText.Undo()   
        
        
        
    
