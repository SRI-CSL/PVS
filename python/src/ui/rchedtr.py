

import wx
import codecs
import util
from styltxt import PVSStyledText

log = util.getLogger(__name__)

class RichEditor(wx.Panel):
    """RichEditor displays the content a file or buffer. It provides syntax highlighting, 
    as well as other functionalities like Copy, Paste, etc"""
    
    def __init__(self, parent, ID, fullname):
        wx.Panel.__init__(self, parent, ID)
        sizer = wx.BoxSizer(orient=wx.VERTICAL)        
        self.styledText = PVSStyledText(self)
        sizer.Add(self.styledText, 1, wx.EXPAND, 0)
        self.statusbar = wx.StatusBar(self)
        sizer.Add(self.statusbar, 0, wx.EXPAND, 0)        
        self.SetSizer(sizer) 
        self.fullname = fullname
        self.decode = codecs.lookup("utf-8")[1]
        self.styledText.Bind(wx.EVT_KEY_UP,self.onTextKeyEvent)
        self.styledText.Bind(wx.EVT_LEFT_UP,self.onMouseEvent)
        
    def addRedMarker(self, lineN):
        self.MarkerAdd(lineN, 1)
        
    def onCursorPositionChanged(self):
        line = self.styledText.GetCurrentLine() + 1
        column = self.styledText.GetColumn(self.styledText.GetCurrentPos()) + 1
        position = "Line: %d, Column %d"%(line, column)
        #log.debug(position)
        self.statusbar.SetStatusText(position, 0)
    
    def onTextKeyEvent(self, event):
        self.onCursorPositionChanged()
        event.Skip()    
    
    def onMouseEvent(self, event):
        self.onCursorPositionChanged()
        event.Skip()    
    
    def OnDestroy(self, evt):
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

    def getFilename(self):
        return self.fullname
    
    def saveFile(self):
        log.info("Saving file %s", self.fullname)
        self.styledText.SaveFile(self.fullname)
        
    
