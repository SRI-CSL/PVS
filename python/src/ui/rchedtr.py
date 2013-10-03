

import wx
import codecs
import logging
import constants
from styltxt import PVSStyledText
from wx.lib.pubsub import setupkwargs, pub 

class RichEditor(wx.Panel):
    """RichEditor displays the content a file or buffer. It provides syntax highlighting, 
    as well as other functionalities like Copy, Paste, etc"""
    
    ALL = "all"
    FULLNAME = "fullname"
    FOCUSED = "focused"
    NEWNAME = "newName"
    
    
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
        pub.subscribe(self.saveFile, constants.PUB_SAVEFILE)
        
    def addRedMarker(self, lineN):
        self.styledText.MarkerAdd(lineN-1, 1)
        
    def removeRedMarkers(self):
        self.styledText.MarkerDeleteAll(1)
        
    def onCursorPositionChanged(self):
        line = self.styledText.GetCurrentLine() + 1
        column = self.styledText.GetColumn(self.styledText.GetCurrentPos()) + 1
        position = "Line: %d, Column %d"%(line, column)
        #logging.debug(position)
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
        logging.info("Setting content of the rich editor")
        if wx.USE_UNICODE:
            unitext = self.decode(text)[0]
            self.styledText.SetText(unitext)
        else:
            self.styledText.SetText(text)

    def getText(self):
        return self.styledText.GetText()

    def getFilename(self):
        return self.fullname
    
    def saveFile(self, newName=None):
        self.removeRedMarkers()
        if newName is not None:
            oldName = self.fullname
            self.fullname = newName
        else:
            oldName = None
        logging.info("Saving file %s", self.fullname)
        self.styledText.SaveFile(self.fullname)
        pub.sendMessage(constants.PUB_FILESAVED, fullname=self.fullname, oldname=oldName)
        
    
