import wx
from constants import EMPTY_STRING
from config import getLogger

log = getLogger(__name__)

class FindReplaceManager:
    def __init__(self, frame, defaultFindText=EMPTY_STRING, defaultReplaceText=EMPTY_STRING):
        self.mainFrame = frame
        self.defaultFindText = defaultFindText
        self.defaultReplaceText = defaultReplaceText
        self.data = None
        
    def show(self):
        data = wx.FindReplaceData()
        data.SetFindString(self.defaultFindText)
        data.SetReplaceString(self.defaultReplaceText)
        dlg = wx.FindReplaceDialog(self.mainFrame, data, "Find & Replace", wx.FR_REPLACEDIALOG)
        dlg.Bind(wx.EVT_FIND, self.OnFind)
        dlg.Bind(wx.EVT_FIND_NEXT, self.OnFindNext)
        dlg.Bind(wx.EVT_FIND_REPLACE, self.OnReplace)
        dlg.Bind(wx.EVT_FIND_REPLACE_ALL, self.OnReplaceAll)
        dlg.Bind(wx.EVT_FIND_CLOSE, self.OnFindClose)
        self.data = data  # save a reference to it...
        self.goingDown = False
        self.wholeWord = False
        self.matchCase = False
        dlg.Show(True)
        
    def readFlags(self):
        _flags = self.data.GetFlags()
        if _flags & 1 > 0:
            self.goingDown = True
        else:
            self.goingDown = False
        if _flags & 2 > 0:
            self.wholeWord = True
        else:
            self.wholeWord = False
        if _flags & 4 > 0:
            self.matchCase = True
        else:
            self.matchCase = False
        
    def OnFindClose(self, evt):
        log.info("FindReplaceDialog closing...")
        evt.GetDialog().Destroy()
        
    def OnFind(self, evt):
        _find = self.data.GetFindString()
        self.readFlags()
        log.info("Find %s", _find)
        log.info("Going Down: %s, Whole Word: %s, Match Case: %s", self.goingDown, self.wholeWord, self.matchCase)
        
    def OnFindNext(self, evt):
        _find = self.data.GetFindString()
        self.readFlags()
        log.info("Find Next %s", _find)
        log.info("Going Down: %s, Whole Word: %s, Match Case: %s", self.goingDown, self.wholeWord, self.matchCase)
        
    def OnReplace(self, evt):
        _find = self.data.GetFindString()
        _replace = self.data.GetReplaceString()
        self.readFlags()
        log.info("Replace %s with %s", _find, _replace)
        log.info("Going Down: %s, Whole Word: %s, Match Case: %s", self.goingDown, self.wholeWord, self.matchCase)
        
    def OnReplaceAll(self, evt):
        _find = self.data.GetFindString()
        _replace = self.data.GetReplaceString()
        self.readFlags()
        log.info("Replace All %s with %s", _find, _replace)
        log.info("Going Down: %s, Whole Word: %s, Match Case: %s", self.goingDown, self.wholeWord, self.matchCase)
        
