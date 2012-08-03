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
        
        dlg.data = data  # save a reference to it...
        dlg.Show(True)
        
    def OnFindClose(self, evt):
        log.info("FindReplaceDialog closing...")
        evt.GetDialog().Destroy()
        
    def OnFind(self, evt):
        _find = set.data.GetFindString()
        log.info("Find %s", _find)
        
    def OnFindNext(self, evt):
        _find = set.data.GetFindString()
        log.info("Find Next %s", _find)
        
    def OnReplace(self, evt):
        _find = set.data.GetFindString()
        _replace = set.data.GetReplaceString()
        log.info("Replace %s with %s", _find, _replace)
        
    def OnReplaceAll(self, evt):
        _find = set.data.GetFindString()
        _replace = set.data.GetReplaceString()
        log.info("Replace All %s with %s", _find, _replace)
        
