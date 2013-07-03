
# This class manager the Find/Replace in the Edit menu

import re
import wx
from constants import EMPTY_STRING
import util

log = util.getLogger(__name__)

class FindReplaceManager:
    """A dialog box for finding and replacing texts in a RichEditor"""
    
    def __init__(self, frame, defaultFindText=EMPTY_STRING, defaultReplaceText=EMPTY_STRING):
        self.defaultFindText = defaultFindText
        self.defaultReplaceText = defaultReplaceText
        self.data = wx.FindReplaceData()
        self.data.SetFlags(1)
        
    def show(self):
        frame = util.getMainFrame()
        self.data.SetFindString(self.defaultFindText)
        self.data.SetReplaceString(self.defaultReplaceText)
        dlg = wx.FindReplaceDialog(frame, self.data, "Find & Replace", wx.FR_REPLACEDIALOG)
        dlg.Bind(wx.EVT_FIND, self.OnFind)
        dlg.Bind(wx.EVT_FIND_NEXT, self.OnFindNext)
        dlg.Bind(wx.EVT_FIND_REPLACE, self.OnReplace)
        dlg.Bind(wx.EVT_FIND_REPLACE_ALL, self.OnReplaceAll)
        dlg.Bind(wx.EVT_FIND_CLOSE, self.OnFindReplaceBoxClose)
        self.goingDown = False
        self.wholeWord = False
        self.matchCase = False
        p1 = frame.GetPosition()
        dlg.Show(True)
        p2 = dlg.GetPosition()
        p3 = (p2[0], max(5, p1[1]-100))
        dlg.SetPosition(p3)
        
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
        
    def OnFindReplaceBoxClose(self, evt):
        log.info("FindReplaceDialog closing...")
        evt.GetDialog().Destroy()
        
    def OnFind(self, evt):
        self.findText()
                         
    def OnFindNext(self, evt):
        self.findText()

    def findText(self):
        #TODO: RichEditor should have an API for finding and replacing text.
        frame = util.getMainFrame()
        _find = self.data.GetFindString()
        log.info("Find Next %s", _find)
        page = frame.notebook.getActiveRichEditor()
        nextOne = self.findPositionOfNext(_find)
        if nextOne is not None:
            page.styledText.SetSelection(nextOne, nextOne + len(_find))
        else:
            frame.showMessage("No more occurrences of '%s' was found"%_find)
        
    def OnReplace(self, evt):
        frame = util.getMainFrame()
        _find = self.data.GetFindString()
        _replace = self.data.GetReplaceString()
        log.info("Replace Next %s", _find)
        page = frame.notebook.getActiveRichEditor()
        nextOne = self.findPositionOfNext(_find)
        if nextOne is not None:
            page.styledText.SetSelection(nextOne, nextOne + len(_find))
            page.styledText.ReplaceSelection(_replace)
        else:
            frame.showMessage("No more occurrences of '%s' was found"%_find)
                    
    def OnReplaceAll(self, evt):
        frame = util.getMainFrame()
        _find = self.data.GetFindString()
        _replace = self.data.GetReplaceString()
        log.info("Replace All %s", _find)
        page = frame.notebook.getActiveRichEditor()
        nextOne = self.findPositionOfNext(_find)
        while nextOne is not None:
            page.styledText.SetSelection(nextOne, nextOne + len(_find))
            page.styledText.ReplaceSelection(_replace)
            nextOne = self.findPositionOfNext(_find)
            
    def findPositionOfNext(self, _findText):
        frame = util.getMainFrame()
        self.readFlags()
        log.info("Going Down: %s, Whole Word: %s, Match Case: %s", self.goingDown, self.wholeWord, self.matchCase)
        flags = re.UNICODE if self.matchCase else re.IGNORECASE | re.UNICODE
        page = frame.notebook.getActiveRichEditor()
        selection = page.styledText.GetSelection()
        log.info("Selection Position: %s", selection)
        cursor = page.styledText.GetCurrentPos()
        log.info("Cursor at: %s", cursor)
        position = selection[1] if selection[0] < selection[1] else cursor
        log.info("Position: %s", position)
        text = page.styledText.GetText()
        if self.wholeWord:
            searchFor = r"\b%s\b"%_findText
        else:
            searchFor = _findText
        pattern = re.compile(searchFor, flags)
        if self.goingDown:
            result = pattern.search(text, position)
        else:
            result = pattern.search(text, 0, position-1)
        
        if result is None:
            return None
        if self.goingDown:
            nextOne = result.start()
        else:
            nextOne = result.start()
            while True:
                result = pattern.search(text, result.end(), position-1)
                if result is None:
                    break
                else:
                    nextOne = result.start()
        log.info("nextOne: %s", position)
        if nextOne < 0:
            nextOne = None
        return nextOne

    
