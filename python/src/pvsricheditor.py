import wx
import wx.stc as stc
import codecs

class PVSRichEditor(wx.Panel):
    def __init__(self, parent, ID):
        wx.Panel.__init__(self, parent, ID)
        sizer = wx.BoxSizer()        
        self.styledText = stc.StyledTextCtrl(self, wx.ID_ANY, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        sizer.Add(self.styledText, 1, wx.EXPAND, 0)
        self.SetSizer(sizer)
        
        face1 = 'Helvetica'
        face2 = 'Times'
        face3 = 'Courier'
        pb = 12
        self.styledText.SetMarginType(0, stc.STC_MARGIN_NUMBER)
        self.styledText.SetMarginWidth(0, 22)
        self.styledText.StyleSetSpec(stc.STC_STYLE_LINENUMBER, "size:%d,face:%s" % (pb, face1))
        self.styledText.SetMarginType(1, stc.STC_MARGIN_SYMBOL)
        self.styledText.MarkerDefine(0, stc.STC_MARK_ROUNDRECT, "#CCFF00", "RED")
        self.styledText.MarkerDefine(1, stc.STC_MARK_CIRCLE, "FOREST GREEN", "SIENNA")
        self.styledText.MarkerDefine(2, stc.STC_MARK_SHORTARROW, "blue", "blue")
        self.styledText.MarkerDefine(3, stc.STC_MARK_ARROW, "#00FF00", "#00FF00")
            
        self.decode = codecs.lookup("utf-8")[1]

    def addRedMarker(self, lineN):
        self.MarkerAdd(lineN, 1)
    
    def OnDestroy(self, evt):
        # This is how the clipboard contents can be preserved after
        # the app has exited.
        wx.TheClipboard.Flush()
        evt.Skip()
        
    def setText(self, text):
        if wx.USE_UNICODE:
            unitext, l = self.decode(text)
            self.styledText.SetText(unitext)
        else:
            self.styledText.SetText(text)




        