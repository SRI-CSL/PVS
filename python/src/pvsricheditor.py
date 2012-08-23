
# This class corresponds to the editor envoronment that displays the content of a file or a buffer
# This class is also responsible for syntax highlighting

import wx
import wx.stc as stc
import codecs
import config

log = config.getLogger(__name__)

faces = { 'times': 'Times',
          'mono' : 'Courier',
          'helv' : 'Helvetica',
          'other': 'new century schoolbook',
          'size' : 12,
          'size2': 10,
         }

class PVSRichEditor(wx.Panel):
    def __init__(self, parent, ID, data):
        wx.Panel.__init__(self, parent, ID)
        sizer = wx.BoxSizer()        
        self.styledText = stc.StyledTextCtrl(self, wx.ID_ANY, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        sizer.Add(self.styledText, 1, wx.EXPAND, 0)
        self.SetSizer(sizer)
        
        self.styledText.SetMarginType(0, stc.STC_MARGIN_NUMBER)
        self.styledText.SetMarginWidth(0, 22)
        self.styledText.StyleSetSpec(stc.STC_STYLE_LINENUMBER, "size:%d,face:%s" % (faces['size'], faces['mono']))
        self.styledText.SetMarginType(1, stc.STC_MARGIN_SYMBOL)
        self.styledText.MarkerDefine(0, stc.STC_MARK_ROUNDRECT, "#CCFF00", "RED")
        self.styledText.MarkerDefine(1, stc.STC_MARK_CIRCLE, "FOREST GREEN", "SIENNA")
        self.styledText.MarkerDefine(2, stc.STC_MARK_SHORTARROW, "blue", "blue")
        self.styledText.MarkerDefine(3, stc.STC_MARK_ARROW, "#00FF00", "#00FF00")
        self.data = data
        self.setSyntaxHighlighting()
        self.decode = codecs.lookup("utf-8")[1]
        
    def setSyntaxHighlighting(self):
        log.debug("Setting syntax highlighting")
        self.styledText.SetLexer(stc.STC_LEX_PYTHON)
        # Number
        self.styledText.StyleSetSpec(stc.STC_P_NUMBER, "fore:#007F7F,size:%(size)d" % faces)
        # String
        self.styledText.StyleSetSpec(stc.STC_P_STRING, "fore:#7F007F,face:%(helv)s,size:%(size)d" % faces)
        # Single quoted string
        self.styledText.StyleSetSpec(stc.STC_P_CHARACTER, "fore:#7F007F,face:%(helv)s,size:%(size)d" % faces)

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
        
    def copy(self):
        self.styledText.Copy()     

    def paste(self):
        self.styledText.Paste()     

    def cut(self):
        self.styledText.Cut()     

    def undo(self):
        self.styledText.Undo()   

        