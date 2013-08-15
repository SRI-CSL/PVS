
import wx
import wx.stc as stc
import logging
from constants import PVS_KEYWORDS, PVS_OPERATORS

faces = {'default_color': '000000',
         'keyword_color': '0000FF',
         'comment_color': '008B00',
         'number_color': '00CD66',
         'operator_color': '878787',
         'string_color': '1C86EE',
         'times': 'Times',
          'mono' : 'Courier',
          'helv' : 'Helvetica',
          'other': 'new century schoolbook',
          'size' : 12,
          'size2': 10,
         }

issl_table = ';+-?.#~'

# Style Id's
  
(STC_PVS_DEFAULT,
STC_PVS_COMMENT,
STC_PVS_KEYWORD,
STC_PVS_OPERATOR,
STC_PVS_STRING,
STC_PVS_NUMBER) = range(6)

#---- Syntax Style Specs ----#
SYNTAX_ITEMS = [
    (STC_PVS_DEFAULT, 'default_style'),
    (STC_PVS_COMMENT, 'comment_style'),
    (STC_PVS_STRING,   'string_style'),
    (STC_PVS_NUMBER,     'number_style'),
    (STC_PVS_KEYWORD,  'keyword4_style'),
    (STC_PVS_OPERATOR,    'operator_style'),
    ]


class PVSStyledText(stc.StyledTextCtrl):
    def __init__(self, parent):
        stc.StyledTextCtrl.__init__(self, parent, wx.ID_ANY, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        self.SetMarginType(0, stc.STC_MARGIN_NUMBER)
        self.SetMarginWidth(0, 22)
        self.StyleSetSpec(stc.STC_STYLE_LINENUMBER, "size:%d,face:%s" % (faces['size'], faces['mono']))
        self.SetMarginType(1, stc.STC_MARGIN_SYMBOL)
        self.MarkerDefine(0, stc.STC_MARK_ROUNDRECT, "#CCFF00", "RED")
        self.MarkerDefine(1, stc.STC_MARK_CIRCLE, "FOREST GREEN", "SIENNA")
        self.MarkerDefine(2, stc.STC_MARK_SHORTARROW, "blue", "blue")
        self.MarkerDefine(3, stc.STC_MARK_ARROW, "#00FF00", "#00FF00")
        self.SetStyleBits(7)
        self.setSyntaxHighlighting_usingmatlab()
        self.Bind(wx.EVT_SET_CURSOR, self.onCursor)  #TODO: what is this?

    def setSyntaxHighlighting_usingmatlab(self):
        logging.debug("Setting syntax highlighting")
        self.SetLexer(stc.STC_LEX_MATLAB)
        # Default
        self.StyleSetSpec(stc.STC_MATLAB_DEFAULT, "fore:#%(default_color)s,size:%(size)d" % faces)
        # Number
        self.StyleSetSpec(stc.STC_MATLAB_NUMBER, "fore:#%(number_color)s,size:%(size)d" % faces)
        # String
        self.StyleSetSpec(stc.STC_MATLAB_DOUBLEQUOTESTRING, "fore:#%(string_color)s,face:%(helv)s,size:%(size)d" % faces)
        self.StyleSetSpec(stc.STC_MATLAB_STRING, "fore:#%(string_color)s,face:%(helv)s,size:%(size)d" % faces)
        # Operator
        self.StyleSetSpec(stc.STC_MATLAB_OPERATOR, "fore:#%(operator_color)s,face:%(helv)s,size:%(size)d" % faces)
        # Keyword
        self.StyleSetSpec(stc.STC_MATLAB_KEYWORD, "fore:#%(keyword_color)s,face:%(helv)s,size:%(size)d" % faces)
        # Comment
        self.StyleSetSpec(stc.STC_MATLAB_COMMENT, "fore:#%(comment_color)s,face:%(mono)s,size:%(size)d" % faces)

        self.SetKeyWords(0, PVS_KEYWORDS)
        self.SetKeyWords(1, " ".join(PVS_OPERATORS))
        self.GetCurrentPos()
        
    def onCursor(self, event):
        logging.info("Event: %s", event)
