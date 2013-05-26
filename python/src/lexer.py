import wx, re, keyword
from wx import stc

class BaseLexer(object):
    """Defines simple interface for custom lexer objects"""
    def __init__(self):
        super(BaseLexer, self).__init__()

    def StyleText(self, event):
        raise NotImplementedError


class SCT_LEX_ERLANG_IDNOISE(BaseLexer):
    STC_ERLANG_IDNOISE_DEFAULT = 1
    STC_ERLANG_IDNOISE_VARIABLE = 2
    STC_ERLANG_IDNOISE_ATOM = 3
    STC_ERLANG_IDNOISE_MODULE = 4
    STC_ERLANG_IDNOISE_KEYWORD = 5
    STC_ERLANG_IDNOISE_COMMENT = 6
    STC_ERLANG_IDNOISE_MACROS = 7
    STC_ERLANG_IDNOISE_NUMBER = 8

    def __init__(self, control):
        super(SCT_LEX_ERLANG_IDNOISE, self).__init__(control)
        self.typeFormatDict = {}
        self.typeFormatDict["other"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_DEFAULT
        self.typeFormatDict["variable"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_VARIABLE
        self.typeFormatDict["atom"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_ATOM
        self.typeFormatDict["module"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_MODULE
        self.typeFormatDict["keyword"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_KEYWORD
        self.typeFormatDict["comment"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_COMMENT
        self.typeFormatDict["macros"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_MACROS
        self.typeFormatDict["number"] = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_NUMBER

    def StyleText(self, event):
        start = self.control.GetEndStyled()
        end = event.GetPosition()
        line = self.control.LineFromPosition(start)
        start = self.control.PositionFromLine(line)
        text = self.control.GetTextRange(start, end)

        self.control.StartStyling(start, 0x1f)
        lastEnd = 0
        for type, start, end, value in getHighlightRules(text):
            style = SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_DEFAULT
            if start > lastEnd:
                self.control.SetStyling(start - lastEnd, style)
            if type in self.typeFormatDict:
                style = self.typeFormatDict[type]
            self.control.SetStyling(len(value), style)
            lastEnd = end

faces = { 'times': 'Times',
          'mono' : 'Courier',
          'helv' : 'Helvetica',
          'other': 'new century schoolbook',
          'size' : 12,
          'size2': 10,
         }

class CustomSTC(stc.StyledTextCtrl):
    FORMATS = {"other": "", "variable": "", "atom": "", "module": "", "keyword": "", "comment": "", "macros": "", "number": "fore:#00CD66,size:%(size)d" % faces }
    lineFont = ""
    
    def __init__(self, parent):
        super(CustomSTC, self).__init__(parent)
        self.custlex = SCT_LEX_ERLANG_IDNOISE(self)

        #self.SetKeyWords(0, kwlist)

        self.SetLexer(stc.STC_LEX_CONTAINER)
        self.EnableLineNumbers()

        self.StyleSetSpec(stc.STC_STYLE_DEFAULT, CustomSTC.formats["other"])
        self.StyleClearAll()
        self.StyleSetSpec(stc.STC_STYLE_LINENUMBER, CustomSTC.lineFont)
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_DEFAULT, CustomSTC.FORMATS["other"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_VARIABLE, CustomSTC.FORMATS["variable"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_ATOM, CustomSTC.FORMATS["atom"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_MODULE, CustomSTC.FORMATS["module"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_KEYWORD, CustomSTC.FORMATS["keyword"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_COMMENT, CustomSTC.FORMATS["comment"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_MACROS, CustomSTC.FORMATS["macros"])
        self.StyleSetSpec(SCT_LEX_ERLANG_IDNOISE.STC_ERLANG_IDNOISE_NUMBER, CustomSTC.FORMATS["number"])

        # Event Handlers
        self.Bind(stc.EVT_STC_STYLENEEDED, self.OnStyle)

    def EnableLineNumbers(self, enable=True):
        """Enable/Disable line number margin"""
        if enable:
            self.SetMarginType(1, stc.STC_MARGIN_NUMBER)
            self.SetMarginMask(1, 0)
            self.SetMarginWidth(1, 35)
        else:
            self.SetMarginWidth(1, 0)

    def OnStyle(self, event):
        if self.custlex:
            self.custlex.StyleText(event)
        else:
            event.Skip()