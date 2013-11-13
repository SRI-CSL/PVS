
import os.path
import wx
import wx.stc as stc
import logging
from constants import PVS_KEYWORDS, PVS_OPERATORS, EMPTY_STRING
import ui.images
from config import PVSIDEConfiguration
import preference
import remgr

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
        self.SetMarginWidth(0, 30)
        self.SetMarginSensitive(0, True)
        self.namesInformation = []
        cfg = PVSIDEConfiguration()
        faces['default_color'] = cfg.default_color
        faces['keyword_color'] = cfg.keyword_color
        faces['comment_color'] = cfg.comment_color
        faces['number_color'] = cfg.number_color
        faces['operator_color'] = cfg.operator_color
        faces['string_color'] = cfg.string_color
        faces['size'] = cfg.font_size
        
        self.StyleSetSpec(stc.STC_STYLE_LINENUMBER, "size:%d,face:%s" % (faces['size'], faces['mono']))
        self.SetMarginType(1, stc.STC_MARGIN_SYMBOL)
        self.SetMouseDownCaptures(True)
        self.UsePopUp(0)
        self.MarkerDefine(0, stc.STC_MARK_ROUNDRECT, "#CCFF00", "RED")
        #self.MarkerDefineBitmap(1, ui.images.getBitmap("debug.png"))
        self.MarkerDefine(1, stc.STC_MARK_CIRCLE, "RED", "RED")
        #self.MarkerDefine(1, stc.STC_MARK_CIRCLE, "FOREST GREEN", "SIENNA")
        self.MarkerDefine(2, stc.STC_MARK_SHORTARROW, "blue", "blue")
        self.MarkerDefine(3, stc.STC_MARK_ARROW, "#00FF00", "#00FF00")
        self.SetStyleBits(7)
        self.SetMouseDwellTime(300)
        self.setSyntaxHighlighting_usingmatlab()
        self.Bind(wx.EVT_SET_CURSOR, self.onCursor)  #TODO: what is this?
        self.Bind(stc.EVT_STC_DWELLSTART, self.onMouseDwellStarted)
        #self.Bind(stc.EVT_STC_DWELLEND, self.onMouseDwellEnded)
        self.Bind(wx.EVT_RIGHT_UP, self.onMouseRightClicked)
        self.Bind(stc.EVT_STC_MARGINCLICK, self.onMarginClicked)

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

    def onMarginClicked(self, event):
        #TODO: Implement this later to give information about the Markers
        position = (self.LineFromPosition(event.Position), self.GetColumn(event.Position))
        event.Skip()
        
    def onMouseRightClicked(self, event):
        position = (event.EventObject.GetCurrentLine()+1, event.EventObject.GetCurLineRaw()[1])
        inf = self._findDecl(position)
        if inf is not None:
            menu = wx.Menu()
            declFile = inf["decl-file"]
            declPlace = inf["decl-place"]
            callback = lambda event: self._showDeclaration(event, declFile=declFile, declPlace=declPlace)
            ID = menu.Append(wx.ID_ANY, "Show Declaration", EMPTY_STRING, wx.ITEM_NORMAL).GetId()
            wx.EVT_MENU(menu, ID, callback)       
            self.PopupMenu(menu, event.GetPosition())
            menu.Destroy()     
        event.Skip()
        
    def _showDeclaration(self, event, declFile, declPlace):
        context = preference.Preferences().getRecentContexts()[0]
        fullname = os.path.join(context, declFile)
        rem = remgr.RichEditorManager()
        openFiles = rem.getOpenFileNames()
        if not fullname in openFiles:
            pub.sendMessage(PUB_ADDFILE, fullname=fullname)
        rem.showRichEditorForFile(fullname)
        fre = rem.getFocusedRichEditor()
        if fre is not None:
            fre.styledText.GotoLine(declPlace[0]-1)

        
    def onMouseDwellStarted(self, event):
        text = None
        ePosition = event.GetPosition()
        if event.Position > -1:
            position = (self.LineFromPosition(ePosition), self.GetColumn(ePosition))
            text = self._getTooltipText(position)
        if text is not None:
            self.SetToolTipString(text)
        else:
            self.SetToolTip(None)
        event.Skip()
    
    def _getTooltipText(self, position):
        (row, column) = position # row and column here both start at 0.
        text = None
        row = row + 1 # PVS Columns start at 0, Rows start at 1
        inf = self._findDecl((row, column))
        if inf is not None:
            text = inf["decl"]
        return text
    
    def _findDecl(self, position, lo=0, hi=None):
        if hi is None:
            hi = len(self.namesInformation)
        while lo < hi:
            mid = (lo+hi)//2
            midval = self.namesInformation[mid]
            place = midval["place"]
            begin = (place[0], place[1])
            end = (place[2], place[3])
            if end < position:
                lo = mid+1
            elif begin > position: 
                hi = mid
            else:
                return midval
        return None
    
    def onCursor(self, event):
        pass
        #logging.debug("Event: %s", event)
