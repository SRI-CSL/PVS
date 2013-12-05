
import os.path
import wx
import wx.stc as stc
import logging
import constants
import ui.images
from config import PVSIDEConfiguration
from wx.lib.pubsub import setupkwargs, pub
import remgr

faces = {'default_color': '000000',
         'identity_color': '000000',
         'keyword_color': '0000FF',
         'comment_color': '008B00',
         'number_color': '00CD66',
         'operator_color': '878787',
         'string_color': '1C86EE',
         'times': 'Times',
          'font' : 'Courier',
          'size' : 12,
          'size2': 10,
         }


class PVSStyledText(stc.StyledTextCtrl):
    def __init__(self, parent):
        stc.StyledTextCtrl.__init__(self, parent, wx.ID_ANY, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        self.SetMarginType(0, stc.STC_MARGIN_NUMBER)
        self.SetMarginWidth(0, 30)
        self.SetMarginSensitive(0, True)
        self.namesInformation = []
        cfg = PVSIDEConfiguration()
        faces['default_color'] = cfg.default_color
        faces['identity_color'] = cfg.identity_color
        faces['keyword_color'] = cfg.keyword_color
        faces['comment_color'] = cfg.comment_color
        faces['number_color'] = cfg.number_color
        faces['operator_color'] = cfg.operator_color
        faces['string_color'] = cfg.string_color
        faces['size'] = cfg.font_size
        faces['font'] = cfg.font
        
        self.StyleSetSpec(stc.STC_STYLE_LINENUMBER, "size:%d,face:%s" % (faces['size2'], faces['times']))
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
        self.StyleSetSpec(stc.STC_MATLAB_DEFAULT, "fore:#%(default_color)s,face:%(font)s,size:%(size)d" % faces)
        self.StyleSetSpec(stc.STC_MATLAB_IDENTIFIER, "fore:#%(default_color)s,face:%(font)s,size:%(size)d" % faces)
        # Number
        self.StyleSetSpec(stc.STC_MATLAB_NUMBER, "fore:#%(number_color)s,face:%(font)s,size:%(size)d" % faces)
        # String
        self.StyleSetSpec(stc.STC_MATLAB_DOUBLEQUOTESTRING, "fore:#%(string_color)s,face:%(font)s,size:%(size)d" % faces)
        self.StyleSetSpec(stc.STC_MATLAB_STRING, "fore:#%(string_color)s,face:%(font)s,size:%(size)d" % faces)
        # Operator
        self.StyleSetSpec(stc.STC_MATLAB_OPERATOR, "fore:#%(operator_color)s,face:%(font)s,size:%(size)d" % faces)
        # Keyword
        self.StyleSetSpec(stc.STC_MATLAB_KEYWORD, "fore:#%(keyword_color)s,face:%(font)s,size:%(size)d" % faces)
        # Comment
        self.StyleSetSpec(stc.STC_MATLAB_COMMENT, "fore:#%(comment_color)s,face:%(font)s,size:%(size)d" % faces)

        self.SetKeyWords(0, constants.PVS_KEYWORDS)
        self.SetKeyWords(1, " ".join(constants.PVS_OPERATORS))
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
            declFile = inf[constants.DECLFILE]
            declPlace = inf[constants.DECLPLACE]
            callback = lambda event: self._showDeclaration(event, declFile=declFile, declPlace=declPlace)
            ID = menu.Append(wx.ID_ANY, "Show Declaration", constants.EMPTY_STRING, wx.ITEM_NORMAL).GetId()
            wx.EVT_MENU(menu, ID, callback)       
            self.PopupMenu(menu, event.GetPosition())
            menu.Destroy()     
        event.Skip()
        
    def _showDeclaration(self, event, declFile, declPlace):
        rem = remgr.RichEditorManager()
        openFiles = rem.getOpenFileNames()
        if not declFile in openFiles:
            pub.sendMessage(constants.PUB_ADDFILE, fullname=declFile)
        rem.showRichEditorForFile(declFile)
        fre = rem.getFocusedRichEditor()
        if fre is not None:
            fre.styledText.GotoLine(declPlace[0]-1)
            fre.styledText.SetFocus()

        
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
            text = inf[constants.DECL]
        return text
    
    def _findDecl(self, position, lo=0, hi=None):
        if hi is None:
            hi = len(self.namesInformation)
        while lo < hi:
            mid = (lo+hi)//2
            midval = self.namesInformation[mid]
            place = midval[constants.LPLACE]
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
