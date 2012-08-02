import wx
import wx.stc as stc
import codecs
from config import PVS_CONSOLE_HAS_HORIZONTAL_SCROLL, getLogger

log = getLogger(__name__)

class PVSConsole(stc.StyledTextCtrl):
    def __init__(self, parent, ID):
        if PVS_CONSOLE_HAS_HORIZONTAL_SCROLL:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        else:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.TE_RICH | wx.TE_RICH2)
            
