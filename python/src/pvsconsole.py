import wx
import wx.stc as stc
import codecs
import config
from eventhandler import *

log = config.getLogger(__name__)

class PVSConsole(stc.StyledTextCtrl):
    def __init__(self, parent, ID):
        if config.PVS_CONSOLE_HAS_HORIZONTAL_SCROLL:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        else:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.TE_RICH | wx.TE_RICH2)
        self.setBidnings()
            

    def setBidnings(self):
        config.frame.Bind(wx.EVT_TEXT_ENTER, onPVSConsoleTextEntered, self)
        config.frame.Bind(wx.EVT_TEXT, onPVSConsoleText, self)