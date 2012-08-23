
# This class manages the GUI's console for PVS

import wx
import wx.stc as stc
import codecs
import config

log = config.getLogger(__name__)

class PVSConsole(stc.StyledTextCtrl):
    def __init__(self, parent, ID):
        if config.PVS_CONSOLE_HAS_HORIZONTAL_SCROLL:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.HSCROLL | wx.TE_RICH | wx.TE_RICH2)
        else:
            stc.StyledTextCtrl.__init__(self, parent, ID, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TE_MULTILINE | wx.TE_RICH | wx.TE_RICH2)
#        self.text_ctrl_2 = wx.TextCtrl(self.panel_4, wx.ID_ANY, "", style=wx.TE_PROCESS_ENTER)
        self.setBidnings()
            

    def setBidnings(self):
        config.frame.Bind(wx.EVT_TEXT_ENTER, self.onPVSConsoleTextEntered, self)
        config.frame.Bind(wx.EVT_TEXT, self.onPVSConsoleText, self)
        
    def onPVSConsoleEntered(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleEntered' not implemented!")
        #event.Skip()

    def onPVSConsoleChanged(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleChanged' not implemented!")
        #event.Skip()

    def onPVSConsoleTextEntered(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleTextEntered' not implemented")
        #event.Skip()

    def onPVSConsoleText(self, event):  # wxGlade: PVSMainFrame.<event_handler>
        log.info("Event handler `onPVSConsoleText' not implemented")
        #event.Skip()
        
    def write(self, text):
        self.AppendText(text)
    
    def fileno(self):
        return 47
    
