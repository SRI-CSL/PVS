import wx
import config

class ProofTreeManager(wx.Frame):

    title = "Proof Tree"

    def __init__(self):
        wx.Frame.__init__(self, wx.GetApp().TopWindow, title=self.title)
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        panel = wx.Panel(self, wx.ID_ANY)
        #self.prooflabel = wx.StaticText(self.window_1_pane_2, wx.ID_ANY, LABEL_PROOF_PANEL)
        self.prooftree = wx.TreeCtrl(panel, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        sizer_6 = wx.BoxSizer(wx.VERTICAL)
        #sizer_6.Add(self.prooflabel, 0, wx.ALIGN_CENTER_HORIZONTAL, 0)
        sizer_6.Add(self.prooftree, 1, wx.EXPAND, 0)
        panel.SetSizer(sizer_6)
        self.SetSize((200, 300))
        
    def OnClose(self, event):
        if event.CanVeto():
            self.Hide()
            config.preference.setProofTree(False)
            config.menubar.proofTree.Check(False)
            event.Veto()
        else:
            #if we don't veto it we allow the event to propogate
            event.Skip() 
