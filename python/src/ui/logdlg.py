import wx
import pvscomm

class PVSCommunicationLogDialog(wx.Dialog):
    def __init__(self, parent, logs):
        wx.Dialog.__init__(self, parent, style = wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER)
        self.logsTextCtrl = wx.TextCtrl(self, wx.ID_ANY, "", style=wx.TE_MULTILINE | wx.TE_READONLY | wx.HSCROLL | wx.TE_WORDWRAP)
        closeButton = wx.Button(self, wx.ID_ANY, "Close")
        clearButton = wx.Button(self, wx.ID_ANY, "Clear Log")

        self.SetTitle("PVS Communication Log")
        self.SetSize((521, 461))

        mainSizer = wx.BoxSizer(wx.VERTICAL)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.logsTextCtrl, 6, wx.ALL | wx.EXPAND, 5)
        sizer_2.Add(closeButton, 0, wx.LEFT | wx.RIGHT, 5)
        sizer_2.Add(clearButton, 0, wx.LEFT | wx.RIGHT, 5)
        mainSizer.Add(sizer_2, 0, wx.ALL | wx.ALIGN_CENTER_HORIZONTAL | wx.ALIGN_CENTER_VERTICAL, 5)
        self.SetSizer(mainSizer)
        self.Layout()
        
        logText = "\n".join(logs)
        self.logsTextCtrl.SetValue(logText)
        closeButton.Bind(wx.EVT_BUTTON, self.onCloseButton)
        clearButton.Bind(wx.EVT_BUTTON, self.onClearButton)
        
    def onCloseButton(self, event):
        self.EndModal(0)
        event.Skip()
        
    def onClearButton(self, event):
        pvscomm.PVSCommunicationLogger().clear()
        self.logsTextCtrl.SetValue("")
        event.Skip()
        
        
        

