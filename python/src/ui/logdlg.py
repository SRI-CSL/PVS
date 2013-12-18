import wx
import pvscomm
import constants
from wx.lib.pubsub import setupkwargs, pub 

class PVSCommunicationLogDialog(wx.Frame):
    def __init__(self, parent, title, loggerName, closeButtonText="Close", clearButtonText="Clear"):
        wx.Frame.__init__(self, parent, style = wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER)
        self.logsTextCtrl = wx.TextCtrl(self, wx.ID_ANY, "", style=wx.TE_MULTILINE | wx.TE_READONLY | wx.HSCROLL | wx.TE_WORDWRAP | wx.TE_RICH2)
        closeButton = wx.Button(self, wx.ID_ANY, closeButtonText)
        clearButton = wx.Button(self, wx.ID_ANY, clearButtonText)
        self.loggerName = loggerName

        self.SetTitle(title)
        self.SetSize((521, 461))

        mainSizer = wx.BoxSizer(wx.VERTICAL)
        sizer_2 = wx.BoxSizer(wx.HORIZONTAL)
        mainSizer.Add(self.logsTextCtrl, 6, wx.ALL | wx.EXPAND, 5)
        sizer_2.Add(closeButton, 0, wx.LEFT | wx.RIGHT, 5)
        sizer_2.Add(clearButton, 0, wx.LEFT | wx.RIGHT, 5)
        mainSizer.Add(sizer_2, 0, wx.ALL | wx.ALIGN_CENTER_HORIZONTAL | wx.ALIGN_CENTER_VERTICAL, 5)
        self.SetSizer(mainSizer)
        self.Layout()
        closeButton.Bind(wx.EVT_BUTTON, self.onCloseButton)
        clearButton.Bind(wx.EVT_BUTTON, self.onClearButton)
        pub.subscribe(self.append, constants.PUB_APPENDLOG)
        self.logsTextCtrl.Clear()
        self._attrs = [wx.TextAttr(wx.BLACK), wx.TextAttr(wx.Colour(130, 130, 130))]
        self._attrIndex = 0
        self.append(loggerName, pvscomm.PVSCommunicationLogger().get(loggerName))
        
    def append(self, name, message):
        if name == self.loggerName:
            if isinstance(message, list) or isinstance(message, tuple):
                for mes in message:
                    self.logsTextCtrl.SetDefaultStyle(self._attrs[self._attrIndex])
                    self._attrIndex = 1 - self._attrIndex
                    self.logsTextCtrl.AppendText(mes)
            else:
                self.logsTextCtrl.SetDefaultStyle(self._attrs[self._attrIndex])
                self._attrIndex = 1 - self._attrIndex
                self.logsTextCtrl.AppendText(str(message))
                
        
    def onCloseButton(self, event):
        self.Hide()
        #self.Close()
        event.Skip()
        
    def onClearButton(self, event):
        self._attrIndex = 0
        pvscomm.PVSCommunicationLogger().clear(self.loggerName)
        self.logsTextCtrl.Clear()
        event.Skip()
        
        
        

