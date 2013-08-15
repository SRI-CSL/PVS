import wx
import logging
from constants import *
from ui.plugin import PluginPanel

class ProofTreePlugin(PluginPanel):
    def __init__(self, parent, definition):
        PluginPanel.__init__(self, parent, definition)
        self.prooftree = wx.TreeCtrl(self, wx.ID_ANY, style=wx.TR_HAS_BUTTONS | wx.TR_DEFAULT_STYLE | wx.SUNKEN_BORDER)
        panelSizer = wx.BoxSizer(wx.VERTICAL)
        panelSizer.Add(self.prooftree, 1, wx.EXPAND, 0)
        self.SetSizer(panelSizer)
