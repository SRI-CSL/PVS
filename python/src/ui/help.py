import wx
import config
import logging
import util
import os.path
from constants import *
import wx.html

class HelpFrame(wx.Frame):
    """This will show the help"""
    
    def __init__(self,):
        wx.Frame.__init__(self, util.getMainFrame(), wx.ID_ANY)
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        self.SetTitle("PVS IDE Help")
        help = wx.html.HtmlWindow(self, wx.ID_ANY, style=wx.NO_BORDER)
        helpFile = os.path.join(config.PVSIDEConfiguration().applicationFolder, "src", "help.html")
        mainSizer.Add(help, 1, wx.EXPAND, 5)
        self.SetSizer(mainSizer)
        help.LoadPage(helpFile)
        self.SetSize((500, 600))
        
