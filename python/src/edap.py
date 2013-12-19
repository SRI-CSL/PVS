#!/usr/bin/env python

# This is the main entry point of the editor.

import wx
import util
import constants
import platform
import time
import logging
import logging.config
import ui.images
import ui.frame
import ui.plugin
import config

class PVSEditorApp(wx.App):
    """The main class that starts the application and shows the main frame"""
    
    def OnInit(self):
        #wx.InitAllImageHandlers()
        
        # Splash Screen:
        #splash = wx.SplashScreen(ui.images.getIDELogo(), wx.SPLASH_CENTRE_ON_SCREEN|wx.SPLASH_TIMEOUT, 1000, None, style=wx.SIMPLE_BORDER|wx.STAY_ON_TOP)
        #time.sleep(1)
        
        #Initiate Main Frame:
        mainFrame = ui.frame.MainFrame(None, wx.ID_ANY, "")
        self.SetTopWindow(mainFrame)
        #favicon = wx.Icon(config.PVSIDEConfiguration().applicationFolder + "/images/pvs.ico", wx.BITMAP_TYPE_ICO, 32, 32)
        #wx.Frame.SetIcon(mainFrame, favicon)
        mainFrame.Show()
        logging.info("Main Frame initialized...") 
        pm = ui.plugin.PluginManager()
        pm.initializePlugins(config.PVSIDEConfiguration().pluginDefinitions)
        operatingSystem = platform.system()
        if operatingSystem == "Windows":
            mainFrame.showDialogBox("PVS does not run on Windows", constants.WARNING)
        mainFrame.restoreOpenFiles()
        return 1