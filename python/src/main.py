#!/usr/bin/env python

# This is the main entry point of the editor.

import wx, os.path, sys
import util
import constants
import platform
import time
import logging
import logging.config
import ui.images
import ui.frame
import ui.plugin
from wx.lib.pubsub import setupkwargs, pub 
import config
import pvscomm
import gc
import argparse

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

# end of class PVSEditorApp

def readCommandLineArguments():
    parser = argparse.ArgumentParser()
    parser.add_argument("-pvs", nargs=1, help="specify the PVS URL", dest="pvsURL")
    parser.add_argument("-url", nargs=1, help="specify the GUI URL", dest="guiURL")
    parser.add_argument("-ll", nargs=1, help="set the logging level to one of: debug, info, warning, error", dest="level")
    args = parser.parse_args()
    return args

def processCommandLineArguments(args):
    pvsURLs = args.pvsURL
    ideURLs = args.guiURL
    logLevels = args.level
    cfg = config.PVSIDEConfiguration()
    if pvsURLs is not None:
        cfg.pvsURL = pvsURLs[0]
    if ideURLs is not None:
        cfg.ideURL = ideURLs[0]
    if logLevels is not None:
        levels = {"debug": logging.DEBUG, "info": logging.INFO, "warning": logging.WARN, "warn": logging.WARN, "error": logging.ERROR, "critical": logging.CRITICAL}
        if logLevels[0] in levels:
            logging.getLogger(constants.LROOT).setLevel(levels[logLevels[0]])
            
def processConfigFile(applicationFolder):
    cfg = config.PVSIDEConfiguration()
    cfg.initialize(applicationFolder)

def configLogger(applicationFolder):
    """Return a logger for the given name"""
    logConfigFile = os.path.join(applicationFolder, "src/logging.cfg")
    if os.path.exists(logConfigFile):
        logging.config.fileConfig(logConfigFile)
    else:
        print "Could not find the config file for logging. Using default settings..."
        hdlr = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter("[%(module)s %(funcName)s] %(levelname)s - %(message)s")
        hdlr.setFormatter(formatter)
        rootLogger = logging.getLogger(constants.LROOT)
        rootLogger.addHandler(hdlr) 
        rootLogger.setLevel(logging.DEBUG)

def downloadFiles(applicationFolder):
    #TODO: I might decide to delete this function completely
    if logging.getLogger(constants.LROOT).getEffectiveLevel() == logging.DEBUG:
        DESTINATION = os.path.join(applicationFolder, "src")
        JSONFILE = os.path.join(DESTINATION, "pvs-gui.json")
        if not os.path.exists(JSONFILE):
            PVS_GUI_JSON = constants.PVS_GITHUB_REPOSITORY + "/src/interface/pvs-gui.json"
            try:
                import requests
                logging.info("Downloading %s", PVS_GUI_JSON)
                r = requests.get(PVS_GUI_JSON)
                jFile = open(JSONFILE, "w")
                jFile.write(r.text)
                jFile.close()
            except Exception:
                logging.error("Please either install the 'requests' package, or download %s and copy it under %s", PVS_GUI_JSON, DESTINATION)
                logging.info("Setting the logging level to INFO")
                logging.getLogger(constants.LROOT).setLevel(logging.INFO)
                
def verifyPythonPackagesAreFine():
    #logging.debug("PubSub version is %s", pub.PUBSUB_VERSION)
    assert (pub.PUBSUB_VERSION == 3), "This application requires PUBSUB version 3 or higher."
    ALWAYSNEEDED = 2
    NEEDEDFORDEBUG = 1
    OPTIONAL = 0
    PACKAGELIST = [("wx", "http://www.wxpython.org/", ALWAYSNEEDED), \
                   ("pyparsing", "http://pyparsing.wikispaces.com/", ALWAYSNEEDED), \
                   ("jsonschema", "https://github.com/Julian/jsonschema", NEEDEDFORDEBUG), \
                   ("requests", "http://www.wxpython.org/", OPTIONAL), \
                   ]
    necessaryPackageMissing = False
    debugPackageMissing = False
    optionalPackageMissing = False
    for PACKAGE in PACKAGELIST:
        (name, website, whenNeeded) = PACKAGE
        try:
            __import__(name)
        except ImportError:
            logging.critical("Please install the '%s' package for Python by visiting %s", name, website)
            if whenNeeded == ALWAYSNEEDED:
                necessaryPackageMissing = True
            elif whenNeeded == NEEDEDFORDEBUG:
                debugPackageMissing = True
            else:
                optionalPackageMissing = True
    if necessaryPackageMissing:
        logging.critical("Application exiting since some necessary packages are missing")
        sys.exit(2)
    if debugPackageMissing and logging.getLogger(constants.LROOT).getEffectiveLevel() == logging.DEBUG:
        logging.info("Setting the logging level to INFO")
        logging.getLogger(constants.LROOT).setLevel(logging.INFO)            
    if optionalPackageMissing:
        logging.warning("The application can still run without the optional packages")
        
if __name__ == "__main__":
    verifyPythonPackagesAreFine()
    args = readCommandLineArguments()
    utilDirectory = os.path.dirname(util.__file__)
    applicationFolder = os.path.abspath(os.path.join(utilDirectory, os.path.pardir))
    configLogger(applicationFolder)
    processConfigFile(applicationFolder)
    logging.debug("Application Folder is %s", applicationFolder)
    processCommandLineArguments(args)
    #downloadFiles(applicationFolder)
    gc.enable()
    gc.set_threshold(1, 2, 3)
    application = PVSEditorApp(0)
    logging.info("Entering MainLoop...")
    #pvscomm.PVSCommandManager().ping()
    
    application.MainLoop()
