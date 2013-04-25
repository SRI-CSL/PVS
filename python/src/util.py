
# This File contans all the global objects for different parts of the GUI 
# and a function to get the logger for each module
import logging, sys, wx, os, os.path
import constants
import ui.dialogs

PVS_CONSOLE_HAS_HORIZONTAL_SCROLL = False
EVT_RESULT_ID = wx.NewId()

# Common GUI Objects:
editor = None
frame = None
menubar = None
toolbar = None
statusbar = None
notebook = None
console = None
filesTreeManager = None
buffersTree = None
filesBuffersManager = None
proofTreeManager = None
runner = None
preference = None 

def getLogger(name):
    """Return a logger for the given name"""
    log = logging.getLogger(name)
    hdlr = logging.StreamHandler(sys.stdout)
    #hdlr = logging.FileHandler('/var/tmp/myapp.log')
    formatter = logging.Formatter('%(name)s - %(levelname)s - %(message)s')
    hdlr.setFormatter(formatter)
    log.addHandler(hdlr) 
    log.setLevel(constants.LOGGER_LEVEL)
    return log

def normalizePath(thePath):
    """Replace ~ with the home directory"""
    if thePath.startswith("~"):
        thePath = getHomeDirectory() + thePath[1:]
    return os.path.abspath(thePath)

def getHomeDirectory():
    """return user's home directory"""
    return os.getenv("HOME")

def getFilenameFromFullPath(fullname):
    return os.path.split(fullname)[1]

def ensureFilesAreSavedToPoceed():
    global filesBuffersManager
    richEditors = filesBuffersManager.files.values()
    filesAreSaved = True
    for richEditor in richEditors:
        if richEditor.styledText.GetModify():
            filesAreSaved = False
            break

    safeToProceed = True
    if not filesAreSaved:
        choice = ui.dialogs.askYesNoCancelQuestion("Some files have been modified. Save changes?")
        if choice == wx.ID_YES:
            filesBuffersManager.saveAllFiles()
        elif choice == wx.ID_CANCEL:
            safeToProceed = False
    return safeToProceed
    

class PVSException(Exception):
    def __init__(self):
        Exception.__init__(self)
