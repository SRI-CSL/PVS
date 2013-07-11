
# This File contains all the generic function calls 

import logging, sys, wx, os, os.path
import constants

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

def getFilenameFromFullPath(fullname, includeExtension=True):
    filename = os.path.split(fullname)[1]
    return filename if includeExtension else os.path.splitext(filename)[0]

def getMainFrame():
    return wx.GetApp().GetTopWindow()

def auiManager():
    return wx.GetApp().GetTopWindow().auiManager

class PVSException(Exception):
    pass

#    def __init__(self, *args, **keywords):
#        Exception.__init__(self, *args, **keywords)

class PVSIDEException(Exception):
    def __init__(self, *args, **keywords):
        Exception.__init__(self, *args, **keywords)

class XMLRPCException(Exception):
    def __init__(self, *args, **keywords):
        Exception.__init__(self, *args, **keywords)
