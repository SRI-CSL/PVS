
# This File contains all the generic function calls 

import logging, sys, wx, os, os.path
import sys
from sxp import sexp

def normalizePath(thePath):
    """Replace ~ with the home directory"""
    if thePath.startswith("~"):
        thePath = getHomeDirectory() + thePath[1:]
    return os.path.abspath(thePath)

def isS_Expression(text):
    """Return true is text is an s-expression"""
    try:
        sexp.parseString(text, parseAll=True)
        return True
    except Exception:
        logging.debug("Text '%s' is not an S-Expression", text)
    return False

def getHomeDirectory():
    """return user's home directory"""
    return normalizePath(os.getenv("HOME"))

def getFilenameFromFullPath(fullname, includeExtension=True):
    filename = os.path.split(fullname)[1]
    return filename if includeExtension else os.path.splitext(filename)[0]

def getMainFrame():
    return wx.GetApp().GetTopWindow()

def auiManager():
    return wx.GetApp().GetTopWindow().auiManager

class PVSException(Exception):
    def __init__(self, message, code=0, data=""):
        Exception.__init__(self, message, code, data)
        self.message = message
        self.code = code
        self.data = data

class PVSIDEException(Exception):
    def __init__(self, *args, **keywords):
        Exception.__init__(self, *args, **keywords)

class XMLRPCException(Exception):
    def __init__(self, *args, **keywords):
        Exception.__init__(self, *args, **keywords)
