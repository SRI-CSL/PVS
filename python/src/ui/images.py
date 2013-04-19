
# This file contains a number of functions that return bitmap to differetn requireed images and icons


import wx
import constants
import os.path

def getBitmap(filename):
    return wx.Bitmap(os.path.join(constants.IMAGE_FOLDER_PATH, filename), wx.BITMAP_TYPE_ANY)

def getFolderImage():
    return getBitmap("folder.gif")

def getPVSLogo():
    return getBitmap("pvslogo.png")

def getNewImage():
    return getBitmap("newfile.png")

def getOpenImage():
    return getBitmap("openfile.png")

def getSaveImage():
    return getBitmap("save.png")

def getSaveAllImage():
    return getBitmap("saveall.png")

def getCutImage():
    return getBitmap("cut.png")

def getCopyImage():
    return getBitmap("copy.png")

def getPasteImage():
    return getBitmap("paste.png")

def getStartPVSImage():
    return getBitmap("start.png")

def getStopPVSImage(enable=True):
    return getBitmap("stop.png") if enable else getBitmap("stop-disable.png")
    
def getTypecheckImage():
    return getBitmap("typecheck.png")

def getTheoryImage():
    return getBitmap("theory.png")

def getFormulaImage():
    return getBitmap("formula.png")


