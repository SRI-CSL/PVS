
# This File contans all the global objects for different parts of the GUI 
# and a function to get the logger for each module


import logging, sys, wx

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
filestreemanager = None
bufferstree = None
filesbuffermanager = None
prooftreemanager = None
runner = None
preference = None 

def getLogger(name):
    log = logging.getLogger(name)
    hdlr = logging.StreamHandler(sys.stdout)
    #hdlr = logging.FileHandler('/var/tmp/myapp.log')
    formatter = logging.Formatter('%(name)s - %(levelname)s - %(message)s')
    hdlr.setFormatter(formatter)
    log.addHandler(hdlr) 
    log.setLevel(logging.DEBUG)
    return log

