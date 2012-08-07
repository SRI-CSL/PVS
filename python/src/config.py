import logging, sys

PVS_CONSOLE_HAS_HORIZONTAL_SCROLL = False

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


def getLogger(name):
    log = logging.getLogger(name)
    hdlr = logging.StreamHandler(sys.stdout)
    #hdlr = logging.FileHandler('/var/tmp/myapp.log')
    formatter = logging.Formatter('%(name)s - %(levelname)s - %(message)s')
    hdlr.setFormatter(formatter)
    log.addHandler(hdlr) 
    log.setLevel(logging.DEBUG)
    return log