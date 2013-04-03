import os

def normalizePath(thePath):
    if thePath.startswith("~"):
        thePath = os.getenv("HOME") + thePath[1:]
    return os.path.abspath(thePath)
