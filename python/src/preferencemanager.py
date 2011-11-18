import pickle
import common
import os.path, os

log = common.getLogger(__name__)

class PreferenceManager:
    """This class manages all the user preferences"""
    
    PREFERENCEFILE = "/.pvseditor"
    CONTEXT = "Context"
    RESTORECONTEXT = "RestoreContext"
    FILESBUFFERSTREES = "FilesBuffersTrees"
    PROOFTREE = "ProofTree"
    PVSLOCATION = "PVSLocation"
    
    def __init__(self):
        filename = os.getenv("HOME") + PreferenceManager.PREFERENCEFILE
        if os.path.exists(filename):
            self.loadPreferences()
        else:
            self.preferences = {}
            self.preferences[PreferenceManager.CONTEXT] = "~/Desktop"
            self.preferences[PreferenceManager.RESTORECONTEXT] = True
            self.preferences[PreferenceManager.FILESBUFFERSTREES] = True
            self.preferences[PreferenceManager.PROOFTREE] = True
            self.preferences[PreferenceManager.PVSLOCATION] = "/Applications/pvs-5.0-ix86-MacOSX-allegro/" 
            self.savePreferences()
        
    def loadPreferences(self):
        filename = os.getenv("HOME") + PreferenceManager.PREFERENCEFILE
        output = open(filename, 'r')
        self.preferences = pickle.load(output)
        log.info("Loaded preferences: %s", self.preferences)
        output.close()

    def savePreferences(self):
        filename = os.getenv("HOME") + PreferenceManager.PREFERENCEFILE
        output = open(filename, 'wt')
        pickle.dump(self.preferences, output)
        log.info("Saved preferences: %s", self.preferences)
        output.close()
    
    def getPVSLocation(self):
        return self.preferences[PreferenceManager.PVSLOCATION]   
    
    def restoreContextAutomatically(self):
        return self.preferences[PreferenceManager.RESTORECONTEXT]
    
    def setRestoreContextAutomatically(self, value):
        self.preferences[PreferenceManager.RESTORECONTEXT] = value
    
    def getContext(self):
        return self.preferences[PreferenceManager.CONTEXT]
    
    def setContext(self, context):
        self.preferences[PreferenceManager.CONTEXT] = context
    
    def visibleFilesBuffersTrees(self):
        return self.preferences[PreferenceManager.FILESBUFFERSTREES]
    
    def visibleProofTree(self):
        return self.preferences[PreferenceManager.PROOFTREE]
    
    def setFilesBuffersTrees(self, value):
        self.preferences[PreferenceManager.FILESBUFFERSTREES] = value
    
    def setProofTree(self, value):
        self.preferences[PreferenceManager.PROOFTREE] = value
    