import pickle
import config
import os.path, os

log = config.getLogger(__name__)

class PVSIDEPreferenceManager:
    PREFERENCEFILE = "/.pvseditor"
    CONTEXT = "Context"
    RESTORECONTEXT = "RestoreContext"
    FILESBUFFERSTREES = "FilesBuffersTrees"
    PROOFTREE = "ProofTree"
    PVSLOCATION = "PVSLocation"
    
    def __init__(self):
        filename = os.getenv("HOME") + PVSIDEPreferenceManager.PREFERENCEFILE
        if os.path.exists(filename):
            self.loadPreferences()
        else:
            self.preferences = {}
            self.preferences[PVSIDEPreferenceManager.CONTEXT] = "~/Desktop"
            self.preferences[PVSIDEPreferenceManager.RESTORECONTEXT] = True
            self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES] = True
            self.preferences[PVSIDEPreferenceManager.PROOFTREE] = True
            self.preferences[PVSIDEPreferenceManager.PVSLOCATION] = "/Applications/pvs-5.0-ix86-MacOSX-allegro/" 
            self.savePreferences()
        
    def loadPreferences(self):
        filename = os.getenv("HOME") + PVSIDEPreferenceManager.PREFERENCEFILE
        output = open(filename, 'r')
        self.preferences = pickle.load(output)
        log.info("Loaded preferences: %s", self.preferences)
        output.close()

    def savePreferences(self):
        filename = os.getenv("HOME") + PVSIDEPreferenceManager.PREFERENCEFILE
        output = open(filename, 'wt')
        pickle.dump(self.preferences, output)
        log.info("Saved preferences: %s", self.preferences)
        output.close()
    
    def getPVSLocation(self):
        return self.preferences[PVSIDEPreferenceManager.PVSLOCATION]   
    
    def restoreContextAutomatically(self):
        return self.preferences[PVSIDEPreferenceManager.RESTORECONTEXT]
    
    def setRestoreContextAutomatically(self, value):
        self.preferences[PVSIDEPreferenceManager.RESTORECONTEXT] = value
    
    def getContext(self):
        return self.preferences[PVSIDEPreferenceManager.CONTEXT]
    
    def setContext(self, context):
        self.preferences[PVSIDEPreferenceManager.CONTEXT] = context
    
    def visibleFilesBuffersTrees(self):
        return self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES]
    
    def visibleProofTree(self):
        return self.preferences[PVSIDEPreferenceManager.PROOFTREE]
    
    def setFilesBuffersTrees(self, value):
        self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES] = value
    
    def setProofTree(self, value):
        self.preferences[PVSIDEPreferenceManager.PROOFTREE] = value
    
