import pickle
import config

log = config.getLogger(__name__)

class PVSIDEPreferenceManager:
    PREFERENCEFILE = "~/.pvseditor"
    CONTEXT = "Context"
    RESTORECONTEXT = "RestoreContext"
    FILESBUFFERSTREES = "FilesBuffersTrees"
    PROOFTREE = "ProofTree"
    
    def __init__(self):
        self.preferences = {}
        self.preferences[PVSIDEPreferenceManager.CONTEXT] = "~/Desktop"
        self.preferences[PVSIDEPreferenceManager.RESTORECONTEXT] = True
        self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES] = True
        self.preferences[PVSIDEPreferenceManager.PROOFTREE] = True
        
    def loadPreferences(self):
        output = open(PVSIDEPreferenceManager.PREFERENCEFILE, 'rb')
        self.preferences = pickle.load(output)
        log.info("Loaded preferences: %s", self.preferences)
        output.close()

    def savePreferences(self):
        output = open(PVSIDEPreferenceManager.PREFERENCEFILE, 'wb')
        pickle.dump(self.preferences, output)
        log.info("Saved preferences: %s", self.preferences)
        output.close()
    
    def restoreContextAutomatically(self):
        return self.preferences[PVSIDEPreferenceManager.RESTORECONTEXT]
    
    def getRestoredContext(self):
        return self.preferences[PVSIDEPreferenceManager.CONTEXT]
    
    def visibleFilesBuffersTrees(self):
        return self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES]
    
    def visibleProofTree(self):
        return self.preferences[PVSIDEPreferenceManager.PROOFTREE]
    
    def setFilesBuffersTrees(self, value):
        self.preferences[PVSIDEPreferenceManager.FILESBUFFERSTREES] = value
    
    def setProofTree(self, value):
        self.preferences[PVSIDEPreferenceManager.PROOFTREE] = value
    
