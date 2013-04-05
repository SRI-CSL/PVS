import pickle
import common
import os.path, os
import utility

log = common.getLogger(__name__)

class PreferenceManager:
    """This class manages all the user preferences"""
    
    GLOBALPREFERENCEFILE = ".pvseditorglobal"
    CONTEXTPREFERENCEFILE = ".pvseditor"
    CONTEXT = "Context"
    RESTORECONTEXT = "RestoreContext"
    FILESBUFFERSTREES = "FilesBuffersTrees"
    PROOFTREE = "ProofTree"
    PVSLOCATION = "PVSLocation"
    OPENFILES = "OpenFiles"
    
    def __init__(self):
        filename = self.getGlobalPreferenceFilename()
        if os.path.exists(filename):
            self.loadGlobalPreferences()
            self.loadContextPreferences()
        else:
            self.globalPreferences = {}
            self.contextPreferences = {}
            self.setContext("~/Desktop")
            self.setPVSLocation("/Users/saadati/projects/pvs/")             
            self.setRestoreContextAutomatically(True)
            self.setFilesBuffersTrees(True)
            self.setProofTree(True)
            
            self.saveContextPreferences()
            self.saveGlobalPreferences()
        
    def getGlobalPreferenceFilename(self):
        f = os.path.join(os.getenv("HOME"), PreferenceManager.GLOBALPREFERENCEFILE)
        return f
    
    def getContextPreferenceFilename(self):
        f = os.path.join(self.globalPreferences[PreferenceManager.CONTEXT], PreferenceManager.CONTEXTPREFERENCEFILE)
        return f
    
    def loadGlobalPreferences(self):
        filename = self.getGlobalPreferenceFilename()
        output = open(filename, 'r')
        self.globalPreferences = pickle.load(output)
        log.info("Loaded global preferences: %s", self.globalPreferences)
        output.close()

    def saveGlobalPreferences(self):
        filename = self.getGlobalPreferenceFilename()
        output = open(filename, 'wt')
        pickle.dump(self.globalPreferences, output)
        log.info("Saved global preferences: %s", self.globalPreferences)
        output.close()
    
    def loadContextPreferences(self):
        filename = self.getContextPreferenceFilename()
        if os.path.exists(filename):
            output = open(filename, 'r')
            self.contextPreferences = pickle.load(output)
            log.info("Loaded context preferences: %s", self.contextPreferences)
            output.close()
        else:
            self.contextPreferences[PreferenceManager.OPENFILES] = []

    def saveContextPreferences(self):
        filename = self.getContextPreferenceFilename()
        output = open(filename, 'wt')
        pickle.dump(self.contextPreferences, output)
        log.info("Saved context preferences: %s", self.contextPreferences)
        output.close()
    
    def getPVSLocation(self):
        return self.globalPreferences[PreferenceManager.PVSLOCATION]   
    
    def restoreContextAutomatically(self):
        return self.globalPreferences[PreferenceManager.RESTORECONTEXT]
    
    def setRestoreContextAutomatically(self, value):
        self.globalPreferences[PreferenceManager.RESTORECONTEXT] = value
    
    def getContext(self):
        return self.globalPreferences[PreferenceManager.CONTEXT]
    
    def setPVSLocation(self, location):
        self.globalPreferences[PreferenceManager.PVSLOCATION] = utility.normalizePath(location)
    
    def setContext(self, context):
        self.saveContextPreferences()
        newContext = utility.normalizePath(context)
        self.globalPreferences[PreferenceManager.CONTEXT] = newContext
        self.loadContextPreferences()
    
    def visibleFilesBuffersTrees(self):
        return self.globalPreferences[PreferenceManager.FILESBUFFERSTREES]
    
    def visibleProofTree(self):
        return self.globalPreferences[PreferenceManager.PROOFTREE]
    
    def setFilesBuffersTrees(self, value):
        self.globalPreferences[PreferenceManager.FILESBUFFERSTREES] = value
    
    def setProofTree(self, value):
        self.globalPreferences[PreferenceManager.PROOFTREE] = value
        
    def fileOpened(self, fullname):
        self.contextPreferences[PreferenceManager.OPENFILES].append(fullname)
        
    def fileClosed(self, fullname):
        self.contextPreferences[PreferenceManager.OPENFILES].remove(fullname)
        
    def listofOpenFiles(self):
        return self.contextPreferences[PreferenceManager.OPENFILES]
