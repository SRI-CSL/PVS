import pickle
import util
import os.path, os
import util
from constants import PVS_L

log = util.getLogger(__name__)

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
    TOOLBAR = "Toolbar"
    
    def __init__(self):
        filename = self.getGlobalPreferenceFilename()
        if os.path.exists(filename):
            self.loadGlobalPreferences()
            self.loadContextPreferences()
        else:
            self.globalPreferences = {}
            self.contextPreferences = {}
            self.setContext(util.getHomeDirectory())
            self.setPVSLocation(os.path.join(util.getHomeDirectory(), PVS_L))             
            self.setContextPreferencesRestoredAutomatically(True)
            self.setVisibleFilesBuffersTrees(True)
            self.setVisibleProofTree(True)
            self.setVisibleToolbar(True)
            
            self.saveContextPreferences()
            self.saveGlobalPreferences()

    def getValue(key):
        if self.contextPreferences.has_key(key):
            log.info("%s was found in context preference", key)
            return self.contextPreferences[key]
        elif self.globalPreferences.has_key(key):
            log.info("%s was found in global preference", key)
            return self.globalPreferences[key]
        log.info("%s was not found in either preferences", key)
        return None
        
    def getGlobalPreferenceFilename(self):
        """return the full path to the global preference file"""
        f = os.path.join(util.getHomeDirectory(), PreferenceManager.GLOBALPREFERENCEFILE)
        return f
    
    def getContextPreferenceFilename(self):
        """return the full path to the context preference file"""
        f = os.path.join(self.getContext(), PreferenceManager.CONTEXTPREFERENCEFILE)
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
    
    # Global Settings:
    
    def getPVSLocation(self):
        """return the path to the pvs directory"""
        return self.globalPreferences[PreferenceManager.PVSLOCATION]   
    
    def setPVSLocation(self, location):
        self.globalPreferences[PreferenceManager.PVSLOCATION] = util.normalizePath(location)
    
    def getContextPreferencesRestoredAutomatically(self):
        """return true only if the context preference is to be loaded automatically"""
        return self.globalPreferences[PreferenceManager.RESTORECONTEXT]
    
    def setContextPreferencesRestoredAutomatically(self, value):
        self.globalPreferences[PreferenceManager.RESTORECONTEXT] = value
    
    def getContext(self):
        """return the last active context"""
        if self.globalPreferences.has_key(PreferenceManager.CONTEXT):
            return self.globalPreferences[PreferenceManager.CONTEXT]
        return util.getHomeDirectory()
    
    def setContext(self, context):
        self.saveContextPreferences()
        newContext = util.normalizePath(context)
        self.globalPreferences[PreferenceManager.CONTEXT] = newContext
        self.loadContextPreferences()
    
    def getVisibleFilesBuffersTrees(self):
        """return true only if the FilesBuffersTree manager is to be visible"""
        return self.globalPreferences[PreferenceManager.FILESBUFFERSTREES]
    
    def setVisibleFilesBuffersTrees(self, value):
        """return whether the FilesBuffersTree manager should be visible"""
        self.globalPreferences[PreferenceManager.FILESBUFFERSTREES] = value
    
    def getVisibleProofTree(self):
        """return true only if the ProofTree manager is to be visible"""
        return self.globalPreferences[PreferenceManager.PROOFTREE]
    
    def setVisibleProofTree(self, value):
        """return whether the ProofTree manager should be visible"""
        self.globalPreferences[PreferenceManager.PROOFTREE] = value
        
    def getVisibleToolbar(self):
        """return true only if the toolbar is to be visible"""
        return self.globalPreferences[PreferenceManager.TOOLBAR]
    
    def setVisibleToolbar(self, value):
        """return whether the toolbar should be visible"""
        self.globalPreferences[PreferenceManager.TOOLBAR] = value
        
    # Local Settings with respect to a context:
    
    def onFileOpened(self, fullname):
        """called when a file is opened"""
        self.contextPreferences[PreferenceManager.OPENFILES].append(fullname)
        
    def onFileClosed(self, fullname):
        """called when a file is closed"""
        self.contextPreferences[PreferenceManager.OPENFILES].remove(fullname)
        
    def listofOpenFiles(self):
        """returns a list of open files"""
        return self.contextPreferences[PreferenceManager.OPENFILES]
