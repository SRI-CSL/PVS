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
        self.globalPreferences = {}
        self.contextPreferences = {}
        self.loadGlobalPreferences()
        self.loadContextPreferences()

    def loadDefaultGlobalPreferences(self):
        self.setContext(util.getHomeDirectory())
        self.setPVSLocation(os.path.join(util.getHomeDirectory(), PVS_L))
        self.contextPreferences[PreferenceManager.OPENFILES] = []
        self.setContextPreferencesRestoredAutomatically(True)
        self.setVisibleFilesBuffersTrees(True)
        self.setVisibleProofTree(True)
        self.setVisibleToolbar(True)

    def loadDefaultContextPreferences(self):
        self.contextPreferences[PreferenceManager.OPENFILES] = []

    def getValue(self, key, default=None):
        if self.contextPreferences.has_key(key):
            log.info("'%s' was found in context preference. Value: %s", key, self.contextPreferences[key])
            return self.contextPreferences[key]
        elif self.globalPreferences.has_key(key):
            log.info("'%s' was found in global preference. Value: %s", key, self.globalPreferences[key])
            return self.globalPreferences[key]
        log.info("'%s' was not found in either preferences", key)
        return default
        
    def getGlobalPreferenceFilename(self):
        """return the full path to the global preference file"""
        f = os.path.join(util.getHomeDirectory(), PreferenceManager.GLOBALPREFERENCEFILE)
        return f
    
    def getContextPreferenceFilename(self):
        """return the full path to the context preference file"""
        f = os.path.join(self.getContext(), PreferenceManager.CONTEXTPREFERENCEFILE)
        return f
    
    def loadGlobalPreferences(self):
        try:
            filename = self.getGlobalPreferenceFilename()
            output = open(filename, 'r')
            self.globalPreferences = pickle.load(output)
            log.info("Loaded global preferences: %s", self.globalPreferences)
            output.close()
        except:
            log.error("Error loading the global preference file. Loading the defaults...")
            self.loadDefaultGlobalPreferences()

    def saveGlobalPreferences(self):
        filename = self.getGlobalPreferenceFilename()
        output = open(filename, 'wt')
        pickle.dump(self.globalPreferences, output)
        log.info("Saved global preferences: %s", self.globalPreferences)
        output.close()
    
    def loadContextPreferences(self):
        try:
            filename = self.getContextPreferenceFilename()
            output = open(filename, 'r')
            self.contextPreferences = pickle.load(output)
            log.info("Loaded context preferences: %s", self.contextPreferences)
            output.close()
        except:
            log.error("Error loading the context preference file. Loading the defaults...")
            self.loadDefaultContextPreferences()
        

    def saveContextPreferences(self):
        filename = self.getContextPreferenceFilename()
        output = open(filename, 'wt')
        filenames = []
        for fullname in util.filesBuffersManager.files.keys():
            filenames.append(util.getFilenameFromFullPath(fullname))
        self.contextPreferences[PreferenceManager.OPENFILES] = filenames
        pickle.dump(self.contextPreferences, output)
        log.info("Saved context preferences: %s", self.contextPreferences)
        output.close()
    
    # Global Settings:
    
    def getPVSLocation(self):
        """return the path to the pvs directory"""
        return self.getValue(PreferenceManager.PVSLOCATION, util.getHomeDirectory())
    
    def setPVSLocation(self, location):
        self.globalPreferences[PreferenceManager.PVSLOCATION] = util.normalizePath(location)
    
    def getContextPreferencesRestoredAutomatically(self):
        """return true only if the context preference is to be loaded automatically"""
        return self.getValue(PreferenceManager.RESTORECONTEXT, False)
    
    def setContextPreferencesRestoredAutomatically(self, value):
        self.globalPreferences[PreferenceManager.RESTORECONTEXT] = value
    
    def getContext(self):
        """return the last active context"""
        return self.getValue(PreferenceManager.CONTEXT, util.getHomeDirectory())
    
    def setContext(self, context):
        newContext = util.normalizePath(context)
        self.globalPreferences[PreferenceManager.CONTEXT] = newContext
        self.loadContextPreferences()
    
    def getVisibleFilesBuffersTrees(self):
        """return true only if the FilesBuffersTree manager is to be visible"""
        return self.getValue(PreferenceManager.FILESBUFFERSTREES, True)
    
    def setVisibleFilesBuffersTrees(self, value):
        """return whether the FilesBuffersTree manager should be visible"""
        self.globalPreferences[PreferenceManager.FILESBUFFERSTREES] = value
    
    def getVisibleProofTree(self):
        """return true only if the ProofTree manager is to be visible"""
        return self.getValue(PreferenceManager.PROOFTREE, True)
    
    def setVisibleProofTree(self, value):
        """return whether the ProofTree manager should be visible"""
        self.globalPreferences[PreferenceManager.PROOFTREE] = value
        
    def getVisibleToolbar(self):
        """return true only if the toolbar is to be visible"""
        return self.getValue(PreferenceManager.TOOLBAR, True)
    
    def setVisibleToolbar(self, value):
        """return whether the toolbar should be visible"""
        self.globalPreferences[PreferenceManager.TOOLBAR] = value
        
    # Local Settings with respect to a context:

    def listOfOpenFiles(self):
        return self.getValue(PreferenceManager.OPENFILES, [])
