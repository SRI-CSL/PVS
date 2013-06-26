import pickle
import util
import os.path, os
import constants
import sets

log = util.getLogger(__name__)

class Preferences:
    """This class manages all the user preferences"""
    __shared_state = {}
    
    GLOBALPREFERENCEFILE = ".pvseditorglobal"
    CONTEXTPREFERENCEFILE = ".pvseditor"
    CONTEXT = "Context"
    RESTORECONTEXT = "RestoreContext"
    PVSLOCATION = "PVSLocation"
    VISIBLEPLUGINS = "VisiblePlusings"
    IGNORE_GLOBALPREFERENCEFILE  = False # if True, load the default global preference
    IGNORE_CONTEXTPREFERENCEFILE = False # if True, load the default context preference
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        
    def loadPreferences(self):
        """load both global and context preferences"""
        filename = self.getGlobalPreferenceFilename()
        self.globalPreferences = {}
        self.contextPreferences = {}
        self.loadGlobalPreferences()
        #self.loadContextPreferences()

    def loadDefaultGlobalPreferences(self):
        self.setContext(util.getHomeDirectory())
        self.setPVSLocation(os.path.join(util.getHomeDirectory(), constants.PVS_L))
        self.contextPreferences[constants.OPENFILES] = []
        self.setContextPreferencesRestoredAutomatically(True)
        self.globalPreferences[Preferences.VISIBLEPLUGINS] = sets.Set([constants.FILESTREE, constants.PROOFTREE, constants.CONSOLEPLUGIN])
        self.setVisibleToolbar(True)

    def loadDefaultContextPreferences(self):
        self.contextPreferences[constants.OPENFILES] = []

    def getValue(self, key, default=None):
        if key in self.contextPreferences:
            log.info("'%s' was found in context preference. Value: %s", key, self.contextPreferences[key])
            return self.contextPreferences[key]
        elif key in self.globalPreferences:
            log.info("'%s' was found in global preference. Value: %s", key, self.globalPreferences[key])
            return self.globalPreferences[key]
        log.info("'%s' was not found in either preferences", key)
        return default
        
    def getGlobalPreferenceFilename(self):
        """return the full path to the global preference file"""
        f = os.path.join(util.getHomeDirectory(), Preferences.GLOBALPREFERENCEFILE)
        return f
    
    def getContextPreferenceFilename(self):
        """return the full path to the context preference file"""
        f = os.path.join(self.getContext(), Preferences.CONTEXTPREFERENCEFILE)
        return f
    
    def loadGlobalPreferences(self):
        if Preferences.IGNORE_GLOBALPREFERENCEFILE:
            loadDefault = True
        else:
            loadDefault = False
            try:
                filename = self.getGlobalPreferenceFilename()
                output = open(filename, 'r')
                self.globalPreferences = pickle.load(output)
                log.info("Loaded global preferences: %s", self.globalPreferences)
                output.close()
            except:
                log.error("Error loading the global preference file...")
                loadDefault = True
        if loadDefault:
            log.info(" Loading the default global preference...")
            self.loadDefaultGlobalPreferences()

    def saveGlobalPreferences(self):
        filename = self.getGlobalPreferenceFilename()
        output = open(filename, 'wt')
        pickle.dump(self.globalPreferences, output)
        log.info("Saved global preferences: %s", self.globalPreferences)
        output.close()
    
    def loadContextPreferences(self):
        if Preferences.IGNORE_CONTEXTPREFERENCEFILE:
            loadDefault = True
        else:
            loadDefault = False
            try:
                filename = self.getContextPreferenceFilename()
                output = open(filename, 'r')
                self.contextPreferences = pickle.load(output)
                log.info("Loaded context preferences: %s", self.contextPreferences)
                output.close()
            except:
                log.error("Error loading the context preference file.")
                loadDefault = True
        if loadDefault:
            log.info(" Loading the default context preference...")
            self.loadDefaultContextPreferences()
        

    def saveContextPreferences(self):
        filename = self.getContextPreferenceFilename()
        output = open(filename, 'wt')
        self.contextPreferences[constants.OPENFILES] = util.getOpenFilesNames()
        pickle.dump(self.contextPreferences, output)
        log.info("Saved context preferences: %s", self.contextPreferences)
        output.close()
    
    # Global Settings:
    
    def getPVSLocation(self):
        """return the path to the pvs directory"""
        return self.getValue(Preferences.PVSLOCATION, util.getHomeDirectory())
    
    def setPVSLocation(self, location):
        self.globalPreferences[Preferences.PVSLOCATION] = util.normalizePath(location)
    
    def getContextPreferencesRestoredAutomatically(self):
        """return true only if the context preference is to be loaded automatically"""
        return self.getValue(Preferences.RESTORECONTEXT, False)
    
    def setContextPreferencesRestoredAutomatically(self, value):
        self.globalPreferences[Preferences.RESTORECONTEXT] = value
    
    def getContext(self):
        """return the last active context"""
        return self.getValue(Preferences.CONTEXT, util.getHomeDirectory())
    
    def setContext(self, context):
        newContext = util.normalizePath(context)
        self.globalPreferences[Preferences.CONTEXT] = newContext
        self.loadContextPreferences()
    
    def shouldPluginBeVisible(self, toolname):
        """return true if the tool with the given name should be visible"""
        return toolname in self.globalPreferences[Preferences.VISIBLEPLUGINS]
    
    def setPluginVisibility(self, toolname, visible):
        if visible:
            self.globalPreferences[Preferences.VISIBLEPLUGINS].add(toolname)
        else:
            if toolname in self.globalPreferences[Preferences.VISIBLEPLUGINS]:
                self.globalPreferences[Preferences.VISIBLEPLUGINS].remove(toolname)
        
    def getVisibleToolbar(self):
        """return true only if the toolbar is to be visible"""
        return self.getValue(constants.TOOLBAR, True)
    
    def setVisibleToolbar(self, value):
        """set the visibility of the toolbar"""
        self.globalPreferences[constants.TOOLBAR] = value
        
    # Local Settings with respect to a context:

    def listOfOpenFiles(self):
        return self.getValue(constants.OPENFILES, [])
