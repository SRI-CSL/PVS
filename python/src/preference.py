import pickle
import util
import os.path, os
import constants
import sets
import logging
from remgr import RichEditorManager

class Preferences:
    """This class manages all the user preferences"""
    __shared_state = {}
    
    GLOBALPREFERENCEFILE = ".pvseditorglobal"
    CONTEXTPREFERENCEFILE = ".pvseditor"
    RECENTCONTEXTS = "RecentContexts"
    PVSLOCATION = "PVSLocation"
    IGNORE_GLOBALPREFERENCEFILE  = False # if True, load the default global preference
    IGNORE_CONTEXTPREFERENCEFILE = False # if True, load the default context preference
    NUMBEROFRECENTCONTEXTS = 10
    
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
        self.setRecentContext(util.getHomeDirectory())
        self.setPVSLocation(os.path.join(util.getHomeDirectory(), constants.PVS_L))
        self.contextPreferences[constants.OPENFILES] = []
        self.setVisibleToolbar(True)

    def loadDefaultContextPreferences(self):
        self.contextPreferences[constants.OPENFILES] = []

    def getValue(self, key, default=None):
        if key in self.contextPreferences:
            logging.info("'%s' was found in context preference. Value: %s", key, self.contextPreferences[key])
            return self.contextPreferences[key]
        elif key in self.globalPreferences:
            logging.info("'%s' was found in global preference. Value: %s", key, self.globalPreferences[key])
            return self.globalPreferences[key]
        logging.info("'%s' was not found in either preferences", key)
        return default
        
    def getGlobalPreferenceFilename(self):
        """return the full path to the global preference file"""
        f = os.path.join(util.getHomeDirectory(), Preferences.GLOBALPREFERENCEFILE)
        return f
    
    def getContextPreferenceFilename(self):
        """return the full path to the context preference file"""
        f = os.path.join(self.getRecentContexts()[0], Preferences.CONTEXTPREFERENCEFILE)
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
                logging.info("Loaded global preferences: %s", self.globalPreferences)
                output.close()
            except:
                logging.error("Error loading the global preference file...")
                loadDefault = True
        if loadDefault:
            logging.info(" Loading the default global preference...")
            self.loadDefaultGlobalPreferences()

    def saveGlobalPreferences(self):
        filename = self.getGlobalPreferenceFilename()
        if os.path.exists(filename):
            output = file(filename, "r+")
        else:
            output = file(filename, "w")
        pickle.dump(self.globalPreferences, output)
        logging.info("Saved global preferences: %s", self.globalPreferences)
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
                logging.info("Loaded context preferences: %s", self.contextPreferences)
                output.close()
            except:
                logging.error("Error loading the context preference file.")
                loadDefault = True
        if loadDefault:
            logging.info(" Loading the default context preference...")
            self.loadDefaultContextPreferences()
        

    def saveContextPreferences(self):
        filename = self.getContextPreferenceFilename()
        output = open(filename, 'wt+')
        self.contextPreferences[constants.OPENFILES] = RichEditorManager().getOpenFileNames()
        pickle.dump(self.contextPreferences, output)
        logging.info("Saved context preferences: %s", self.contextPreferences)
        output.close()
    
    # Global Settings:
    
    def getPVSLocation(self):
        """return the path to the pvs directory"""
        return self.getValue(Preferences.PVSLOCATION, util.getHomeDirectory())
    
    def setPVSLocation(self, location):
        self.globalPreferences[Preferences.PVSLOCATION] = util.normalizePath(location)
    
    def getRecentContexts(self):
        """return the last active context"""
        contexts = self.getValue(Preferences.RECENTCONTEXTS, [util.getHomeDirectory()])
        cxts = []
        for context in contexts:
            ncontext = util.normalizePath(context)
            if not ncontext in cxts:
                cxts.append(ncontext)
        return cxts
    
    def setRecentContext(self, context):
        newContext = util.normalizePath(context)
        recent = self.getRecentContexts()
        if newContext in recent:
            recent.remove(newContext)
        recent.insert(0, newContext)
        if len(recent) > Preferences.NUMBEROFRECENTCONTEXTS:
            recent.pop()
        self.globalPreferences[Preferences.RECENTCONTEXTS] = recent
        self.loadContextPreferences()
            
    def getVisibleToolbar(self):
        """return true only if the toolbar is to be visible"""
        return self.getValue(constants.TOOLBAR, True)
    
    def setVisibleToolbar(self, value):
        """set the visibility of the toolbar"""
        self.globalPreferences[constants.TOOLBAR] = value
        
    # Local Settings with respect to a context:

    def listOfOpenFiles(self):
        return self.getValue(constants.OPENFILES, [])
