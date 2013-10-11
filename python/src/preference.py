import pickle
import util
import os.path, os
import constants
import sets
import logging
from wx.lib.pubsub import setupkwargs, pub 

class Preferences:
    """This class manages all the user preferences"""
    __shared_state = {}
    
    PREFERENCEFILE = ".pvseditor"
    RECENTCONTEXTS = "RecentContexts"
    RECENTFILES = "RecentFiles"
    OPENFILES = "OpenFiles"
    IGNORE_PREFERENCEFILE  = False # if True, load the default preference
    NUMBEROFRECENTCONTEXTS = 10
    NUMBEROFRECENTFILES = 20
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "preferences" in self.__dict__:
            self.preferences = {}
            
    def clearAllPreferences(self):
        self.preferences[Preferences.OPENFILES] = sets.Set()
        self.preferences[Preferences.RECENTCONTEXTS] = []
        self.preferences[Preferences.RECENTFILES] = []
        
    def getPreferenceFilename(self):
        """return the full path to the global preference file"""
        f = os.path.join(util.getHomeDirectory(), Preferences.PREFERENCEFILE)
        return f
    
    def loadDefaultPreferences(self):
        self.setRecentContext(util.getHomeDirectory())
        
    def getValue(self, key, default=None):
        if key in self.preferences:
            logging.info("'%s' was found in preference. Value: %s", key, self.preferences[key])
            return self.preferences[key]
        logging.info("'%s' was not found in preferences", key)
        return default
        
    def loadPreferences(self):
        self.clearAllPreferences()
        if Preferences.IGNORE_PREFERENCEFILE:
            loadDefault = True
        else:
            loadDefault = False
            try:
                filename = self.getPreferenceFilename()
                output = open(filename, 'r')
                self.preferences = pickle.load(output)
                logging.info("Loaded preferences: %s", self.preferences)
                output.close()
            except e:
                logging.error("Error loading the preference file...")
                loadDefault = True
        if loadDefault:
            logging.info(" Loading the default preference...")
            self.loadDefaultPreferences()

    def savePreferences(self):
        filename = self.getPreferenceFilename()
        if os.path.exists(filename):
            output = file(filename, "r+")
        else:
            output = file(filename, "w")
        pickle.dump(self.preferences, output)
        logging.info("Saved preferences: %s", self.preferences)
        output.close()
    
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
        self.preferences[Preferences.RECENTCONTEXTS] = recent
        
    def getRecentFiles(self):
        """return the recently opened files"""
        fullnames = self.getValue(Preferences.RECENTFILES, [])
        return fullnames
    
    def setRecentFile(self, fullname):
        self.removeFromRecentFiles(fullname)
        fullname = util.normalizePath(fullname)
        recent = self.getRecentFiles()
        recent.insert(0, fullname)
        if len(recent) > Preferences.NUMBEROFRECENTFILES:
            recent.pop()
        self.preferences[Preferences.RECENTFILES] = recent
        
    def removeFromRecentFiles(self, fullname):
        if Preferences.RECENTFILES in self.preferences:
            fullname = util.normalizePath(fullname)
            if fullname in self.preferences[Preferences.RECENTFILES]:
                self.preferences[Preferences.RECENTFILES].remove(fullname)
        
    def listOfOpenFiles(self):
        return self.getValue(Preferences.OPENFILES, [])
    
    def setListOfOpenFiles(self, fullnames):
        self.preferences[Preferences.OPENFILES] = sets.Set(fullnames)
    
        

    
