import wx
import util
from constants import *
from preference import Preferences
from wx.lib.pubsub import pub
import wx.lib.agw.aui as aui

log = util.getLogger(__name__)

class PluginManager:
    """A class to manage all the tool frames, create, and show/hide them"""
    NAME = "name"
    CLASS = "class"
    SIZE = "size"
    LOCATION = "location"
    TOP = "top"
    BOTTOM = "bottom"
    LEFT = "left"
    RIGHT = "right"
    FLOAT = "float"
    
    __shared_state = {}    
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "pluginsDefinitions" in self.__dict__:
            self.plugins = {}
            self.pluginsDefinitions = []
            
    def initializePlugins(self, pluginsDefinitions):
        preference = Preferences()
        self.pluginsDefinitions = pluginsDefinitions
        for pluginDefinitions in pluginsDefinitions:
            self.create(pluginDefinitions)
            
    def create(self, pluginDefinition):
        name = pluginDefinition[PluginManager.NAME]
        panelClass = pluginDefinition[PluginManager.CLASS]
        size = pluginDefinition[PluginManager.SIZE]
        panel = panelClass(util.getMainFrame(), pluginDefinition)
        location = pluginDefinition[PluginManager.LOCATION] if PluginManager.LOCATION in pluginDefinition else PluginManager.FLOAT
        auiManager = util.auiManager()
        paneInfo = aui.AuiPaneInfo()
        paneInfo = paneInfo.Caption(name).Name(name).BestSize(size)
        if location == PluginManager.TOP:
            paneInfo = paneInfo.Top()
        elif location == PluginManager.BOTTOM:
            paneInfo = paneInfo.Bottom()
        elif location == PluginManager.LEFT:
            paneInfo = paneInfo.Left()
        elif location == PluginManager.RIGHT:
            paneInfo = paneInfo.Right()
        elif location == PluginManager.FLOAT:
            paneInfo = paneInfo.Float()
        else:
            log.error("Unknown Location: %s", location)
            paneInfo = paneInfo.Float()
        auiManager.AddPane(panel, paneInfo)
        auiManager.Update()
        pub.sendMessage(PUB_ADDITEMTOVIEWMENU, name=name, callBackFunction=(lambda ce: self.togglePluginVisibility(name)))
        return None
            
    def togglePluginVisibility(self, name):
        print "togglePluginVisibility was called for %s"%(name, )
        paneInfo = self.getPlugin(name)
        newVisibility = not paneInfo.IsShown()
        paneInfo.Show(newVisibility)
        util.auiManager().Update()
        
    def getPlugin(self, name):
        paneInfo = util.auiManager().GetPane(name)
        if not paneInfo.IsOk():
            raise util.PVSIDEException("There is no panel with the name '%s'"%name)
        return paneInfo
        
    def destroy(self, name):
        plugin = self.getPlugin(name)
        plugin.Destroy()
        del self.plugins[name]
        self.toolsDir[name] = [info[0], None, False]


class PluginPanel(wx.Panel):
    def __init__(self, parent, pluginDefinition):
        wx.Panel.__init__(self, parent)
        wx.definition = pluginDefinition
        self.SetName(pluginDefinition["name"])
