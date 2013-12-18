import wx
import util
import logging
from constants import *
import wx.lib.agw.aui as aui
from wx.lib.pubsub import setupkwargs, pub
import pvscomm
import importlib

class PluginManager:
    """A class to manage all the tool frames, create, and show/hide them"""
    NAME = "name"
    MODULE = "module"
    CLASS = "class"
    SIZE = "size"
    LOCATION = "location"
    VISIBLE = "visible"
    TOP = "top"
    BOTTOM = "bottom"
    LEFT = "left"
    RIGHT = "right"
    FLOAT = "float"
    CENTER = "center"
    
    __shared_state = {}    
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "pluginsDefinitions" in self.__dict__:
            self.pluginsDefinitions = []
            pub.subscribe(self.onPVSModeUpdated, PUB_UPDATEPVSMODE)
            
    def initializePlugins(self, pluginsDefinitions):
        import preference
        preference = preference.Preferences()
        self.pluginsDefinitions = pluginsDefinitions
        for pluginDefinitions in pluginsDefinitions:
            self.create(pluginDefinitions)
            
    def onPVSModeUpdated(self, pvsMode = PVS_MODE_OFF):
        for plugin in self.pluginsDefinitions:
            if PluginManager.VISIBLE in plugin:
                visibility = plugin[PluginManager.VISIBLE]
                visible = pvsMode in visibility
            else:
                visible=True
            self.showPlugin(plugin[PluginManager.NAME], visible)
            pub.sendMessage(PUB_SHOWPLUGIN, name=plugin[PluginManager.NAME], value=visible)
            
    def create(self, pluginDefinition):
        name = pluginDefinition[PluginManager.NAME]
        module = importlib.import_module(pluginDefinition[PluginManager.MODULE])
        panelClass = getattr(module, pluginDefinition[PluginManager.CLASS])
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
        elif location == PluginManager.CENTER:
            paneInfo = paneInfo.Center()
        else:
            logging.error("Unknown Location: %s", location)
            paneInfo = paneInfo.Float()
        auiManager.AddPane(panel, paneInfo)
        if PluginManager.VISIBLE in pluginDefinition:
            self.showPlugin(name, pvscomm.PVSCommandManager().pvsMode in pluginDefinition[PluginManager.VISIBLE])
        else:
            self.showPlugin(name, False)            
        auiManager.Update()
        pub.sendMessage(PUB_ADDITEMTOVIEWMENU, name=name, callBackFunction=(lambda ce: self.togglePluginVisibility(name)))
        return None
            
    def togglePluginVisibility(self, name):
        logging.debug("Name: %s", name)
        paneInfo = self.getPlugin(name)
        self.showPlugin(name, not paneInfo.IsShown())
        
    def showPlugin(self, name, visible):
        logging.debug("Name: %s Visibility: %s", name, visible)
        paneInfo = self.getPlugin(name)
        paneInfo.Show(visible)
        util.auiManager().Update()
        
    def getPlugin(self, name):
        paneInfo = util.auiManager().GetPane(name)
        if not paneInfo.IsOk():
            raise util.PVSIDEException("There is no panel with the name '%s'"%name)
        return paneInfo
        
    def destroy(self, name):
        plugin = self.getPlugin(name)
        plugin.Destroy()
        util.auiManager().Update()
        
    def shouldPluginBeVisible(self, name, pvsMode):
        for definition in self.pluginsDefinitions:
            if definition[PluginManager.NAME] == name:
                if PluginManager.VISIBLE in definition:
                    return pvsMode in definition[PluginManager.VISIBLE]
                else:
                    return True
        
        raise util.PVSIDEException("There is no plugin with the name '%s'"%name)


class PluginPanel(wx.Panel):
    
    def __init__(self, parent, pluginDefinition):
        wx.Panel.__init__(self, parent)
        self.definition = pluginDefinition
        self.SetName(pluginDefinition[PluginManager.NAME])



