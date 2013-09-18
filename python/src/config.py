from ui.plugin import PluginManager
import ui.plg.ft
import ui.plg.pt
import ui.plg.pm
import ui.plg.console
import constants
import ConfigParser
import logging
import os.path

class PVSIDEConfiguration:
    __shared_state = {}
    
    def __init__(self):
        self.__dict__ = self.__shared_state
        if not "pvsURL" in self.__dict__:
            self.pvsURL = "http://localhost:22334"
            self.ideURL = "http://localhost:22335/RPC2"
            self.ideSize = (900, 600)
            self.ideMinumumSize = (500, 300)
            self.proverCommands = { \
                "----": "", \
                "grind": "(grind)\n", \
                "assert": "(assert)\n", \
                "skosimp": "(skosimp)\n", \
                "expand": "(expand <formula>)",          
            }
            self.imageFolderName = "images"
            self.pluginDefinitions = []
            
    def initialize(self, applicationFolder):
        self.applicationFolder = applicationFolder
        self.imageFolderPath = os.path.join(applicationFolder, self.imageFolderName)    

        configFile = os.path.join(applicationFolder, "src/pvside.cfg")
        if not os.path.exists(configFile):
            logging.warn("Cannot find the config file. Using the default configuration")
            return
        config = ConfigParser.ConfigParser()
        config.read(configFile)
        logging.debug("Configuring General Values")
        
        self.pvsURL = config.get(constants.GENERAL, "pvs_url", 0)
        self.ideURL = config.get(constants.GENERAL, "ide_url", 0)
        self.ideSize = self._string2Tuple(config.get(constants.GENERAL, "ide_size", 0), True)
        self.ideMinumumSize = self._string2Tuple(config.get(constants.GENERAL, "ide_minimum_size", 0), True)
        logging.debug("Configuring the Plugins")
        for section in config.sections():
            if section.startswith("Plugin_"):
                pluginDef = {}
                pluginDef[PluginManager.LOCATION] = PluginManager.FLOAT
                pluginDef[PluginManager.SIZE] = (200, 300)
                for option in config.options(section):
                    pluginDef[option] = config.get(section, option)                
                pluginDef[PluginManager.SIZE] = self._string2Tuple(pluginDef[PluginManager.SIZE], True)
                if PluginManager.VISIBLE in pluginDef:
                    pluginDef[PluginManager.VISIBLE] = self._string2Tuple(pluginDef[PluginManager.VISIBLE])
                self.pluginDefinitions.append(pluginDef)
        
        logging.debug("Configuring the proof commands")
        proverCommandSection = "Prover_Commands"
        self.proverCommands = {}
        self.proverCommands["----"] = ""
        for command in config.options(proverCommandSection):
            fullCommand = config.get(proverCommandSection, command)
            fullCommand = fullCommand.replace("\\n", "\n")
            self.proverCommands[command] = fullCommand
        logging.info("Configuration Successful")
                
        
            

    def _string2Tuple(self, s, convert=False):
        s = s.strip("()")
        if convert:
            return tuple(int(num) for num in s.split(","))
        return tuple(item.strip() for item in s.split(","))

