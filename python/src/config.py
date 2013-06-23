from ui.plugin import PluginManager
from ui.plg.ft import FilesTreePlugin
from ui.plg.pt import ProofTreePlugin
from ui.plg.console import ConsolePlugin
import constants


# Global Settings for the editor:

EDITOR_SIZE = (1100, 800)
EDITOR_MINIMUM_SIZE = (500, 300)

# Plugin Definitions:
FILES_TREE_PLUGIN = {PluginManager.CLASS: FilesTreePlugin, PluginManager.NAME: constants.FILESTREE, PluginManager.SIZE: (200, 300), PluginManager.LOCATION: PluginManager.LEFT}
PROOF_TREE_PLUGIN = {PluginManager.CLASS: ProofTreePlugin, PluginManager.NAME: constants.PROOFTREE, PluginManager.SIZE: (200, 300), PluginManager.LOCATION: PluginManager.RIGHT}
CONSOLE_PLUGIN    = {PluginManager.CLASS: ConsolePlugin, PluginManager.NAME: constants.CONSOLEPLUGIN, PluginManager.SIZE: (EDITOR_SIZE[0], 150), PluginManager.LOCATION: PluginManager.BOTTOM}

PLUGIN_DEFINITIONS = [FILES_TREE_PLUGIN, PROOF_TREE_PLUGIN, CONSOLE_PLUGIN]