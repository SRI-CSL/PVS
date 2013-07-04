from ui.plugin import PluginManager
import ui.plg.ft
import ui.plg.pt
import ui.plg.console
import constants


# Global Settings for the editor:

PVSURL = "http://localhost:22334"
EDITORHOST = "localhost"
EDITORPORT = 22335

EDITOR_SIZE = (900, 600)
EDITOR_MINIMUM_SIZE = (500, 300)

# Plugin Definitions:
FILES_TREE_PLUGIN = {PluginManager.CLASS: ui.plg.ft.FilesTreePlugin, PluginManager.NAME: constants.FILESTREE, PluginManager.SIZE: (200, 300), PluginManager.LOCATION: PluginManager.LEFT}
PROOF_TREE_PLUGIN = {PluginManager.CLASS: ui.plg.pt.ProofTreePlugin, PluginManager.NAME: constants.PROOFTREE, PluginManager.SIZE: (200, 300), PluginManager.LOCATION: PluginManager.RIGHT}
CONSOLE_PLUGIN    = {PluginManager.CLASS: ui.plg.console.ConsolePlugin, PluginManager.NAME: constants.CONSOLEPLUGIN, PluginManager.SIZE: (EDITOR_SIZE[0], 150), PluginManager.LOCATION: PluginManager.BOTTOM}

PLUGIN_DEFINITIONS = [FILES_TREE_PLUGIN, PROOF_TREE_PLUGIN, CONSOLE_PLUGIN]