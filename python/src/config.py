from ui.plugin import PluginManager
import ui.plg.ft
import ui.plg.pt
import ui.plg.pm
import ui.plg.console
import constants

# Global Settings for the editor:

PVSURL = "http://localhost:22334"
EDITORHOST = "localhost"
EDITORPORT = 22335

EDITOR_SIZE = (900, 600)
EDITOR_MINIMUM_SIZE = (500, 300)

# Plugin Definitions:
FILES_TREE_PLUGIN = {PluginManager.CLASS: ui.plg.ft.FilesTreePlugin, \
                    PluginManager.NAME: "Files Tree", \
                    PluginManager.SIZE: (200, 300), \
                    PluginManager.LOCATION: PluginManager.LEFT, \
                    PluginManager.VISIBLE: [constants.PVS_MODE_UNKNOWN, constants.PVS_MODE_OFF, constants.PVS_MODE_LISP]}
CONSOLE_PLUGIN    = {PluginManager.CLASS: ui.plg.console.ConsolePlugin, \
                     PluginManager.NAME: "Console", \
                     PluginManager.SIZE: (EDITOR_SIZE[0], 150), \
                     PluginManager.LOCATION: PluginManager.BOTTOM, \
                     PluginManager.VISIBLE: [constants.PVS_MODE_LISP, constants.PVS_MODE_UNKNOWN]}
PROOF_MANAGER_PLUGIN = {PluginManager.CLASS: ui.plg.pm.ProofManagerPlugin, \
                        PluginManager.NAME: "Proof Manager", \
                        PluginManager.SIZE: (400, 400), \
                        PluginManager.LOCATION: PluginManager.RIGHT, \
                        PluginManager.VISIBLE: [constants.PVS_MODE_PROVER]}
# PROOF_TREE_PLUGIN = {PluginManager.CLASS: ui.plg.pt.ProofTreePlugin, PluginManager.NAME: "Proof Tree", PluginManager.SIZE: (200, 300), PluginManager.LOCATION: PluginManager.RIGHT}


PLUGIN_DEFINITIONS = [FILES_TREE_PLUGIN, PROOF_MANAGER_PLUGIN, CONSOLE_PLUGIN]