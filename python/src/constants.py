
# This File contains all the global constant strings used in the system

import logging

IMAGE_FOLDER_NAME = "images"
IMAGE_FOLDER_PATH = None
APPLICATION_FOLDER = None
LOGGER_LEVEL = logging.DEBUG

PVS_U = "PVS"
PVS_L = "pvs"
PVS_EXTENSION = ".pvs"
FULLNAME = "fullname"
ID_U = "ID"
ID_L = "id"

LABEL_FILE = "File"
LABEL_EDIT = "Edit"
LABEL_VIEW = "View"
LABEL_NEW = "New"
LABEL_OPEN = "Open"
LABEL_SAVE = "Save"
LABEL_SAVEAS = "Save As"
LABEL_SAVEALL = "Save All"
LABEL_QUIT = "Quit"
LABEL_UNDO = "Undo"
LABEL_REDO = "Redo"
LABEL_SELECTALL = "Select All"
LABEL_COPY = "Copy"
LABEL_CUT = "Cut"
LABEL_PASTE = "Paste"
LABEL_FIND = "Find"
LABEL_REPLACE = "Replace"
LABEL_STARTPVS = "Start " + PVS_U
LABEL_STOPPVS = "Stop " + PVS_U
LABEL_TYPECHECK = "Typecheck"
LABEL_CLOSEFILE = "Close File"
LABEL_PROVE_FORMULA = "Prove This"

DOTDOTDOT = "..."
TAB = "\t"
CONTROL = "Ctrl"
SHIFT = "Shift"
EMPTY_STRING = ""
NEWLINE = "\n"
LOGGERNAME = "PVSEditor"
TOOLBAR = "Toolbar"

#Frame Titles::
MAINFRAME = "PVS Editor"
CONSOLEPLUGIN = "PVS Console"
FILESTREE = "Files Tree"
PROOFTREE = "Proof Tree"

PVS_MODE_OFF = "off"
PVS_MODE_LISP = "lisp"
PVS_MODE_PROVER = "prover"
PVS_MODE_UNKNOWN = "unknown"

# PUBSUB Messages:
PUB_UPDATETOOLBAR = "toolbar.Update" #TODO: This is not used anywhere
PUB_UPDATEMENUBAR = "menubar.Update"
PUB_SHOWPLUGIN = "menubar.showPlugin"
PUB_SHOWTOOLBAR = "toolbar.showToolbar"
PUB_NUMBEROFOPENFILESCHANGED = "NumberOfOpenFilesChanged"
PUB_ADDITEMTOVIEWMENU = "menubar.AddToViewMenu"
PUB_UPDATESTATUSBAR = "statusbar.Update"
PUB_UPDATEPVSMODE = "pvs.mode.Update"
PUB_UPDATEPVSCONTEXT = "pvs.context.Update"
PUB_PVSMESSAGERECEIVED = "pvs.messageReceived"
PUB_CONSOLECLEARIN = "console.ClearIn"
PUB_CONSOLEINEDITABLE = "console.SetInEditable"
PUB_CONSOLEINITIALIZE = "console.initialize"
PUB_CONSOLEWRITELINE = "console.writeLine"
PUB_CONSOLEWRITEPROMPT = "console.writePrompt"
PUB_TOOLSTATUSUPDATE = "ToolManager.ToolStatusUpdate"
PUB_SAVEFILE = "File.Save"
PUB_FILESAVED = "pvs.file.Saved"
PUB_FILETYPECHECKED = "pvs.file.typechecked"
PUB_ADDFILE = "AddFile"
PUB_CLOSEALLBUFFERS = "CloseAllBuffers"
PUB_CLOSEALLFILES = "CloseAllFiles"
PUB_CLOSEFILE = "File.Close"

GROUP_TOOLS = "tools"

THEORIES = "theories"
DECLARATIONS = "declarations"
KIND = "kind"
FORMULA_DECLARATION = "formulaDecl"
ERROR = "Error"
WARNING = "Warning"
MESSAGE = "Message"
QUESTION = "Question"
FILE = "File"
THEORY = "Theory"
FORMULA = "Formula"
ROOT = "Root"
INPUT_LOGGER = "-logger"
LOG_LEVEL_DEBUG = "debug"
LOG_LEVEL_OFF = "off"
OPENFILES = "Open Files"
PVSMODE = "PVS Mode"


PVS_KEYWORDS = u'and conjecture fact let table andthen containing false library then array conversion forall macro theorem assuming conversion+ formula measure theory assumption conversion- from nonempty_type true auto_rewrite corollary function not type auto_rewrite+ datatype has_type o type+ auto_rewrite- else if obligation var axiom elsif iff of when begin end implies or where but endassuming importing orelse with by endcases in postulate xor cases endcond inductive proposition challenge endif judgement recursive claim endtable lambda sublemma closure exists law subtypes cond exporting lemma subtype_of'

#('and', 'conjecture', 'fact', 'let', 'table', 'andthen', 'containing', 'false', 'library', 'then', 'array', 'conversion', 'forall', 'macro', 'theorem', 'assuming', 'conversion+', 'formula', 'measure', 'theory', 'assumption', 'conversion-', 'from', 'nonempty_type', 'true', 'auto_rewrite', 'corollary', 'function', 'not', 'type', 'auto_rewrite+', 'datatype', 'has_type', 'o', 'type+', 'auto_rewrite-', 'else', 'if', 'obligation', 'var', 'axiom', 'elsif', 'iff', 'of', 'when', 'begin', 'end', 'implies', 'or', 'where', 'but', 'endassuming', 'importing', 'orelse', 'with', 'by', 'endcases', 'in', 'postulate', 'xor', 'cases', 'endcond', 'inductive', 'proposition', 'challenge', 'endif', 'judgement', 'recursive', 'claim', 'endtable', 'lambda', 'sublemma', 'closure', 'exists', 'law', 'subtypes', 'cond', 'exporting', 'lemma', 'subtype_of', )

PVS_OPERATORS = ("#", "*", ":)", "=>", "\\/", "/=", "|=", "##", "**", "::", ">", "|>", "#)", "+", ":=", ">=", "]|", "|[", "#]", "++", ";", ">>", "^", "|]", "%", ",", "<", ">>=", "^^", "||", "&", "-", "<<", "@", "`", "|}", "&&", "->", "<<=", "@@", ".", "<=", "{|", "~", "(#", "/", "<=>", "[#", "{||}", "(:", "//", "<>", "[]", "|","(|", "<|", "[|", "|)","(||)", "/\\", "=", "[||]", "|-", ":", "==", "\\", "|->",)

