# This File contains all the global constant strings used in the system

PVS_U = "PVS"
PVS_L = "pvs"
PVS_EXTENSION = ".pvs"
FULLNAME = "fullname"
ID_U = "ID"
ID_L = "id"
LTHEORY = "theory"
LFORMULA = "formula"
LROOT = "root"
LFILE = "file"
LTYPE = "type"
LKIND = "kind"
LPLACE = "place"
LCONTEXT = "context"
LINACTIVECONTEXT = "inactive-context"
DECLS = "decls"
DECL = "decl"
DECLFILE = "decl-file"
DECLPLACE = "decl-place"
JSONLOG = "json-log"
COMMENTARYLOG = "commentary-log"

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
GENERAL = "General"
EDITOR = "Editor"

#Frame Titles:
MAINFRAME = "PVS GUI"

PVS_MODE_OFF = "off"
PVS_MODE_LISP = "lisp"
PVS_MODE_PROVER = "prover"
PVS_MODE_UNKNOWN = "unknown"

# PUBSUB Messages:
PUB_UPDATEMENUBAR = "menubar.Update"
PUB_SHOWPLUGIN = "menubar.showPlugin"
PUB_NUMBEROFOPENFILESCHANGED = "NumberOfOpenFilesChanged"
PUB_ADDITEMTOVIEWMENU = "menubar.AddToViewMenu"
PUB_UPDATESTATUSBAR = "statusbar.Update"
PUB_UPDATEPVSMODE = "pvs.mode.Update"
PUB_UPDATEPVSCONTEXT = "pvs.context.Update"
PUB_PVSMESSAGERECEIVED = "pvs.messageReceived"
PUB_CONSOLECLEARIN = "console.ClearIn"
PUB_CONSOLEINITIALIZE = "console.initialize"
PUB_CONSOLEWRITELINE = "console.writeLine"
PUB_CONSOLEWRITEPROMPT = "console.writePrompt"
PUB_TOOLSTATUSUPDATE = "ToolManager.ToolStatusUpdate"
PUB_PROOFINFORMATIONRECEIVED = "Proof.Information.Received"
PUB_SAVEFILE = "File.Save"
PUB_FILESAVED = "pvs.file.Saved"
PUB_FILETYPECHECKED = "pvs.file.typechecked"
PUB_ADDFILE = "AddFile"
PUB_CLOSEFILE = "File.Close"
PUB_ERRORLOCATION = "File.Error.Location"
PUB_REMOVEANNOTATIONS = "File.Remove.Annotations"
PUB_PREPARERECENTFILESMENU = "Prepare.Recent.Files.Menu"
PUB_NAMESINFOUPDATE = "Names.Info.Updated"
PUB_APPENDLOG = "Append.To.Log.Dialog"

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

PVS_OPERATORS = [u'(', u')', u'-', u'+', u'*', u'/', u':=', u'::=', u'|->', u';', u',', u'=', u'/=', \
                 u'<', u'<=', u'>', u'>=', u'{', u'}', u'[', u']', u'.', u'&', u'<=>', u'->', u':', \
                 u'=>', u'[#', u'#]', u'(#', u'#)', u'(:', u':)', u'{:', u':}', u'{{', u'}}', u'|', \
                 u'^', u'/\\', u'\\/', u'<>', u'~', u'==', u'++', u'**', u'//', u'^^', u'<<', u'>>', \
                 u'::', u'#', u'@', u'@@', u'##', u'|[', u']|', u'!', u'<<=', u'>>=', u'`', u"'", u'..', \
                 u'||', u'|-', u'|=', u'<|', u'|>', u':->', u'\xa7', u'[|', u'|]', u'[||]', u'(|', u'|)', \
                 u'(||)', u'{|', u'|}', u'{||}', u'\u2329', u'\u232a', u'\u2329\u232a', u'\u301a', \
                 u'\u301b', u'\u301a\u301b', u'\xab', u'\xbb', u'\xab\xbb', u'\u300a', u'\u300b', \
                 u'\u300a\u300b', u'\u2308', u'\u2309', u'\u2308\u2309', u'\u230a', u'\u230b', \
                 u'\u230a\u230b', u'\u231c', u'\u231d', u'\u231c\u231d', u'\u231e', u'\u231f', \
                 u'\u231e\u231f', u'\u25a1', u'\u25c7', u'\xac', u'\u25ef', u'\u221a', u'\u2218', \
                 u'\u2228', u'\u2227', u'\u2295', u'\u2298', u'\u2297', u'\u2296', u'\u2299', u'\u229b', \
                 u'\u2a01', u'\u2a02', u'\u2a00', u'\u22a2', u'\u22a8', u'\xb1', u'\u2213', u'\u2214', \
                 u'\xd7', u'\xf7', u'\u229e', u'\u229f', u'\u22a0', u'\u2241', u'\u223c', u'\u2243', \
                 u'\u2245', u'\u2247', u'\u2248', u'\u2249', u'\u224d', u'\u224e', u'\u224f', u'\u2250', \
                 u'\u2257', u'\u2259', u'\u2261', u'\u22c8', u'\u2264', u'\u2265', u'\u2266', u'\u2267', \
                 u'\u2268', u'\u2269', u'\u226a', u'\u226b', u'\u226e', u'\u226f', u'\u2270', u'\u2271', \
                 u'\u227a', u'\u227b', u'\u25c1', u'\u25b7', u'\u2208', u'\u2209', u'\u220b', u'\u2229', \
                 u'\u222a', u'\u2282', u'\u2283', u'\u2284', u'\u2285', u'\u2286', u'\u2287', u'\u228e', \
                 u'\u228a', u'\u228b', u'\u228f', u'\u2290', u'\u2293', u'\u2294', u'\u22c0', u'\u22c1', \
                 u'\u22c2', u'\u22c3', u'\u2022', u'\u2190', u'\u2191', u'\u2192', u'\u2193', u'\u219d', \
                 u'\u21a6', u'\u21d0', u'\u21d2', u'\u21d1', u'\u21d3', u'\u21d4', u'\u2207', u'\u22a3', \
                 u'\u22a5', u'\u22a9', u'\u25ef', u'\u2605', u'\u2720']

MATH_SYMBOLS = {"\FORALL": u"\u2200", "\EXISTS": u"\u2203"}

PVS_GITHUB_REPOSITORY = "https://raw.github.com/samowre/PVS/master"