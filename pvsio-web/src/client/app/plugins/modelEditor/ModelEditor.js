/**
 *
 * @author Patrick Oladimeji
 * @date 11/21/13 15:03:48 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";
    var CodeMirror          = require("cm/lib/codemirror"),
        PVSioWebClient      = require("PVSioWebClient"),
        d3                  = require("d3/d3"),
        ProjectManager		= require("project/ProjectManager"),
        sourceCodeTemplate	= require("text!pvsioweb/forms/templates/sourceCodeEditorPanel.handlebars"),
        Logger              = require("util/Logger"),
        NotificationManager = require("project/NotificationManager"),
        Notification        = require("pvsioweb/forms/displayNotification"),
        WSManager           = require("websockets/pvs/WSManager"),
        FileSystem          = require("filesystem/FileSystem");
//        MIME                = require("util/MIME");
    var instance,
        fs;
    var currentProject,
        projectManager,
        editor,
        editorContainer,
        pvsioWebClient;
    var undoHistory;

    require("cm/addon/fold/foldcode");
    require("cm/addon/fold/foldgutter");
    require("cm/addon/fold/indentFold");
    require("cm/addon/hint/show-hint");
    require("cm/addon/hint/pvs-hint");
    require("cm/addon/edit/closebrackets");
    require("cm/addon/edit/matchbrackets");
    require("cm/addon/selection/active-line");
    require("cm/addon/display/placeholder");
    require("cm/addon/dialog/dialog");
    require("cm/addon/search/searchcursor");
    require("cm/addon/search/search");
    require("cm/mode/pvs/pvs");
    require("cm/mode/mal/mal");

    function markDirty(file) {
        if (!file.isDirectory) {
            file.dirty(true); //update the selected project file content
            projectManager.project()._dirty(true);
        }
    }
    function _editorChangedHandler(editor) {
        var file = projectManager.getSelectedData();
        if (!file.isDirectory) {
            file.content = editor.getValue();
            var dirty = (file.content !== editor.getValue());
            if (dirty) {
                markDirty(file);
            }
        }
    }
    var saveHistory = function (descriptor) {
        if (descriptor && !descriptor.isDirectory && descriptor.path) {
            undoHistory.set(descriptor.path, editor.getHistory());
            console.log("Undo history saved for file " + descriptor.path);
            editor.clearHistory();
            return true;
        }
        return false;
    };

    var restoreHistory = function (descriptor) {
        if (descriptor && !descriptor.isDirectory && descriptor.path) {
            editor.clearHistory();
            var history = undoHistory.get(descriptor.path);
            if (history) {
                editor.setHistory(history);
                console.log("Undo history restored for file " + descriptor.path);
                return true;
            } else {
                editor.clearHistory();
            }
        }
        return false;
    };

    function onSelectedFileChanged(event) {
        function savePreviousFileHistory(previousData) {
            if (previousData) {
                if (projectManager.project().getDescriptor(previousData.path)) {
                    editor.saveHistory(
                        projectManager.project().getDescriptor(
                            previousData.path
                        )
                    );
                }
            }
            editor.clearHistory();
        }
        if (event.selectedItem) {
            if (event.selectedItem.isDirectory) {
                document.getElementById("editor").style.display = "inline-block";
                document.getElementById("imageViewer").style.display = "none";
                editor.off("change", _editorChangedHandler);
                // save undo history of previous file before proceeding with editor change
                savePreviousFileHistory(event.selectedItem.previousData);
                editor.setOption("mode", "txt");
                editor.setValue("<< Please select a file to view/edit its content within the model editor. >>");
                editor.setOption("readOnly", true);
                editor.markClean();
                editor.on("change", _editorChangedHandler);
            } else {
                var file = projectManager.project().getDescriptor(event.selectedItem.path);
                if (file) {
                    file.getContent().then(function (content) {
                        if (file.isImage()) {
                            document.getElementById("editor").style.display = "none";
                            document.getElementById("imageViewer").style.display = "inline-block";
                            d3.select("#imageViewer img")
                                .attr("src", file.content)
                                .attr("style", "max-height: 400px; max-width: 600px");
                        } else {
                            document.getElementById("editor").style.display = "inline-block";
                            document.getElementById("imageViewer").style.display = "none";
                            editor.off("change", _editorChangedHandler);
                            editor.setOption("mode", "txt");
                            // save undo history of previous file before proceeding with editor change
                            savePreviousFileHistory(event.selectedItem.previousData);
                            editor.setValue(content);
                            restoreHistory(event.selectedItem);
                            editor.setOption("readOnly", false);
                            editor.markClean();
                            if (file.isPVSFile()) {
                                editor.setOption("mode", "pvs");
                            }
                            editor.on("change", _editorChangedHandler);
                        }
                    }).catch(function (err) {
                        Logger.log(err);
                    });
                } else {
                    console.log("Warning: file descriptor not present in FileTree\n" +
                                JSON.stringify(event.selectedItem.path));
                }
            }
        }
    }


    function ModelEditor() {
        pvsioWebClient = PVSioWebClient.getInstance();
        projectManager = ProjectManager.getInstance();
        currentProject = projectManager.project();
        projectManager.addListener("SelectedFileChanged", function () {
            if (editor) {
                editor.refresh();
            }
        });
        undoHistory = d3.map();
        CodeMirror.defineExtension("saveHistory", saveHistory);
        CodeMirror.defineExtension("restoreHistory", restoreHistory);
        document.onkeydown = function (event) {
            // key 83 is 's'
            if (event.ctrlKey && (event.keyCode === 70 || event.which === 70)) {
                event.preventDefault();
                event.stopPropagation();
                if (document.getElementById("model-editor-search-input")) {
                    document.getElementById("model-editor-search-input").click();
                    console.log("Search...");
                }
            }
        };

        fs = new FileSystem();
    }

    ModelEditor.prototype.getName = function () {
        return "Model Editor";
    };
    /////These are the api methods that the prototype builder plugin exposes
    ModelEditor.prototype.getDependencies = function () { return []; };

    function bindListeners(projectManager) {
        d3.select("#btnImportFiles").on("click", function () {
            return new Promise(function (resolve, reject) {
                function writeFiles(files) {
                    var promises = [];
                    function getImportFolderName() {
                        var selectedData = projectManager.getSelectedData();
                        return (selectedData.isDirectory) ? selectedData.path
                                : selectedData.path.split("/").slice(0, -1).join("/");
                    }
                    var importFolder = getImportFolderName();
                    files.forEach(function (file) {
                        // FIXME: directories are discarded when using absolute paths. Can we do better?
                        file.path = importFolder + "/" + file.name;
                        promises.push(fs.writeFileDialog(file.path, file.content, { encoding: file.encoding }));
                    });
                    Promise.all(promises).then(function (res) {
                        resolve(res);
                    }).catch(function (err) { reject(err); });
                }
                if (PVSioWebClient.getInstance().serverOnLocalhost()) {
                    fs.readFileDialog({title: "Import files into Project"}).then(function (res) {
                        writeFiles(res);
                    }).catch(function (err) { reject(err); });
                } else {
                    fs.readLocalFileDialog().then(function (res) {
                        writeFiles(res);
                    }).catch(function (err) { reject(err); });
                }
            });
        });
        d3.select("#btnSaveFile").on("click", function () {
            var project = projectManager.project();
            if (project) {
                var descriptor = projectManager.getSelectedFile();
                if (descriptor) {
                    descriptor.content = instance.getEditor().doc.getValue();
                    descriptor.dirty(false);
                    projectManager.project().saveFiles([descriptor], { overWrite: true }).then(function (res) {
                        var notification = descriptor.name + " saved successfully!";
                        NotificationManager.show(notification);
                    }).catch(function (err) {
                        NotificationManager.error(err);
                    });
                }
            }
        });

        function reloadPVSio() {
            //compilation is emulated by restarting the pvsioweb process on the server
            var project = projectManager.project(), ws = WSManager.getWebSocket();
            if (project && project.mainPVSFile()) {
                ws.lastState("init(0)");
                // the main file can be in a subfolder: we need to pass information about directories!
                var mainFile = project.mainPVSFile().path.replace(project.name() + "/", "");
                ws.startPVSProcess({name: mainFile, projectName: project.name()}, function (err) {
                    //make projectManager bubble the process ready event
                    if (!err) {
                        projectManager.fire({type: "PVSProcessReady"});
                    } else {
                        projectManager.fire({type: "PVSProcessDisconnected", err: err});
                    }
                });
            }
        }
        //handle typecheck event
        //this function should be edited to only act on the selected file when multiple files are in use
        d3.select("#btnTypeCheck").on("click", function () {
            function typecheck(pvsFile) {
                var btn = d3.select("#btnTypeCheck").html("Compiling...").attr("disabled", true);
                var ws = WSManager.getWebSocket();
                var fp = pvsFile.path;
                ws.send({type: "typeCheck", path: fp},
                     function (err, res) {
                        btn.html("Compile").attr("disabled", null);
                        var msg = res.stdout;
                        if (!err) {
                            reloadPVSio();
                            msg = msg.substring(msg.indexOf("Proof summary"), msg.length);
                            Notification.create({
                                header: pvsFile.name + " compiled successfully! ",
                                notification: msg.split("\n")
                            }).on("ok", function (e, view) { view.remove(); });
                        } else {
                            var logFile = projectManager.project().name() + "/" + fp.substring(0, fp.length - 4) + ".log";
                            var header = "Compilation error";
                            ws.getFile(logFile, function (err, res) {
                                if (!err) {
                                    msg = res.content.substring(res.content.indexOf("Parsing "));
                                    msg = msg.replace("Parsing", "Error while parsing");
                                } else {
                                    msg = msg.substring(msg.indexOf("Writing output to file"));
                                    header += ", please check the PVS output file for details.";
                                }
                                Notification.create({
                                    header: header,
                                    notification: msg.split("\n")
                                }).on("ok", function (e, view) { view.remove(); });
                            });
                        }
                    });
            }

            // if the pvsFile is not specified, we compile the main file
            // note: this happens when a directory is selected
            var pvsFile = projectManager.getSelectedFile() || projectManager.project().mainPVSFile();
            if (!pvsFile) { return; }
            if (pvsFile.dirty()) {
                document.getElementById("btnSaveFile").click();
            }
            typecheck(pvsFile);
        });
        d3.select("#btnSetMainFile").on("click", function () {
            var pvsFile = projectManager.getSelectedFile(), project = projectManager.project();
            if (pvsFile) {
                var ws = WSManager.getWebSocket();
                ws.send({
                    type: "setMainFile",
                    projectName: project.name(),
                    name: pvsFile.name,
                    path: pvsFile.path
                }, function (err) {
                    //if there was no error update the main file else alert user
                    if (!err) {
                        // set main file
                        project.mainPVSFile(pvsFile);
                        // disable button
                        d3.select("#btnSetMainFile").attr("disabled", true);
                        var notification = pvsFile.path + " is now the Main file";
                        NotificationManager.show(notification);
                        // reload pvsio
                        reloadPVSio();
                    } else {
                        NotificationManager.err(err);
                    }
                });
            }
        });
    }

    /**
        @returns {Promise} a promise that resolves when the prototype builder has been initialised
    */
    ModelEditor.prototype.initialise = function () {
        editorContainer = pvsioWebClient.createCollapsiblePanel({
            headerText: "Model Editor",
            showContent: true,
            onClick: function () {
                editor.refresh();
            },
            owner: this.getName()
        });
        editorContainer.append("div").html(sourceCodeTemplate);

        // this enables autocompletion
        editor = new CodeMirror(d3.select("#editor").node(), {
            mode: "txt",
            lineNumbers: true,
            foldGutter: true,
            autofocus: false,
            gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter", "breakpoints"],
            autoCloseBrackets: true,
            matchBrackets: true,
            styleActiveLine: false,
            placeholder: "Type the formal model here...",
            extraKeys: {
                "Ctrl-Space": "autocomplete"
            }
        });
        editor.on("gutterClick", function (cm, n) {
            function makeMarker() {
                var marker = document.createElement("div");
                marker.style.position = "absolute";
                marker.style.border = "2px solid steelblue";
                marker.style.height = "16px";
                return marker;
            }
            console.log("line = " + n);
            var info = cm.lineInfo(n);
            cm.setGutterMarker(n, "breakpoints", info.gutterMarkers ? null : makeMarker());
        });
        var showHint = true;
        editor.on("inputRead", function (cm) {
            if (showHint) {
                CodeMirror.showHint(cm, CodeMirror.hint.pvs, { completeSingle: false, alignWithWord: true });
            }
        });
        editor.on("keyup", function (cm, event) {
            //ctrl+f triggers search -- this is done at a DOM level, see ModelEditor constructor
//            function isCtrlF(event) {
//                return event.ctrlKey && (event.keyCode === 70 || event.which === 70);
//            }
//            if (isCtrlF(event)) {
//                document.getElementById("model-editor-search-input").click();
//            }

            //show hints only when typing words
            function isLetter(event) {
                var keyCode = (event.ctrlKey || event.altKey) ? 0 : (event.keyCode || event.which);
                return keyCode >= 65 && keyCode <= 90;
            }
            showHint = (isLetter(event)) ? true : false;

            //mark dirty if the document changes or on undo/redo
            var selectedData = projectManager.getSelectedData();
            function isPrintableAscii(event) {
                var keyCode = (event.ctrlKey || event.altKey) ? 0 : (event.keyCode || event.which);
                return (keyCode === 8 || keyCode === 9 || keyCode === 13 || keyCode === 32 ||
                            (keyCode >= 46 && keyCode <= 90) || (keyCode >= 96 && keyCode <= 111) ||
                                (keyCode >= 186 && keyCode <= 192) || (keyCode >= 219 && keyCode <= 222));
            }
            function isCtrlZ(event) {
                return event.ctrlKey && (event.keyCode === 90 || event.which === 90);
            }
            function isCtrlX(event) {
                return event.ctrlKey && (event.keyCode === 88 || event.which === 88);
            }
            function isCtrlV(event) {
                return event.ctrlKey && (event.keyCode === 86 || event.which === 86);
            }
            if (!selectedData.isDirectory &&
                    ((!event.ctrlKey && isPrintableAscii(event)) || isCtrlZ(event) || isCtrlX(event) || isCtrlV(event))) {
                markDirty(selectedData);
            }
        });
        editor.setSize("100%", "400px"); // width, height
        projectManager.addListener("SelectedFileChanged", onSelectedFileChanged);

        document.getElementById("model-editor-search-input").addEventListener("click", function () {
            document.getElementById("model-editor-search-input").select();
            if (document.getElementById("model-editor-search-input").value === "") {
                editor.options.search = document.getElementById("model-editor-search-input").value;
                CodeMirror.commands.clearSearch(editor);
            }
        });
        document.getElementById("btnSearchNext").addEventListener("click", function () {
            editor.options.search = document.getElementById("model-editor-search-input").value;
            CodeMirror.commands.find(editor);
        });
        document.getElementById("btnSearchPrev").addEventListener("click", function () {
            editor.options.search = document.getElementById("model-editor-search-input").value;
            CodeMirror.commands.findPrev(editor);
        });

        onSelectedFileChanged({ selectedItem: projectManager.getSelectedData() });
        // render the file tree view
        projectManager.renderFileTreeView();
        // bind listeners for buttons in the toolbar
        bindListeners(projectManager);
        return Promise.resolve(true);
    };

    ModelEditor.prototype.unload = function () {
        editor = null;
        pvsioWebClient.removeCollapsiblePanel(editorContainer);
        return Promise.resolve(true);
    };

    ModelEditor.prototype.getEditor = function () {
        return editor;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new ModelEditor();
            }
            return instance;
        }
    };
});

//--- KeyCodes ---
//backspace	8
//tab	9
//enter	13
//shift	16
//ctrl	17
//alt	18
//pause/break	19
//caps lock	20
//escape	27
//(space)	32
//page up	33
//page down	34
//end	35
//home	36
//left arrow	37
//up arrow	38
//right arrow	39
//down arrow	40
//insert	45
//delete	46
//0	48
//1	49
//2	50
//3	51
//4	52
//5	53
//6	54
//7	55
//8	56
//9	57
//a	65
//b	66
//c	67
//d	68
//e	69
//f	70
//g	71
//h	72
//i	73
//j	74
//k	75
//l	76
//m	77
//n	78
//o	79
//p	80
//q	81
//r	82
//s	83
//t	84
//u	85
//v	86
//w	87
//x	88
//y	89
//z	90
//left window key	91
//right window key	92
//select key	93
//numpad 0	96
//numpad 1	97
//numpad 2	98
//numpad 3	99
//numpad 4	100
//numpad 5	101
//numpad 6	102
//numpad 7	103
//numpad 8	104
//numpad 9	105
//multiply	106
//add	107
//subtract	109
//decimal point	110
//divide	111
//f1	112
//f2	113
//f3	114
//f4	115
//f5	116
//f6	117
//f7	118
//f8	119
//f9	120
//f10	121
//f11	122
//f12	123
//num lock	144
//scroll lock	145
//semi-colon	186
//equal sign	187
//comma	188
//dash	189
//period	190
//forward slash	191
//grave accent	192
//open bracket	219
//back slash	220
//close braket	221
//single quote	222
