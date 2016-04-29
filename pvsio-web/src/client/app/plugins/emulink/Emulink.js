/**
 *
 * @author Paolo Masci
 * @date 25/05/14 6:39:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise, d3*/
define(function (require, exports, module) {
    "use strict";
    var ProjectManager		= require("project/ProjectManager"),
        ModelEditor         = require("plugins/modelEditor/ModelEditor"),
        PVSioWebClient      = require("PVSioWebClient"),
        EditorModeUtils     = require("plugins/emulink/EmuchartsEditorModes"),
        EmuchartsManager    = require("plugins/emulink/EmuchartsManager"),
        displayAddState        = require("plugins/emulink/forms/displayAddState"),
        displayRenameState     = require("plugins/emulink/forms/displayRenameState"),
        displayChangeStateColor= require("plugins/emulink/forms/displayChangeStateColor"),
        displayAddTransition   = require("plugins/emulink/forms/displayAddTransition"),
        displayRename          = require("plugins/emulink/forms/displayRename"),
        displayDelete          = require("plugins/emulink/forms/displayDelete"),
//        displayAddExpression   = require("plugins/emulink/forms/displayAddExpression"),
        displayAddVariable     = require("plugins/emulink/forms/displayAddVariable"),
        displayEditVariable    = require("plugins/emulink/forms/displayEditVariable"),
        displaySelectVariable  = require("plugins/emulink/forms/displaySelectVariable"),
        displaySelectTransition  = require("plugins/emulink/forms/displaySelectTransition"),
        displaySelectState     = require("plugins/emulink/forms/displaySelectState"),
        displayAddConstant     = require("plugins/emulink/forms/displayAddConstant"),
        displayEditConstant    = require("plugins/emulink/forms/displayEditConstant"),
        displaySelectConstant  = require("plugins/emulink/forms/displaySelectConstant"),
        QuestionForm           = require("pvsioweb/forms/displayQuestion"),
        EmuchartsPVSPrinter    = require("plugins/emulink/EmuchartsPVSPrinter"),
        EmuchartsLustrePrinter = require("plugins/emulink/EmuchartsLustrePrinter"),
        EmuchartsPIMPrinter    = require("plugins/emulink/EmuchartsPIMPrinter"),
        EmuchartsCppPrinter    = require("plugins/emulink/EmuchartsCppPrinter"),
        EmuchartsMALPrinter    = require("plugins/emulink/EmuchartsMALPrinter2"),
        EmuchartsVDMPrinter    = require("plugins/emulink/EmuchartsVDMPrinter"),
        EmuchartsJSPrinter     = require("plugins/emulink/EmuchartsJavaScriptPrinter"),
        EmuchartsAdaPrinter    = require("plugins/emulink/EmuchartsAdaPrinter"),
        EmuchartsBlessPrinter  = require("plugins/emulink/EmuchartsBlessPrinter"),
//        EmuchartsTextEditor    = require("plugins/emulink/EmuchartsTextEditor"),
        FileHandler            = require("filesystem/FileHandler"),
        FileSystem             = require("filesystem/FileSystem"),
        displayNotificationView  = require("plugins/emulink/forms/displayNotificationView"),
        PimTestGenerator       = require("plugins/emulink/models/pim/PIMTestGenerator"),
        PIMImporter            = require("plugins/emulink/models/pim/PIMImporter"),
        PIMEmulink             = require("plugins/emulink/models/pim/PIMEmulink"),
        ContextTable           = require("plugins/emulink/tools/ContextTable"),
        MachineStatesTable     = require("plugins/emulink/tools/MachineStatesTable"),
        TransitionsTable       = require("plugins/emulink/tools/TransitionsTable"),
        ExportDiagram          = require("plugins/emulink/tools/ExportDiagram"),
        MIME                   = require("util/MIME").getInstance();
    
    var instance;
    var fs;
    var projectManager;
    var editor;
    var ws;
    var pvsioWebClient;
    var canvas;

    var emuchartsManager;
    var MODE;
    var emuchartsPVSPrinter;
    var emuchartsLustrePrinter;
    var emuchartsPIMPrinter;
    var emuchartsCppPrinter;
    var emuchartsMALPrinter;
    var emuchartsVDMPrinter;
    var emuchartsJSPrinter;
    var emuchartsAdaPrinter;
    var emuchartsBlessPrinter;
    var pimImporter;
    var pimTestGenerator;
    var pimEmulink;
    var contextTable;
    var machineStatesTable;
    var transitionsTable;
    var exportDiagram;

    var options = { autoinit: true };

    var displayNotification = function (msg, title) {
        title = title || "Notification";
        displayNotificationView.create({
            header: title,
            message: msg,
            buttons: ["Ok"]
        }).on("ok", function (e, view) {
            view.remove();
        });
    };

    function initToolbars() {
        // make sure the svg is visible
        d3.select("#EmuchartLogo").classed("hidden", true);
        d3.select("#graphicalEditor").classed("hidden", false);
        // reset toolbar color
        document.getElementById("btn_toolbarBrowse").style.background = "black";
        document.getElementById("btn_toolbarAddState").style.background = "black";
        document.getElementById("btn_toolbarAddTransition").style.background = "black";
        document.getElementById("btn_toolbarRename").style.background = "black";
        document.getElementById("btn_toolbarDelete").style.background = "black";
    }

    function modeChange_callback(event) {
        var EmuchartsEditorMode = document.getElementById("EmuchartsEditorMode");
        if (EmuchartsEditorMode) {
            if (event.mode === MODE.BROWSE()) {
                EmuchartsEditorMode.style.background = "green";
            } else { EmuchartsEditorMode.style.background = "steelblue"; }
            EmuchartsEditorMode.textContent = "Editor mode: " + MODE.mode2string(event.mode);
        }
        var infoBox = document.getElementById("infoBox");
        if (infoBox) {
            infoBox.value = MODE.modeTooltip(event.mode);
        }
    }

    function addState_handler(evt) {
        var stateID = emuchartsManager.getFreshStateName();
        var position = { x: evt.mouse[0], y: evt.mouse[1] };
        emuchartsManager.add_state(stateID, position);
        if (options.autoinit && emuchartsManager.getStates().length === 1) {
            var newTransitionName = emuchartsManager.getFreshInitialTransitionName();
            emuchartsManager.add_initial_transition(newTransitionName, stateID);
        }
    }

    function deleteTransition_handler(event) {
        var transitionID = event.edge.id;
        emuchartsManager.delete_transition(transitionID);
    }

    function deleteInitialTransition_handler(event) {
        var transitionID = event.edge.id;
        emuchartsManager.delete_initial_transition(transitionID);
    }

    function deleteState_handler(event) {
        var stateID = event.node.id;
        emuchartsManager.delete_state(stateID);
    }

    var maxLen = 48;

    // rename dialog window for states
    function renameState(theState) {
        if (emuchartsManager.getIsPIM()) {
            return pimEmulink.editState(theState);
        }
        
        displayRenameState.create({
            header: "Renaming state " + theState.name.substring(0, maxLen) + "...",
            textLabel: {
                newStateName: "State name",
                newStateColor: "State color"
            },
            placeholder: {
                newStateName: theState.name,
                newStateColor: theState.color
            },
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newStateName = e.data.labels.get("newStateName");
            var newStateColor = e.data.labels.get("newStateColor");
            if (newStateName && newStateName.value !== "") {
                emuchartsManager.edit_state(
                    theState.id,
                    { name: newStateName,
                      color: newStateColor }
                );
                view.remove();
                machineStatesTable.setMachineStates(emuchartsManager.getStates());
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }
    // change state color dialog
    function changeStateColor(theState) {
        if (emuchartsManager.getIsPIM()) {
            return pimEmulink.editState(theState);
        }
        
        displayChangeStateColor.create({
            header: "Renaming state " + theState.name.substring(0, maxLen) + "...",
            textLabel: {
                newStateName: "State name",
                newStateColor: "State color"
            },
            placeholder: {
                newStateName: theState.name,
                newStateColor: theState.color
            },
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newStateName = e.data.labels.get("newStateName");
            var newStateColor = e.data.labels.get("newStateColor");
            if (newStateColor && newStateColor.value !== "") {
                emuchartsManager.edit_state(
                    theState.id,
                    { name: newStateName,
                      color: newStateColor }
                );
                view.remove();
                machineStatesTable.setMachineStates(emuchartsManager.getStates());
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }

    function renameState_handler(event) {
        renameState(event.node);
    }
    function changeStateColor_handler(event) {
        changeStateColor(event.node);
    }

    // rename dialog window for transitions
    function editTransition(t) {
        if (emuchartsManager.getIsPIM()) {
            pimEmulink.editTransition(t);
            return;
        }

        displayRename.create({
            header: "Renaming transition " + t.name.substring(0, maxLen) + "...",
            required: false,
            currentLabel: t.name, // this dialog will show just one transition
            buttons: ["Cancel", "Rename"]
        }).on("rename", function (e, view) {
            var transitionLabel = e.data.labels.get("newLabel");
            if (!transitionLabel) { transitionLabel = ""; }
            emuchartsManager.rename_transition(t.id, transitionLabel);
            view.remove();
            transitionsTable.setTransitions(emuchartsManager.getTransitions());
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    }

    function renameTransition_handler(event) {
        editTransition(event.edge);
    }

    // rename dialog window for initial transitions
    function editInitialTransition(t) {
        displayRename.create({
            header: "Renaming initial transition " + t.name.substring(0, maxLen) + "...",
            required: false,
            currentLabel: t.name,
            buttons: ["Cancel", "Rename"]
        }).on("rename", function (e, view) {
            var transitionLabel = e.data.labels.get("newLabel");
            if (!transitionLabel) { transitionLabel = ""; }
            emuchartsManager.rename_initial_transition(t.id, transitionLabel);
            view.remove();
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    }
    function renameInitialTransition_handler(event) {
        editInitialTransition(event.edge);
    }

    function addTransition_handler(event) {
        var newTransitionName = emuchartsManager.getFreshTransitionName();
        emuchartsManager.add_transition(newTransitionName,
                                        event.source.id,
                                        event.target.id);
    }

    function addInitialTransition_handler(event) {
        var newTransitionName = emuchartsManager.getFreshInitialTransitionName();
        emuchartsManager.add_initial_transition(newTransitionName, event.target.id);
    }
    
    function stateAdded_handler(event) {
        machineStatesTable.addMachineStates([ event.state ]);
    }
    function stateRemoved_handler(event) {
        machineStatesTable.setMachineStates(emuchartsManager.getStates());
    }
    function stateRenamed_handler(event) { }//print_theory(); print_node(); }
    function stateColorChanged_handler(event) { }//print_theory(); print_node(); }
    function transitionAdded_handler(event) {
        transitionsTable.addTransitions([ event.transition ]);
    }
    function transitionRemoved_handler(event) {
        transitionsTable.setTransitions(emuchartsManager.getTransitions());
    }
    function transitionRenamed_handler(event) { }//print_theory(); print_node(); }
    function initialTransitionAdded_handler(event) { }//console.log("initial transition added"); }//print_theory(); print_node(); }
    function initialTransitionRemoved_handler(event) { }//console.log("initial transition removed"); }//print_theory(); print_node(); }
    function initialTransitionRenamed_handler(event) { }//console.log("initial transition renamed"); }//print_theory(); print_node(); }
    function constantAdded_handler(event) { }//print_theory(); print_node(); }
    function variableAdded_handler(event) {
        contextTable.addContextVariables([ event.variable ]);
    }
    function variableRemoved_handler(event) {
        contextTable.setContextVariables(emuchartsManager.getVariables());
    }


    /**
     * Constructor
     * @memberof Emulink
     */
    function Emulink() {
        emuchartsPVSPrinter = new EmuchartsPVSPrinter("emuchart_th");
        emuchartsLustrePrinter = new EmuchartsLustrePrinter("emuchart_Lustre");
        emuchartsPIMPrinter = new EmuchartsPIMPrinter("emuchart_PIM");
        emuchartsCppPrinter = new EmuchartsCppPrinter("emuchart_Cpp");
        emuchartsMALPrinter = new EmuchartsMALPrinter("emuchart_MAL");
        emuchartsVDMPrinter = new EmuchartsVDMPrinter("emuchart_VDM");
        emuchartsJSPrinter = new EmuchartsJSPrinter("emucharts_JS");
        emuchartsAdaPrinter = new EmuchartsAdaPrinter("emucharts_Ada");
        emuchartsBlessPrinter = new EmuchartsBlessPrinter("emucharts_Bless");

        pvsioWebClient = PVSioWebClient.getInstance();
        MODE = new EditorModeUtils();
        emuchartsManager = new EmuchartsManager();
        emuchartsManager.addListener("emuCharts_editorModeChanged", modeChange_callback);
        emuchartsManager.addListener("emuCharts_addState", addState_handler);
//        emuchartsManager.addListener("emuCharts_d3ZoomTranslate", d3ZoomTranslate_handler);
        emuchartsManager.addListener("emuCharts_deleteTransition", deleteTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteInitialTransition", deleteInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteState", deleteState_handler);
        emuchartsManager.addListener("emuCharts_renameState", renameState_handler);
        emuchartsManager.addListener("emuCharts_changeStateColor", changeStateColor_handler);
        emuchartsManager.addListener("emuCharts_renameTransition", renameTransition_handler);
        emuchartsManager.addListener("emuCharts_renameInitialTransition", renameInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_addTransition", addTransition_handler);
        emuchartsManager.addListener("emuCharts_addInitialTransition", addInitialTransition_handler);

        emuchartsManager.addListener("emuCharts_stateAdded", stateAdded_handler);
        emuchartsManager.addListener("emuCharts_stateRemoved", stateRemoved_handler);
        emuchartsManager.addListener("emuCharts_constantAdded", constantAdded_handler);
        emuchartsManager.addListener("emuCharts_variableAdded", variableAdded_handler);
        emuchartsManager.addListener("emuCharts_variableRemoved", variableRemoved_handler);
        emuchartsManager.addListener("emuCharts_transitionAdded", transitionAdded_handler);
        emuchartsManager.addListener("emuCharts_transitionRenamed", transitionRenamed_handler);
        emuchartsManager.addListener("emuCharts_transitionRemoved", transitionRemoved_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionAdded", initialTransitionAdded_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionRenamed", initialTransitionRenamed_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionRemoved", initialTransitionRemoved_handler);
        emuchartsManager.addListener("emuCharts_stateRenamed", stateRenamed_handler);
        emuchartsManager.addListener("emuCharts_stateColorChanged", stateColorChanged_handler);
        fs = new FileSystem();

        // PIM objects.
        pimImporter = new PIMImporter();
        pimEmulink = new PIMEmulink(emuchartsManager);
        pimTestGenerator = new PimTestGenerator("pim_Test_Gen");
        
        exportDiagram = new ExportDiagram();
    }

    Emulink.prototype.getName = function () {
        return "EmuCharts Editor";
    };
    
    function editVariable (variableID) {
        var variableScopes = emuchartsManager.getVariableScopes();
        var theVariable = emuchartsManager.getVariable(variableID);
        var scopeOptions = [];
        variableScopes.forEach(function (option) {
            if (option === theVariable.scope) {
                scopeOptions.push({ value: option, selected: true});
            } else {
                scopeOptions.push({ value: option, selected: false});
            }
        });
        displayEditVariable.create({
            header: "Editing variable " + theVariable.name,
            textLabel: {
                newVariableName: "Variable name",
                newVariableType: "Variable type",
                newVariableValue: "Initial value",
                newVariableScope: "Variable scope"
            },
            placeholder: {
                newVariableName: theVariable.name,
                newVariableType: theVariable.type,
                newVariableValue: theVariable.value,
                newVariableScope: theVariable.scope
            },
            scopeOptions: scopeOptions,
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newVariableName = e.data.labels.get("newVariableName");
            var newVariableType = e.data.labels.get("newVariableType");
            var newVariableValue = e.data.labels.get("newVariableValue");
            var newVariableScope = variableScopes[e.data.options.get("newVariableScope")];
            if (newVariableName && newVariableName.value !== "" &&
                    newVariableType && newVariableType.value !== "" &&
                    newVariableValue && newVariableValue.value !== "") {
                emuchartsManager.rename_variable(
                    theVariable.id,
                    {   name: newVariableName,
                        type: newVariableType,
                        value: newVariableValue,
                        scope: newVariableScope   }
                );
                view.remove();
                contextTable.setContextVariables(emuchartsManager.getVariables());
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }

    Emulink.prototype.createHtmlElements = function () {
        var _this = this;
        var content = require("text!plugins/emulink/forms/maincontent.handlebars");
        canvas = pvsioWebClient.createCollapsiblePanel({
            headerText: "EmuCharts Editor",
            showContent: true,
            owner: _this.getName()
        });
        canvas = canvas.html(content);
        if (document.getElementById("StateAttributes")) {
            contextTable = new ContextTable();
            contextTable.addListener("ContextTable_deleteVariable", function(event) {
                emuchartsManager.delete_variable(event.variable.id);
            });
            contextTable.addListener("ContextTable_editVariable", function(event) {
                editVariable(event.variable.id);
            });
        }
        if (document.getElementById("MachineStates")) {
            machineStatesTable = new MachineStatesTable();
            machineStatesTable.addListener("MachineStatesTable_deleteState", function(event) {
                emuchartsManager.delete_state(event.state.id);
            });
            machineStatesTable.addListener("MachineStatesTable_renameState", function(event) {
                var theState = emuchartsManager.getState(event.state.id);
                renameState(theState);
            });
            machineStatesTable.addListener("MachineStatesTable_changeStateColor", function(event) {
                var theState = emuchartsManager.getState(event.state.id);
                changeStateColor(theState);
            });
        }
        if (document.getElementById("TransitionsTable")) {
            transitionsTable = new TransitionsTable();
            transitionsTable.addListener("TransitionsTable_deleteTransition", function(event) {
                emuchartsManager.delete_transition(event.transition.id);
            });
            transitionsTable.addListener("TransitionsTable_renameTransition", function(event) {
                var theTransition = emuchartsManager.getTransition(event.transition.id);
                editTransition(theTransition);
            });
        }
        
        // bootstrap buttons
        function openChart(callback) {
            function doOpen(err, res) {
                if (res) {
                    if (res.name.lastIndexOf(".emdl") === res.name.length - 5) {
                        res.content = JSON.parse(res.content);
                        emuchartsManager.importEmucharts(res);
                    } else if (res.name.lastIndexOf(".muz") === res.name.length - 4) {
                        emuchartsManager.importPIMChart(res);
                    } else {
                        // Try parse as PIM
                        pimImporter.importPIM(res, emuchartsManager);
                    }
                    if (callback && typeof callback === "function") {
                        callback(err, res);
                    }
                } else {
                    console.log("Error while opening file (" + err + ")");
                }
            }
            var opt = {
                header: "Open EmuChart file...",
                extensions: ".emdl,.muz,.pim"
            };
            if (PVSioWebClient.getInstance().serverOnLocalhost()) {
                return new Promise(function (resolve, reject) {
                    fs.readFileDialog({
                        encoding: "utf8",
                        title: opt.header,
                        filter: MIME.filter(opt.extensions.split(","))
                    }).then(function (descriptors) {
                        doOpen(null, descriptors[0]);
                        resolve(descriptors[0]);
                    }).catch(function (err) {
                        doOpen(err, null);
                        reject(err);
                    });
                });
            } else {
                FileHandler.openLocalFileAsText(function (err, res) {
                    doOpen(err, res);
                }, opt);
            }
        }
        function importChart(callback) {
            var opt = {
                header: "Import Chart...",
                extensions: ".muz,.pim"
            };
            // MUZ
            FileHandler.openLocalFileAsText(function (err, res) {
                if (res) {
                    if (res.name.lastIndexOf(".muz") === res.name.length - 4) {
                        emuchartsManager.importPIMChart(res);
                    }
                    else {
                        pimImporter.importPIM(res, emuchartsManager);
                    }
                    if (callback && typeof callback === "function") {
                        callback(err, res);
                    }
                }
            }, opt);
        }

        function restartEditor() {
            // set initial editor mode
            emuchartsManager.set_editor_mode(MODE.BROWSE());            
            // render emuchart
            emuchartsManager.render();
            // make svg visible and reset colors
            initToolbars();
            // set initial editor mode
            d3.select("#btn_toolbarBrowse").node().click();
            // set Variables Table
            contextTable.setContextVariables(emuchartsManager.getVariables());
            // set Machine States Table
            machineStatesTable.setMachineStates(emuchartsManager.getStates());
            // set Transitions Table
            transitionsTable.setTransitions(emuchartsManager.getTransitions());        
        }
        
        d3.select("#btnNewEmuchart").on("click", function () {
            d3.select("#EmuchartLogo").classed("hidden", true);
            d3.select("#graphicalEditor").classed("hidden", false);
            emuchartsManager.newEmucharts("emucharts.pvs");
            restartEditor();
        });
        d3.select("#btnLoadEmuchart").on("click", function () {
            openChart(function f() {
                restartEditor();
            });
        });

        // toolbar
        d3.select("#btn_toolbarAddState").on("click", function () {
            initToolbars();
            this.style.background = "steelblue";
            emuchartsManager.set_editor_mode(MODE.ADD_STATE());
        });
        d3.select("#btn_toolbarAddTransition").on("click", function () {
            initToolbars();
            this.style.background = "steelblue";
            emuchartsManager.set_editor_mode(MODE.ADD_TRANSITION());
        });
        d3.select("#btn_toolbarRename").on("click", function () {
            initToolbars();
            this.style.background = "steelblue";
            emuchartsManager.set_editor_mode(MODE.RENAME());
        });
        d3.select("#btn_toolbarDelete").on("click", function () {
            initToolbars();
            this.style.background = "steelblue";
            emuchartsManager.set_editor_mode(MODE.DELETE());
        });
        d3.select("#btn_toolbarBrowse").on("click", function () {
            initToolbars();
            this.style.background = "green";
            emuchartsManager.set_editor_mode(MODE.BROWSE());
        });
        d3.select("#btn_toolbarZoomIn").on("click", function () {
            emuchartsManager.zoom_in();
        });
        d3.select("#btn_toolbarZoomOut").on("click", function () {
            emuchartsManager.zoom_out();
        });
        d3.select("#btn_toolbarZoomReset").on("click", function () {
            emuchartsManager.zoom_reset();
        });




        //-- Emuchart menu -----------------------------------------------------------
        d3.select("#menuEmuchart").on("mouseover", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            if (!emuchartsManager.empty_chart()) {
                // we need to delete the current chart because we handle one chart at the moment
                QuestionForm.create({
                    header: "Warning: unsaved changes will be discarded.",
                    question: "Unsaved changes in the current chart will be discarded."
                                + "Would you like continue?",
                    buttons: ["Cancel", "Ok"]
                }).on("ok", function (e, view) {
                    emuchartsManager.delete_chart();
                    d3.select("#btnNewEmuchart").node().click();
                    view.remove();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
            }
        });
        d3.select("#btn_menuCloseChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            if (!emuchartsManager.empty_chart()) {
                // we need to delete the current chart because we handle one chart at the moment
                QuestionForm.create({
                    header: "Warning: the current chart has unsaved changes.",
                    question: "The current chart has unsaved changes that will be lost. Confirm Close?",
                    buttons: ["Cancel", "Confirm close"]
                }).on("ok", function (e, view) {
                    emuchartsManager.delete_chart();
                    d3.select("#btn_toolbarBrowse").node().click();
                    view.remove();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
            }
        });
        d3.select("#btn_menuOpenChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            // we need to delete the current chart because we handle one chart at the moment
            if (!emuchartsManager.empty_chart()) {
                QuestionForm.create({
                    header: "Warning: unsaved changes will be discarded.",
                    question: "Unsaved changes in the current chart will be discarded."
                                + "Would you like continue?",
                    buttons: ["Cancel", "Ok"]
                }).on("ok", function (e, view) {
                    emuchartsManager.delete_chart();
                    document.getElementById("btnLoadEmuchart").click();
                    view.remove();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
            } else {
                emuchartsManager.delete_chart();
                document.getElementById("btnLoadEmuchart").click();
            }
        });
        d3.select("#btn_menuImportChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            // we need to delete the current chart because we handle one chart at the moment
            QuestionForm.create({
                header: "Warning: unsaved changes will be discarded.",
                question: "Unsaved changes in the current chart will be discarded."
                            + "Would you like continue?",
                buttons: ["Cancel", "Ok"]
            }).on("ok", function (e, view) {
                emuchartsManager.delete_chart();
                //document.getElementById("btnImportChart").click();
                view.remove();
                importChart(function f() {
                    // render emuchart
                    emuchartsManager.render();
                    // make svg visible and reset colors
                    initToolbars();
                    // set initial editor mode
                    d3.select("#btn_toolbarBrowse").node().click();
                });
            }).on("cancel", function (e, view) {
                view.remove();
            });
        });
        d3.select("#btn_menuQuitEmulink").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            if (!emuchartsManager.empty_chart()) {
                // we need to delete the current chart because we handle one chart at the moment
                QuestionForm.create({
                    header: "Warning: the current chart has unsaved changes.",
                    question: "The current chart has unsaved changes that will be lost. Confirm quit?",
                    buttons: ["Cancel", "Quit Emulink"]
                }).on("ok", function (e, view) {
                    emuchartsManager.delete_chart();
                    initToolbars();
                    view.remove();
                    // FIXME: need a better way to deselect the checkbox
                    document.getElementById("plugin_Emulink").checked = false;
                    _this.unload();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
            }
        });
        d3.select("#btn_menuSaveChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            if (!emuchartsManager.empty_chart()) {
                var name = "emucharts_" + projectManager.project().name() + ".emdl";
                var emuchart = {
                    descriptor: {
                        file_type: "emdl",
                        version: "1.3",
                        description: "emucharts model",
                        chart_name: ("emucharts_" + projectManager.project().name())
                    },
                    chart: {
                        states: emuchartsManager.getStates(),
                        transitions: emuchartsManager.getTransitions(),
                        initial_transitions: emuchartsManager.getInitialTransitions(),
                        constants: emuchartsManager.getConstants(),
                        variables: emuchartsManager.getVariables()
                    }
                };
                // PIM.
                emuchart.chart.pmr = emuchartsManager.getPMR(null, true);
                emuchart.chart.isPIM = emuchartsManager.getIsPIM();

                var content = JSON.stringify(emuchart, null, " ");
                projectManager.project().addFile(name, content, { overWrite: true }).then(function (res) {
                    //displayNotification("File " + name + " saved successfully!");
                }).catch(function (err) {
                    var msg = "Error while saving file " + name + " (" + JSON.stringify(err) + ")";
                    console.log(msg);
                    displayNotification(msg);
                });
            }
        });
        d3.select("#btn_menuExportAsImage").on("click", function () {
            exportDiagram.toVectorialImage(emuchartsManager);
        });

        //-- States menu -----------------------------------------------------------
        d3.select("#menuStates").on("mouseover", function () {
            document.getElementById("menuStates").children[1].style.display = "block";
        });

        d3.select("#btn_menuNewState").on("click", function () {
            document.getElementById("menuStates").children[1].style.display = "none";
            var label = emuchartsManager.getFreshStateName();
            displayAddState.create({
                header: "Please enter a label for the new machine state",
                textLabel: label,
                buttons: ["Cancel", "Create"]
            }).on("create", function (e, view) {
                var nodeLabel = e.data.labels.get("newLabel");
                emuchartsManager.add_state(nodeLabel);
                view.remove();
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });
        d3.select("#btn_menuRenameState").on("click", function () {
            document.getElementById("menuStates").children[1].style.display = "none";
            var states = emuchartsManager.getStates();
            var labels = [];
            states.forEach(function (state) {
                labels.push(state.name + "  (id: " + state.id + ")");
            });
            displaySelectState.create({
                header: "Editing states...",
                message: "Please select a state",
                transitions: labels,
                buttons: ["Cancel", "Select"]
            }).on("select", function (e, view) {
                if (states.length > 0) {
                    var v = e.data.options.get("selectedState");
                    var theState = states[v];
                    view.remove();
                    renameState(theState);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });
        d3.select("#btn_menuDeleteState").on("click", function () {
            document.getElementById("menuStates").children[1].style.display = "none";
            var states = emuchartsManager.getStates();
            var labels = [];
            states.forEach(function (state) {
                labels.push(state.name + "  (id: " + state.id + ")");
            });
            displayDelete.create({
                header: "Please select state to be deleted...",
                textLabel: "State to be deleted",
                currentLabels: labels,
                buttons: ["Cancel", "Delete"]
            }).on("delete", function (e, view) {
                var s = e.data.options.get("currentLabel");
                var stateID = states[s].id;
                emuchartsManager.delete_state(stateID);
                view.remove();
            }).on("cancel", function (e, view) {
                // just remove rename window
                view.remove();
            });
        });

        //-- Transitions menu -----------------------------------------------------------
        d3.select("#menuTransitions").on("mouseover", function () {
            document.getElementById("menuTransitions").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewTransition").on("click", function () {
            document.getElementById("menuTransitions").children[1].style.display = "none";
//            var newTransitionName = emuchartsManager.getFreshTransitionName();
            var states = emuchartsManager.getStates();
            var labels = [];
            states.forEach(function (state) {
                labels.push(state.name + "  (id: " + state.id + ")");
            });
            displayAddTransition.create({
                header: "Please enter label for new transition",
                textLabel: "New transition",
                sourceNodes: labels,
                targetNodes: labels,
                buttons: ["Cancel", "Create"]
            }).on("create", function (e, view) {
                var transitionLabel = e.data.labels.get("newLabel");
                if (transitionLabel && transitionLabel.value !== "") {
                    var sourceNode = e.data.options.get("sourceNode");
                    var sourceNodeID = states[sourceNode].id;
                    var targetNode = e.data.options.get("targetNode");
                    var targetNodeID = states[targetNode].id;
                    emuchartsManager.add_transition(transitionLabel, sourceNodeID, targetNodeID);
                    view.remove();
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });
        d3.select("#btn_menuRenameTransition").on("click", function () {
            document.getElementById("menuTransitions").children[1].style.display = "none";
            var transitions = emuchartsManager.getTransitions();
            var nTransitions = transitions.length;
            var initialTransitions = emuchartsManager.getInitialTransitions();
            initialTransitions.forEach(function (it) {
                transitions.push(it);
            });
            var labels = [];
            transitions.forEach(function (transition) {
                if (transition.source) {
                    labels.push(transition.name + "  ("
                                + transition.source.name + "->"
                                + transition.target.name + ")");
                } else {
                    labels.push(transition.name + "  ("
                                + "INIT" + "->"
                                + transition.target.name + ")");
                }
            });
            displaySelectTransition.create({
                header: "Editing transitions...",
                message: "Please select a transition",
                transitions: labels,
                buttons: ["Cancel", "Select"]
            }).on("select", function (e, view) {
                if (transitions.length > 0) {
                    var v = e.data.options.get("selectedTransition");
                    var theTransition = transitions[v];
                    view.remove();
                    if (v < nTransitions) {
                        editTransition(theTransition);
                    } else {
                        editInitialTransition(theTransition);
                    }
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });
        d3.select("#btn_menuDeleteTransition").on("click", function () {
            document.getElementById("menuTransitions").children[1].style.display = "none";
            var transitions = emuchartsManager.getTransitions();
            var initialTransitions = emuchartsManager.getInitialTransitions();
            initialTransitions.forEach(function (it) {
                transitions.push(it);
            });
            var labels = [];
            transitions.forEach(function (transition) {
                if (transition.source) {
                    labels.push(transition.name + "  ("
                                + transition.source.name + "->"
                                + transition.target.name + ")");
                } else {
                    labels.push(transition.name + "  ("
                                + "INIT" + "->"
                                + transition.target.name + ")");
                }
            });
            displayDelete.create({
                header: "Please select transition to be deleted...",
                textLabel: "Transition to be deleted",
                currentLabels: labels,
                buttons: ["Cancel", "Delete"]
            }).on("delete", function (e, view) {
                var t = e.data.options.get("currentLabel");
                var transitionID = transitions[t].id;
                emuchartsManager.delete_transition(transitionID);
                emuchartsManager.delete_initial_transition(transitionID);
                view.remove();
            }).on("cancel", function (e, view) {
                // just remove rename window
                view.remove();
            });
        });

        //-- Context menu -----------------------------------------------------------
        d3.select("#menuContext").on("mouseover", function () {
            document.getElementById("menuContext").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewVariable").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            var scopeOptions = emuchartsManager.getVariableScopes();
            displayAddVariable.create({
                header: "Please enter new state variable...",
                textLabel: {
                    newVariableName: "Variable name",
                    newVariableType: "Variable type",
                    newVariableValue: "Initial value",
                    newVariableScope: "Variable scope"
                },
                placeholder: {
                    newVariableName: "Name, e.g., display",
                    newVariableType: "Type, e.g., real",
                    newVariableValue: "Value, e.g., 0"
                },
                scopeOptions: scopeOptions,
                buttons: ["Cancel", "Create variable"]
            }).on("create_variable", function (e, view) {
                console.log("add variable");
                var newVariableName = e.data.labels.get("newVariableName");
                var newVariableType = e.data.labels.get("newVariableType");
                var newVariableValue = e.data.labels.get("newVariableValue");
                var newVariableScope = scopeOptions[e.data.options.get("newVariableScope")];
                if (newVariableName && newVariableName.value !== "" &&
                        newVariableType && newVariableType.value !== "" &&
                        newVariableValue && newVariableValue.value !== "") {
                    emuchartsManager.add_variable({
                        name: newVariableName,
                        type: newVariableType,
                        value: newVariableValue,
                        scope: newVariableScope
                    });
                    view.remove();
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });  
        d3.select("#btn_menuEditVariable").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the variable that needs to be edited
            var stateVariables = emuchartsManager.getVariables();
            var labels = [];
            var variables = [];
            stateVariables.forEach(function (variable) {
                labels.push(variable.name + ": " + variable.type + " (" + variable.scope + ")");
                variables.push(variable);
            });
            displaySelectVariable.create({
                header: "Edit state variable",
                message: "Please select a state variable",
                variables: labels,
                buttons: ["Cancel", "Select"]
            }).on("select", function (e, view) {
                if (variables.length > 0) {
                    var v = e.data.options.get("selectedVariable");
                    var theVariable = variables[v];
                    view.remove();
                    editVariable(theVariable.id);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });

        d3.select("#btn_menuDeleteVariable").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the variable that needs to be edited
            var stateVariables = emuchartsManager.getVariables();
            var labels = [];
            var variables = [];
            stateVariables.forEach(function (variable) {
                labels.push(variable.name + ": " + variable.type + " (" + variable.scope + ")");
                variables.push(variable);
            });
            displaySelectVariable.create({
                header: "Delete state variable",
                message: "Please select a state variable",
                variables: labels,
                buttons: ["Cancel", "Delete Variable"]
            }).on("delete_variable", function (e, view) {
                if (variables.length > 0) {
                    var v = e.data.options.get("selectedVariable");
                    var theVariable = variables[v];
                    view.remove();
                    emuchartsManager.delete_variable(theVariable.id);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });

        d3.select("#btn_menuNewConstant").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            displayAddConstant.create({
                header: "Please enter new constant...",
                textLabel: {
                    newConstantName: "Constant name",
                    newConstantType: "Constant type",
                    newConstantValue: "Constant value"
                },
                placeholder: {
                    newConstantName: "Name, e.g., maxRate",
                    newConstantType: "Type, e.g., real",
                    newConstantValue: "Value, e.g., 1200"
                },
                buttons: ["Cancel", "Create constant"]
            }).on("create_constant", function (e, view) {
                var newConstantName = e.data.labels.get("newConstantName");
                var newConstantType = e.data.labels.get("newConstantType");
                var newConstantValue = e.data.labels.get("newConstantValue");
                if (newConstantName && newConstantName.value !== ""
                        && newConstantType && newConstantType.value !== "") {
                    emuchartsManager.add_constant({
                        name: newConstantName,
                        type: newConstantType,
                        value: newConstantValue //value can be left unspecified (uninterpreted constant)
                    });
                    view.remove();
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuEditConstant").on("click", function () {
            var editConstant = function (theConstant) {
                displayEditConstant.create({
                    header: "Editing constant " + theConstant.name,
                    textLabel: {
                        newConstantName: "Constant name",
                        newConstantType: "Constant type",
                        newConstantValue: "Constant value"
                    },
                    placeholder: {
                        newConstantName: theConstant.name,
                        newConstantType: theConstant.type,
                        newConstantValue: theConstant.value
                    },
                    buttons: ["Cancel", "Ok"]
                }).on("ok", function (e, view) {
                    var newConstantName = e.data.labels.get("newConstantName");
                    var newConstantType = e.data.labels.get("newConstantType");
                    var newConstantValue = e.data.labels.get("newConstantValue");
                    if (newConstantName && newConstantName.value !== ""
                            && newConstantType && newConstantType.value !== "") {
                        emuchartsManager.rename_constant(
                            theConstant.id,
                            {   name: newConstantName,
                                type: newConstantType,
                                value: newConstantValue   }
                        );
                        view.remove();
                    }
                }).on("cancel", function (e, view) {
                    // just remove window
                    view.remove();
                });
            };

            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the variable that needs to be edited
            var constants = emuchartsManager.getConstants();
            var labels = [];
            constants.forEach(function (constant) {
                var l = constant.name + ": " + constant.type;
                if (constant.value) {
                    l += " = " + constant.value;
                }
                labels.push(l);
                constants.push(constant);
            });
            displaySelectConstant.create({
                header: "Edit constant",
                message: "Please select a constant",
                constants: labels,
                buttons: ["Cancel", "Select"]
            }).on("select", function (e, view) {
                if (constants.length > 0) {
                    var c = e.data.options.get("selectedConstant");
                    var theConstant = constants[c];
                    view.remove();
                    editConstant(theConstant);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });

        d3.select("#btn_menuDeleteConstant").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the variable that needs to be edited
            var constants = emuchartsManager.getConstants();
            var labels = [];
            constants.forEach(function (constant) {
                var l = constant.name + ": " + constant.type;
                if (constant.value) {
                    l += " = " + constant.value;
                }
                labels.push(l);
                constants.push(constant);
            });
            displaySelectConstant.create({
                header: "Delete constant",
                message: "Please select a constant",
                constants: labels,
                buttons: ["Cancel", "Delete Constant"]
            }).on("delete_constant", function (e, view) {
                if (constants.length > 0) {
                    var c = e.data.options.get("selectedConstant");
                    var theConstant = constants[c];
                    view.remove();
                    emuchartsManager.delete_constant(theConstant.id);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });

        //-- Code generators menu -----------------------------------------------------------
        d3.select("#menuCodeGenenerators").on("mouseover", function () {
            document.getElementById("menuCodeGenenerators").children[1].style.display = "block";
        });
        d3.select("#btn_menuPVSPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_th"),
                author: {
                    name: "Paolo Masci",
                    affiliation: "Queen Mary University of London, United Kingdom",
                    contact: "http://www.eecs.qmul.ac.uk/~masci/"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsPVSPrinter.print(emucharts);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".pvs";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, PVS model is undefined.");
            }
        });
        d3.select("#btn_menuPIMPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_PIM"),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsPIMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".tex";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, PIM model is undefined.");
            }
        });
        d3.select("#btn_menuCppPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name()),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsPIMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".cpp";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, C++ model is undefined.");
            }
        });
        d3.select("#btn_menuMALPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_MAL"),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsMALPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".i";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, MAL model is undefined.");
            }
        });
        d3.select("#btn_menuVDMPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_VDM"),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsVDMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".vdmsl";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, VDM model is undefined.");
            }
        });

        d3.select("#btn_menuJavaScriptPrinter").on("click", function () {
            var emucharts = {
                name: (emuchartsJSPrinter.modelName),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsJSPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".js";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, JavaScript model is undefined.");
            }
        });

        d3.select("#btn_menuAdaPrinter").on("click", function () {
            var emucharts = {
                name: (emuchartsAdaPrinter.modelName),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsAdaPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.spec && model.body) {
                var overWrite = {overWrite: true};
                projectManager.project().addFile(emucharts.name + ".adb", model.body, overWrite);
                projectManager.project().addFile(emucharts.name + ".ads", model.spec, overWrite);
            } else {
                console.log("Warning, Ada model is undefined.");
            }
        });
        d3.select("#btn_menuBlessPrinter").on("click", function () {
            var emucharts = {
                name: (emuchartsBlessPrinter.modelName),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: {
                    input: emuchartsManager.getInputVariables(),
                    output: emuchartsManager.getOutputVariables(),
                    local: emuchartsManager.getLocalVariables()
                },
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsBlessPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.thread) {
                var overWrite = {overWrite: true};
                projectManager.project().addFile(emucharts.name + ".aadl", model.thread, overWrite);
            } else {
                console.log("Warning, Bless model is undefined.");
            }
        });        
        //-- Zoom menu -----------------------------------------------------------
        d3.select("#menuZoom").on("mouseover", function () {
            document.getElementById("menuZoom").children[1].style.display = "block";
        });
        d3.select("#btn_menuZoomIn").on("click", function () {
            emuchartsManager.zoom_in();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });
        d3.select("#btn_menuZoomOut").on("click", function () {
            emuchartsManager.zoom_out();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });
        d3.select("#btn_menuZoomReset").on("click", function () {
            emuchartsManager.zoom_reset();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });

        //--node filter handler
        d3.select("input#filter").on("keyup", function () {
            var editor = emuchartsManager.getSelectedEditor();
            if (editor) {
                editor._nodeFilter = d3.select("input#filter").property("value");
                emuchartsManager.render();
            }
        });

        //-- tables
        d3.select("#btnStates").on("click", function () {
            d3.select("#btnStates").attr("class", "active");
            d3.select("#btnTransitions").attr("class", "");
            d3.select("#btnVariables").attr("class", "");
            d3.select("#MachineStates").style("display", "block");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "none");
        });
        d3.select("#btnTransitions").on("click", function () {
            d3.select("#btnStates").attr("class", "");
            d3.select("#btnTransitions").attr("class", "active");
            d3.select("#btnVariables").attr("class", "");
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "block").classed("active");
            d3.select("#StateAttributes").style("display", "none");
        });
        d3.select("#btnVariables").on("click", function () {
            d3.select("#btnStates").attr("class", "");
            d3.select("#btnTransitions").attr("class", "");
            d3.select("#btnVariables").attr("class", "active");
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "block").classed("active");
        });
        d3.select("#btnViewHideTable").on("click", function () {
            d3.select("#EmuchartsFloatTable").style("top", "814px");
            d3.select("#btnViewHideTable").style("display", "none");
            d3.select("#btnViewRevealTable").style("display", "block");
        });
        d3.select("#btnViewRevealTable").on("click", function () {
            d3.select("#EmuchartsFloatTable").style("top", "614px");
            d3.select("#btnViewHideTable").style("display", "block");
            d3.select("#btnViewRevealTable").style("display", "none");
        });
        
        //-- PIM -----------------------------------------------------------------
        d3.select("#btn_toPIM").on("click", function () {
            if (emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is already a PIM.");
                return;
            }
            if (emuchartsManager.toPIM(true)) {
                console.log("Success, converted emuchart to a PIM.");
            }
            else {
                console.log("Warning, unable to convert emuchart to a PIM.");
            }
        });
        d3.select("#btn_fromPIM").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            if (emuchartsManager.toPIM(false)) {
                console.log("Success, converted emuchart from a PIM.");
            }
            else {
                console.log("Warning, unable to convert emuchart from a PIM.");
            }
        });
        d3.select("#btn_menuTestGenerator").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            var initTrans = emuchartsManager.getInitialTransitions();
            var emuchart = {
                name: ("emucharts_" + projectManager.project().name()),
                author: {
                    name: "<author name>",
                    affiliation: "<affiliation>",
                    contact: "<contact>"
                },
                //constants: emuchartsManager.getConstants(),
                //variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: initTrans,
                pm: {
                    name: projectManager.project().name(),
                    widgets: [],
                    components: emuchartsManager.getStates(),
                    pmr: []
                },
                start_state: initTrans ? initTrans[0].target.name : "",
                final_states: [],
                isPIM: emuchartsManager.getIsPIM()
            };

            var tests = pimTestGenerator.print(emuchart.name, { pims: [ emuchart ], pms: [] });
            if (tests.err) {
                console.log(tests.err);
                return;
            }
            if (tests.res) {
                var name = tests.file_name;
                var content = tests.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, TestGenerator model is undefined.");
            }
        });
        d3.select("#btn_menuTestGeneratorFromFile").on("click", function () {
            var models;
            // Generate tests from importing a file
            FileHandler.openLocalFileAsText(function (err, res) {
                if (res) {
                    // Try parse as PIM
                    models = pimImporter.importPIM(res);
                    if (models.err) {
                        console.log(models.err);
                        return;
                    }
                    // Remove file extension
                    var name = res.name.substr(0, res.name.lastIndexOf('.'));
                    var tests = pimTestGenerator.print(name, models.models);
                    if (tests.err) {
                        console.log(tests.err);
                        return;
                    }

                    if (tests.res) {
                        var testsName = tests.file_name;
                        var content = tests.res;
                        return projectManager.project().addFile(testsName, content, { overWrite: true });

                    } else {
                        console.log("Warning, TestGenerator model is undefined.");
                    }

                } else {
                    console.log("Error while opening file (" + err + ")");
                }

            }, { header: "Open PIM file..." });
        });
	};


    Emulink.prototype.getDependencies = function () {
        return [];
    };

    function onProjectChanged(event) {
        // try to open the default emuchart file associated with the project
        var defaultEmuchartFilePath = event.current + "/" + "emucharts_" + event.current + ".emdl";
        fs.readFile(defaultEmuchartFilePath).then(function (res) {
            res.content = JSON.parse(res.content);
            emuchartsManager.importEmucharts(res);
            // make svg visible and reset colors
            initToolbars();
            // render emuchart
            emuchartsManager.render();
            // set initial editor mode
            d3.select("#btn_toolbarBrowse").node().click();
            //set Variables Table
            contextTable.setContextVariables(emuchartsManager.getVariables());
            // set Machine States Table
            machineStatesTable.setMachineStates(emuchartsManager.getStates());
            // set Transitions Table
            transitionsTable.setTransitions(emuchartsManager.getTransitions());
        }).catch(function (err) {
            // if the default emuchart file is not in the project, then just clear the current diagram
            d3.select("#btnNewEmuchart").node().click();
        });
    }

    Emulink.prototype.initialise = function () {
//        //enable the plugin -- this should also enable any dependencies defined in getDependencies method
//        var prototypeBuilder = PrototypeBuilder.getInstance();
        // create local references to PVS editor, websocket client, and project manager
        editor = ModelEditor.getInstance().getEditor();
        ws = pvsioWebClient.getWebSocket();
        projectManager = ProjectManager.getInstance();
        // listen to ProjectChanged events so that we can update the editor when a new project is opened
        projectManager.addListener("ProjectChanged", onProjectChanged);
        // create user interface elements
        this.createHtmlElements();
        // try to load default emuchart for the current project
        onProjectChanged({current: projectManager.project().name()});
        return Promise.resolve(true);
    };

    Emulink.prototype.unload = function () {
        PVSioWebClient.getInstance().removeCollapsiblePanel(canvas);
        canvas = null;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new Emulink();
            }
            return instance;
        }
    };
});
