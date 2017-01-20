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
        PluginManager       = require("plugins/PluginManager"),
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
        displayAddDatatype     = require("plugins/emulink/forms/displayAddDatatype"),
        displayEditDatatype    = require("plugins/emulink/forms/displayEditDatatype"),
        displaySelectDatatype  = require("plugins/emulink/forms/displaySelectDatatype"),
        QuestionForm           = require("pvsioweb/forms/displayQuestion"),
        EmuchartsCodeGenerators = require("plugins/emulink/models/EmuchartsCodeGenerators"),
        ConsistencyTemplateView = require("plugins/emulink/tools/propertytemplates/ConsistencyTemplateView"),
        FeedbackTemplateView = require("plugins/emulink/tools/propertytemplates/FeedbackTemplateView"),
        ReversibilityTemplateView = require("plugins/emulink/tools/propertytemplates/ReversibilityTemplateView"),
//        EmuchartsTextEditor    = require("plugins/emulink/EmuchartsTextEditor"),
        EmuchartsParser        = require("plugins/emulink/EmuchartsParser"),
        pvsTheory              = require("text!./models/pvs/templates/pvsTheory.handlebars"),
        pvsFunctionWithInit    = require("text!./models/pvs/templates/pvsFunctionWithInit.handlebars"),
        pvsTheoremInduction    = require("text!./models/pvs/templates/pvsTheoremInduction.handlebars"),
        FileHandler            = require("filesystem/FileHandler"),
        FileSystem             = require("filesystem/FileSystem"),
        displayNotificationView  = require("plugins/emulink/forms/displayNotificationView"),
        PimTestGenerator       = require("plugins/emulink/models/pim/PIMTestGenerator"),
        PMTextGenerator        = require("plugins/emulink/models/pim/PMTextGenerator"),
        PIMImporter            = require("plugins/emulink/models/pim/PIMImporter"),
        PIMEmulink             = require("plugins/emulink/models/pim/PIMEmulink"),
        ContextTable           = require("plugins/emulink/tools/ContextTable"),
        MachineStatesTable     = require("plugins/emulink/tools/MachineStatesTable"),
        TransitionsTable       = require("plugins/emulink/tools/TransitionsTable"),
        ConstantsTable         = require("plugins/emulink/tools/ConstantsTable"),
        DatatypesTable         = require("plugins/emulink/tools/DatatypesTable"),
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
    var emuchartsCodeGenerators;

    var pimImporter;
    var pimTestGenerator;
    var pimEmulink;
    var contextTable;
    var machineStatesTable;
    var transitionsTable;
    var constantsTable;
    var datatypesTable;
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
            emuchartsManager.refresh_transition(t.id, { color: "black" });
            view.remove();
        });
    }

    function renameTransition_handler(event) {
        editTransition(event.edge);
    }

    function highlightTransition_handler(event) {
        transitionsTable.selectTransition(event.edge.id);
    }
    function selectTransition_handler(event) {
        transitionsTable.scrollTop(event.edge.id);
        transitionsTable.selectTransition(event.edge.id);
    }
    function deselectTransition_handler(event) {
        transitionsTable.deselectTransition(event.edge.id);
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
        machineStatesTable.setMachineStates(emuchartsManager.getStates());
    }
    function stateRemoved_handler(event) {
        machineStatesTable.setMachineStates(emuchartsManager.getStates());
    }
    function stateRenamed_handler(event) { }//print_theory(); print_node(); }
    function stateColorChanged_handler(event) { }//print_theory(); print_node(); }
    function transitionAdded_handler(event) {
        transitionsTable.setTransitions(emuchartsManager.getTransitions());
    }
    function transitionRemoved_handler(event) {
        transitionsTable.setTransitions(emuchartsManager.getTransitions());
    }
    function transitionRenamed_handler(event) { }//print_theory(); print_node(); }
    function initialTransitionAdded_handler(event) { }//console.log("initial transition added"); }//print_theory(); print_node(); }
    function initialTransitionRemoved_handler(event) { }//console.log("initial transition removed"); }//print_theory(); print_node(); }
    function initialTransitionRenamed_handler(event) { }//console.log("initial transition renamed"); }//print_theory(); print_node(); }
    function constantAdded_handler(event) {
        constantsTable.setConstants(emuchartsManager.getConstants());
    }
    function constantRemoved_handler(event) {
        constantsTable.setConstants(emuchartsManager.getConstants());
    }
    function datatypeAdded_handler(event) {
        datatypesTable.setDatatypes(emuchartsManager.getDatatypes());
    }
    function datatypeRemoved_handler(event) {
        datatypesTable.setDatatypes(emuchartsManager.getDatatypes());
    }
    function variableAdded_handler(event) {
        contextTable.setContextVariables(emuchartsManager.getVariables());
    }
    function variableRemoved_handler(event) {
        contextTable.setContextVariables(emuchartsManager.getVariables());
    }


    /**
     * Constructor
     * @memberof Emulink
     */
    function Emulink() {
        pvsioWebClient = PVSioWebClient.getInstance();
        emuchartsCodeGenerators = EmuchartsCodeGenerators.getInstance();
        MODE = new EditorModeUtils();
        emuchartsManager = EmuchartsManager.getInstance();
        emuchartsManager.addListener("emuCharts_editorModeChanged", modeChange_callback);
        emuchartsManager.addListener("emuCharts_addState", addState_handler);
//        emuchartsManager.addListener("emuCharts_d3ZoomTranslate", d3ZoomTranslate_handler);
        emuchartsManager.addListener("emuCharts_deleteTransition", deleteTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteInitialTransition", deleteInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteState", deleteState_handler);
        emuchartsManager.addListener("emuCharts_renameState", renameState_handler);
        emuchartsManager.addListener("emuCharts_changeStateColor", changeStateColor_handler);
        emuchartsManager.addListener("emuCharts_renameTransition", renameTransition_handler);
        emuchartsManager.addListener("emuCharts_highlightTransition", highlightTransition_handler);
        emuchartsManager.addListener("emuCharts_selectTransition", selectTransition_handler);
        emuchartsManager.addListener("emuCharts_deselectTransition", deselectTransition_handler);
        emuchartsManager.addListener("emuCharts_renameInitialTransition", renameInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_addTransition", addTransition_handler);
        emuchartsManager.addListener("emuCharts_addInitialTransition", addInitialTransition_handler);

        emuchartsManager.addListener("emuCharts_stateAdded", stateAdded_handler);
        emuchartsManager.addListener("emuCharts_stateRemoved", stateRemoved_handler);
        emuchartsManager.addListener("emuCharts_constantAdded", constantAdded_handler);
        emuchartsManager.addListener("emuCharts_constantRemoved", constantRemoved_handler);
        emuchartsManager.addListener("emuCharts_datatypeAdded", datatypeAdded_handler);
        emuchartsManager.addListener("emuCharts_datatypeRemoved", datatypeRemoved_handler);
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
        emuchartsManager.addListener("emuCharts_newEmuchartsLoaded", function (event) {
            // update tables
            contextTable.setContextVariables(emuchartsManager.getVariables());
            machineStatesTable.setMachineStates(emuchartsManager.getStates());
            transitionsTable.setTransitions(emuchartsManager.getTransitions());
            constantsTable.setConstants(emuchartsManager.getConstants());
            datatypesTable.setDatatypes(emuchartsManager.getDatatypes());
        });
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

    Emulink.prototype.getId = function () {
        return this.getName().replace(/\s/g, "");
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
    function editConstant (theConstant) {
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
                constantsTable.setConstants(emuchartsManager.getConstants());
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }

    function editDatatype (theDatatype) {
        displayEditDatatype.create({
            header: "Editing datatype " + theDatatype.name,
            textLabel: {
                newDatatypeName: "Datatype name",
                newDatatypeConstructor1: "Datatype constants",
                newDatatypeValue: "Initial value"
            },
            placeholder: {
                newDatatypeName: theDatatype.name,
                newDatatypeConstructor1: theDatatype.constructors.join(", "),
                newDatatypeValue: theDatatype.value
            },
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newDatatypeName = e.data.labels.get("newDatatypeName");
            var newDatatypeConstructor1 = e.data.labels.get("newDatatypeConstructor1");
            var newDatatypeValue = e.data.labels.get("newDatatypeValue");
            if (newDatatypeName && newDatatypeName.value !== ""
                    && newDatatypeConstructor1 && newDatatypeConstructor1.value !== "") {
                emuchartsManager.rename_datatype(
                    theDatatype.id,
                    {   name: newDatatypeName,
                        constructors: newDatatypeConstructor1.split(",").map(function (c) { return c.trim(); }),
                        value: newDatatypeValue   }
                );
                view.remove();
                datatypesTable.setDatatypes(emuchartsManager.getDatatypes());
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
            headerText: _this.getName(),
            showContent: true,
            owner: _this.getId()
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
                if (theTransition) {
                    editTransition(theTransition);
                }
            });
            transitionsTable.addListener("TransitionsTable_selectTransition", function(event) {
                var theTransition = emuchartsManager.getTransition(event.transition.id);
                if (theTransition) {
                    emuchartsManager.select_transition(theTransition.id);
                }
            });
            transitionsTable.addListener("TransitionsTable_deselectAllTransition", function(event) {
                emuchartsManager.deselect_all_transition();
            });
        }
        if (document.getElementById("ConstantsTable")) {
            constantsTable = new ConstantsTable();
            constantsTable.addListener("ConstantsTable_deleteConstant", function(event) {
                emuchartsManager.delete_constant(event.constant.id);
            });
            constantsTable.addListener("ConstantsTable_editConstant", function(event) {
                var theConstant = emuchartsManager.getConstant(event.constant.id);
                editConstant(theConstant);
            });
        }
        if (document.getElementById("DatatypesTable")) {
            datatypesTable = new DatatypesTable();
            datatypesTable.addListener("DatatypesTable_deleteDatatype", function(event) {
                emuchartsManager.delete_datatype(event.datatype.id);
            });
            datatypesTable.addListener("DatatypesTable_editDatatype", function(event) {
                var theDatatype = emuchartsManager.getDatatype(event.datatype.id);
                editDatatype(theDatatype);
            });
        }

        // bootstrap buttons
        function openChart(callback) {
            //FIXME! move this function to EmuchartsManager
            function doOpen(err, res) {
                if (res) {
                    if (res.name.lastIndexOf(".emdl") === res.name.length - 5) {
                        res.content = JSON.parse(res.content);
                        emuchartsManager.importEmucharts(res);
                    } else if (res.name.lastIndexOf(".muz") === res.name.length - 4) {
                        emuchartsManager.importPIMChart(res);
                    } else if (res.name.lastIndexOf(".pim") === res.name.length - 4) {
                        pimImporter.importPIM(res, emuchartsManager);
                    } else if (res.name.lastIndexOf(".xml") === res.name.length - 4) {
                        emuchartsManager.importUppaalV4(res);
                    } else {
                        err = "Unrecognised file extension (file: " + res.name + ")";
                        console.log(err);
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
                extensions: ".emdl,.muz,.pim,.xml"
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
                extensions: ".muz,.pim,.xml"
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
            // set Constants
            constantsTable.setConstants(emuchartsManager.getConstants());
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
        // bootstrap tooltip
        $('[data-toggle="tooltip"]').tooltip();


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
            // if (!emuchartsManager.empty_chart()) {
            //     QuestionForm.create({
            //         header: "Warning: unsaved changes will be discarded.",
            //         question: "Unsaved changes in the current chart will be discarded."
            //                     + "Would you like continue?",
            //         buttons: ["Cancel", "Ok"]
            //     }).on("ok", function (e, view) {
            //         emuchartsManager.delete_chart();
            //         document.getElementById("btnLoadEmuchart").click();
            //         view.remove();
            //     }).on("cancel", function (e, view) {
            //         view.remove();
            //     });
            // } else {
            //     emuchartsManager.delete_chart();
                document.getElementById("btnLoadEmuchart").click();
            // }
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
                        version: "1.4",
                        description: "emucharts model",
                        chart_name: ("emucharts_" + projectManager.project().name())
                    },
                    chart: {
                        states: emuchartsManager.getStates(),
                        transitions: emuchartsManager.getTransitions(),
                        initial_transitions: emuchartsManager.getInitialTransitions(),
                        datatypes: emuchartsManager.getDatatypes(),
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
        d3.select("#btn_menuLayOutStates").on("click", function () {
            var editor = emuchartsManager.getSelectedEditor();
            if (editor) {
                var trans = editor.getTransformation();
                // editor.layOutChart_nath();
                editor.layOutChart();
                emuchartsManager.render({ trans: trans });
            }
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

        d3.select("#btn_menuNewDatatype").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            displayAddDatatype.create({
                header: "Please enter new datatype...",
                textLabel: {
                    newDatatypeName: "Datatype name",
                    newDatatypeConstructor1: "Datatype constants",
                    newDatatypeValue: "Initial value" // initial value
                },
                placeholder: {
                    newDatatypeName: "Name, e.g., MultiDisplay",
                    newDatatypeConstructor1: "Comma-separated list of constants, e.g., RATE, VTBI",
                    newDatatypeValue: "Initial value, e.g., RATE"
                },
                buttons: ["Cancel", "Create datatype"]
            }).on("create_datatype", function (e, view) {
                var newDatatypeName = e.data.labels.get("newDatatypeName");
                var newDatatypeConstructor1 = e.data.labels.get("newDatatypeConstructor1");
                var newDatatypeValue = e.data.labels.get("newDatatypeValue");
                if (newDatatypeName && newDatatypeName.value !== ""
                        && newDatatypeConstructor1 && newDatatypeConstructor1.value !== "") {
                    emuchartsManager.add_datatype({
                        name: newDatatypeName,
                        constructors: newDatatypeConstructor1.split(",").map(function (c){ return c.trim(); }),
                        value: newDatatypeValue
                    });
                    view.remove();
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuEditDatatype").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the datatype that needs to be edited
            var datatypes = emuchartsManager.getDatatypes();
            var labels = [];
            var data = [];
            datatypes.forEach(function (datatype) {
                var l = datatype.name;
                labels.push(l);
                data.push(datatype);
            });
            displaySelectDatatype.create({
                header: "Edit datatype",
                message: "Please select a datatype",
                datatypes: labels,
                buttons: ["Cancel", "Select"]
            }).on("select", function (e, view) {
                if (datatypes.length > 0) {
                    var c = e.data.options.get("selectedDatatype");
                    var theDatatype = data[c];
                    view.remove();
                    editDatatype(theDatatype);
                }
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
                return;
            });
        });

        d3.select("#btn_menuDeleteDatatype").on("click", function () {
            document.getElementById("menuContext").children[1].style.display = "none";
            // step 1: ask to select the datatype that needs to be edited
            var datatypes = emuchartsManager.getDatatypes();
            var labels = [];
            var data = [];
            datatypes.forEach(function (datatype) {
                var l = datatype.name;
                labels.push(l);
                data.push(datatype);
            });
            displaySelectDatatype.create({
                header: "Delete datatype",
                message: "Please select a datatype",
                datatypes: labels,
                buttons: ["Cancel", "Delete Datatype"]
            }).on("delete_datatype", function (e, view) {
                if (datatypes.length > 0) {
                    var c = e.data.options.get("selectedDatatype");
                    var theDatatype = data[c];
                    view.remove();
                    emuchartsManager.delete_datatype(theDatatype.id);
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

        function printer_template(printer_name, file_extension) {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name().replace(/-/g, "_")),
                author: {
                    name: "xxxx",
                    affiliation: "xxxx",
                    contact: "xxx"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                datatypes: emuchartsManager.getDatatypes(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = printer_name.print(emucharts);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + file_extension;
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, " + file_extension.replace(".","") + " model is undefined.");
            }
        }

        d3.select("#btn_menuPVSPrinter").on("click", function () {
            printer_template(emuchartsCodeGenerators.emuchartsPVSPrinter, ".pvs");
        });
        d3.select("#btn_menuNuXMVPrinter").on("click", function () {
            printer_template(emuchartsCodeGenerators.emuchartsNuXMVPrinter, ".smv");
            // var emucharts = {
            //     name: ("emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_SMV"),
            //     author: {
            //         name: "<author name>",
            //         affiliation: "<affiliation>",
            //         contact: "<contact>"
            //     },
            //     importings: [],
            //     constants: emuchartsManager.getConstants(),
            //     variables: emuchartsManager.getVariables(),
            //     states: emuchartsManager.getStates(),
            //     transitions: emuchartsManager.getTransitions(),
            //     initial_transitions: emuchartsManager.getInitialTransitions()
            // };
            // var model = emuchartsCodeGenerators.emuchartsNuXMVPrinter.print(emucharts);
            // if (model.err) {
            //     console.log(model.err);
            //     return;
            // }
            // if (model.res) {
            //     var name = emucharts.name + ".smv";
            //     var content = model.res;
            //     return projectManager.project().addFile(name, content, { overWrite: true });
            // } else {
            //     console.log("Warning, NuXMV model is undefined.");
            // }
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
            var model = emuchartsCodeGenerators.emuchartsPIMPrinter.print(emucharts);
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
            var model = emuchartsCodeGenerators.emuchartsPIMPrinter.print(emucharts);
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
                console.log("Warning, C++ code is undefined.");
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
            var model = emuchartsCodeGenerators.emuchartsMALPrinter.print(emucharts);
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
            var model = emuchartsCodeGenerators.emuchartsVDMPrinter.print(emucharts);
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
                name: ("emucharts_" + projectManager.project().name() + "_JS"),
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
            var model = emuchartsCodeGenerators.emuchartsJSPrinter.print(emucharts);
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
                console.log("Warning, JavaScript code is undefined.");
            }
        });

        d3.select("#btn_menuAdaPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_ADA"),
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
            var model = emuchartsCodeGenerators.emuchartsAdaPrinter.print(emucharts);
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
                console.log("Warning, Ada code is undefined.");
            }
        });
        d3.select("#btn_menuBlessPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_Bless"),
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
            var model = emuchartsCodeGenerators.emuchartsBlessPrinter.print(emucharts);
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
        d3.select("#btn_menuMisraCPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_MisraC"),
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
            var model = emuchartsCodeGenerators.emuchartsMisraCPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.thread && model.header) {
                var overWrite = {overWrite: true};
                projectManager.project().addFile("Makefile", model.makefile, overWrite);
                projectManager.project().addFile("main.c", model.main, overWrite);
                projectManager.project().addFile(emucharts.name + ".c", model.thread, overWrite);
                projectManager.project().addFile(emucharts.name + ".h", model.header, overWrite);
                projectManager.project().addFile("Android_" + emucharts.name + ".c", model.Android_thread, overWrite);
                projectManager.project().addFile("Android_" + emucharts.name + ".h", model.Android_header, overWrite);

                projectManager.project().addFile("Doxyfile", model.doxygen, overWrite);
            } else {
                console.log("Warning, MisraC code is undefined.");
            }
        });

        //-- Verification menu ---------------------------------------------------
        d3.select("#btn_menuConsistencyOfActions").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            });
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = new EmuchartsParser();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            ConsistencyTemplateView.create({
                header: "Consistency of user actions",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_th";
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var pvs_theorem = Handlebars.compile(pvsTheoremInduction, { noEscape: true })({
                        name: "CONSISTENCY",
                        property: "consistency",
                        property_definition: e.data.get("pvs_property"),
                        trans: "trans",
                        State: "State"
                    });
                    var theTheory = Handlebars.compile(pvsTheory, { noEscape: true })(
                        { name: "consistency_th",
                          definitions: Handlebars.compile(pvsFunctionWithInit, { noEscape: true })({
                            functionName: "action",
                            transitions: transitions
                          }),
                          body: pvs_theorem,
                          importing: emucharts_theory_name });
                    projectManager.project().addFile("consistency_th.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("consistency_th.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuReversibilityOfActions").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            });
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = new EmuchartsParser();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            ReversibilityTemplateView.create({
                header: "Reversibility of user actions",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_th";
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var pvs_theorem = Handlebars.compile(pvsTheoremInduction, { noEscape: true })({
                        name: "REVERSIBILITY",
                        property: "reversibility",
                        property_definition: e.data.get("pvs_property"),
                        trans: "trans",
                        State: "State"
                    });
                    var theTheory = Handlebars.compile(pvsTheory, { noEscape: true })(
                        { name: "reversibility_th",
                          definitions: Handlebars.compile(pvsFunctionWithInit, { noEscape: true })({
                            functionName: "action",
                            transitions: transitions
                          }),
                          body: pvs_theorem,
                          importing: emucharts_theory_name });
                    projectManager.project().addFile("reversibility_th.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("reversibility_th.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuVisibilityOfModes").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            });
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = new EmuchartsParser();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            FeedbackTemplateView.create({
                header: "Visibility of modes",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_th";
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var pvs_theorem = Handlebars.compile(pvsTheoremInduction, { noEscape: true })({
                        name: "VISIBILITY",
                        property: "visibility",
                        property_definition: e.data.get("pvs_property"),
                        trans: "trans",
                        State: "State"
                    });
                    var theTheory = Handlebars.compile(pvsTheory, { noEscape: true })(
                        { name: "visibility_th",
                          definitions: Handlebars.compile(pvsFunctionWithInit, { noEscape: true })({
                            transitions: transitions
                          }),
                          body: pvs_theorem,
                          importing: emucharts_theory_name });
                    projectManager.project().addFile("visibility_th.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("visibility_th.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
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

        //-- Emuchart Selector  -----------------------------------------------------------
        d3.select("#btn_emucharts_1").on("click", function () {
            emuchartsManager.selectEmucharts("EMUCHART__0");
        });
        d3.select("#btn_emucharts_2").on("click", function () {
            emuchartsManager.selectEmucharts("EMUCHART__1");
        });
        d3.select("#btn_emucharts_3").on("click", function () {
            emuchartsManager.selectEmucharts("EMUCHART__2");
        });

        //-- tables
        // TODO: Just passing by, but this should really be generalised to a function
        d3.select("#btnStates").on("click", function () {
            d3.select("#btnStates").classed("active", true);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStates").style("display", "block");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnTransitions").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", true);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "block").classed("active");
            d3.select("#StateAttributes").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnVariables").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", true);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "block").classed("active");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnConstants").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", true);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "none");
            d3.select("#ConstantsTable").style("display", "block").classed("active");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnDatatypes").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", true);
            d3.select("#MachineStates").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateAttributes").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "block").classed("active");
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
        d3.select("#btn_menuPMTextGenerator").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            var emuchart = {
                pm: {
                    name: projectManager.project().name(),
                    widgets: [],
                    components: emuchartsManager.getStates(),
                    pmr: []
                }
            };

            var text = PMTextGenerator.print(("emucharts_" + projectManager.project().name()), emuchart);
            if (text.err) {
                console.log(text.err);
                return;
            }
            if (text.res) {
                var name = text.file_name;
                var content = text.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            }
        });
	};

    Emulink.prototype.getEmuchartsManager = function () {
        return emuchartsManager;
    };

    Emulink.prototype.getDependencies = function () {
        return [];
    };

    function onProjectChanged(event) {
        // try to open the default emuchart file associated with the project
        var defaultEmuchartFilePath = event.current + "/" + "emucharts_" + event.current + ".emdl";
        return fs.readFile(defaultEmuchartFilePath).then(function (res) {
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
            // set Constants Table
            constantsTable.setConstants(emuchartsManager.getConstants());
            // set Datatypes Table
            datatypesTable.setDatatypes(emuchartsManager.getDatatypes());
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
        return new Promise(function (resolve, reject) {
            // try to load default emuchart for the current project
            onProjectChanged({current: projectManager.project().name()})
                .then(function () {
                    resolve(true);
                });
        });
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
        },

        hasInstance: function () {
            return !!instance;
        }
    };
});
