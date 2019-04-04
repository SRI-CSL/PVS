/**
 *
 * @author Paolo Masci
 * @date May 13, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher"),
        contextMenus = require("plugins/emulink/menus/ContextMenus").getInstance(),
        emuchartsManager = require("plugins/emulink/EmuchartsManager").getInstance(),
        PIMEmulink = require("plugins/emulink/models/pim/PIMEmulink");

    var displayEditVariable = require("plugins/emulink/forms/displayEditVariable"),
        displayEditConstant = require("plugins/emulink/forms/displayEditConstant"),
        displayEditDatatype = require("plugins/emulink/forms/displayEditDatatype"),
        displaySelectState = require("plugins/emulink/forms/displaySelectState"),
        displaySelectTransition = require("plugins/emulink/forms/displaySelectTransition"),
        displayAddState = require("plugins/emulink/forms/displayAddState"),
        displayRenameState = require("plugins/emulink/forms/displayRenameState"),
        displayRename = require("plugins/emulink/forms/displayRename"),
        displayAddTransition = require("plugins/emulink/forms/displayAddTransition"),
        displayChangeStateColor= require("plugins/emulink/forms/displayChangeStateColor");

    var StateVariablesTable = require("plugins/emulink/tools/StateVariablesTable"),
        MachineStatesTable = require("plugins/emulink/tools/MachineStatesTable"),
        TransitionsTable = require("plugins/emulink/tools/TransitionsTable"),
        ConstantsTable = require("plugins/emulink/tools/ConstantsTable"),
        DatatypesTable = require("plugins/emulink/tools/DatatypesTable");

    var pimEmulink;
    var maxLen = 48; // max length of labels shown in dialogs, e.g., for transition labels

    var instance;
    function EmuchartsTables () {
        pimEmulink = new PIMEmulink(emuchartsManager);
        this.stateVariablesTable = new StateVariablesTable("EmuchartsFloatTableContent");
        this.machineStatesTable = new MachineStatesTable("EmuchartsFloatTableContent");
        this.transitionsTable = new TransitionsTable("EmuchartsFloatTableContent");
        this.constantsTable = new ConstantsTable("EmuchartsFloatTableContent");
        this.datatypesTable = new DatatypesTable("EmuchartsFloatTableContent");
        eventDispatcher(this);
        return this;
    }

    var minimized_table = { top: 814, height: 0 };
    var maximised_table = { top: 26, height: 800 };

    function installTableHandlers (_this) {
        if (document.getElementById("StateVariablesTable")) {
            _this.stateVariablesTable.addListener("StateVariablesTable_add", function(evt) {
                contextMenus.add_statevariable();
            });
            _this.stateVariablesTable.addListener("StateVariablesTable_delete", function(evt) {
                emuchartsManager.delete_variable(evt.id);
            });
            _this.stateVariablesTable.addListener("StateVariablesTable_edit", function(evt) {
                var theVariable = emuchartsManager.getVariable(evt.id);
                edit_variable(_this, theVariable);
            });
        }
        if (document.getElementById("MachineStatesTable")) {
            _this.machineStatesTable.addListener("MachineStatesTable_add", function(event) {
                add_machinestate();
            });
            _this.machineStatesTable.addListener("MachineStatesTable_delete", function(event) {
                emuchartsManager.delete_state(event.id);
            });
            _this.machineStatesTable.addListener("MachineStatesTable_edit", function(event) {
                var theState = emuchartsManager.getState(event.id);
                edit_machinestate(_this, theState);
            });
            // machineStatesTable.addListener("MachineStatesTable_changeColor", function(event) {
            //     var theState = emuchartsManager.getState(event.id);
            //     changeStateColor(theState);
            // });
        }
        if (document.getElementById("TransitionsTable")) {
            _this.transitionsTable.addListener("TransitionsTable_add", function(event) {
                add_transition();
            });
            _this.transitionsTable.addListener("TransitionsTable_delete", function(event) {
                emuchartsManager.delete_transition(event.id);
            });
            _this.transitionsTable.addListener("TransitionsTable_edit", function(event) {
                var theTransition = emuchartsManager.getTransition(event.id);
                if (theTransition) {
                    edit_transition(_this, theTransition);
                }
            });
            _this.transitionsTable.addListener("TransitionsTable_select", function(event) {
                var theTransition = emuchartsManager.getTransition(event.id);
                if (theTransition) {
                    emuchartsManager.select_transition(theTransition.id);
                }
            });
            _this.transitionsTable.addListener("TransitionsTable_deselectAll", function(event) {
                emuchartsManager.deselect_all_transition();
            });
        }
        if (document.getElementById("ConstantsTable")) {
            _this.constantsTable.addListener("ConstantsTable_add", function(event) {
                contextMenus.add_constant();
            });
            _this.constantsTable.addListener("ConstantsTable_delete", function(event) {
                emuchartsManager.delete_constant(event.id);
            });
            _this.constantsTable.addListener("ConstantsTable_edit", function(event) {
                var theConstant = emuchartsManager.getConstant(event.id);
                edit_constant(_this, theConstant);
            });
        }
        if (document.getElementById("DatatypesTable")) {
            _this.datatypesTable.addListener("DatatypesTable_add", function(event) {
                contextMenus.add_datatype();
            });
            _this.datatypesTable.addListener("DatatypesTable_delete", function(event) {
                emuchartsManager.delete_datatype(event.id);
            });
            _this.datatypesTable.addListener("DatatypesTable_edit", function(evt) {
                var theDatatype = emuchartsManager.getDatatype(evt.id);
                edit_datatype(_this, theDatatype);
            });
        }
        // handlers for resizing table
        d3.select("#btn_minimizeTable").on("click", function () {
            // d3.select("#EmuchartsFloatTable").transition().duration(200).style("top", minimized_table.top + "px");
            d3.selectAll(".EmuchartsTableContent").style("width", "100% ").transition().duration(200).style("height", minimized_table.height + "px");
        });
        var dragResize = d3.behavior.drag()
                            .origin(function () {
                                var origin = d3.select("#EmuchartsFloatTable").node().getBoundingClientRect();
                                console.log(origin);
                                return { x: 0, y: origin.top };
                            })
                            .on("drag", function () {
                                d3.event.sourceEvent.stopPropagation();
                                var m = d3.mouse(d3.select("#btn_resizeTableHeight").node());
                                // update table height
                                if (m && m.length > 1) {
                                    // var top = parseFloat(d3.select("#EmuchartsFloatTable").style("top")) + m[1];
                                    var height = parseFloat(d3.select(".EmuchartsTableContent").style("height")) - m[1];
                                    var max_height = parseFloat(d3.select("#ContainerStateMachine").style("height")) || maximised_table.height;
                                    // top = (top < maximised_table.top) ? maximised_table.top
                                    //         : (top > minimized_table.top) ? minimized_table.top
                                    //         : top;
                                    height = (height < minimized_table.height) ? minimized_table.height
                                            : (height > max_height) ? max_height
                                            : height;
                                    // d3.select("#EmuchartsFloatTable").style("top", top + "px");
                                    d3.selectAll(".EmuchartsTableContent").style("width", "100% ").style("height", height + "px");
                                }
                            });
        d3.select("#btn_resizeTableHeight").call(dragResize);

        // table selectors
        d3.select("#btnStates").on("click", function () {
            d3.select("#btnStates").classed("active", true);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStatesTable").style("display", "block");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateVariablesTable").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnTransitions").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", true);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStatesTable").style("display", "none");
            d3.select("#TransitionsTable").style("display", "block").classed("active");
            d3.select("#StateVariablesTable").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnVariables").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", true);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStatesTable").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateVariablesTable").style("display", "block").classed("active");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnConstants").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", true);
            d3.select("#btnDatatypes").classed("active", false);
            d3.select("#MachineStatesTable").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateVariablesTable").style("display", "none");
            d3.select("#ConstantsTable").style("display", "block").classed("active");
            d3.select("#DatatypesTable").style("display", "none");
        });
        d3.select("#btnDatatypes").on("click", function () {
            d3.select("#btnStates").classed("active", false);
            d3.select("#btnTransitions").classed("active", false);
            d3.select("#btnVariables").classed("active", false);
            d3.select("#btnConstants").classed("active", false);
            d3.select("#btnDatatypes").classed("active", true);
            d3.select("#MachineStatesTable").style("display", "none");
            d3.select("#TransitionsTable").style("display", "none");
            d3.select("#StateVariablesTable").style("display", "none");
            d3.select("#ConstantsTable").style("display", "none");
            d3.select("#DatatypesTable").style("display", "block").classed("active");
        });
    }

    EmuchartsTables.prototype.createHtmlElements = function () {
        this.stateVariablesTable.createHtmlElements();
        this.stateVariablesTable.createHtmlElements();
        this.machineStatesTable.createHtmlElements();
        this.transitionsTable.createHtmlElements();
        this.constantsTable.createHtmlElements();
        this.datatypesTable.createHtmlElements();
        installTableHandlers(this);
    };

    function edit_variable (_this, theVariable) {
        var variableScopes = emuchartsManager.getVariableScopes();
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
                _this.set({ statevariables: emuchartsManager.getVariables() });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }
    function edit_machinestate_aux(_this) {
        document.getElementById("menuStates").children[1].style.display = "none";
        var states = emuchartsManager.getStates();
        var labels = [];
        states.forEach(function (state) {
            labels.push(state.name + "  (id: " + state.id + ")");
        });
        displaySelectState.create({
            header: "Editing modes...",
            message: "Please select a mode",
            transitions: labels,
            buttons: ["Cancel", "Select"]
        }).on("select", function (e, view) {
            if (states.length > 0) {
                var v = e.data.options.get("selectedState");
                var theState = states[v];
                view.remove();
                edit_machinestate(_this, theState);
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
            return;
        });
    }
    function edit_transition_aux(_this) {
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
            header: "Editing triggers...",
            message: "Please select a trigger",
            transitions: labels,
            buttons: ["Cancel", "Select"]
        }).on("select", function (e, view) {
            if (transitions.length > 0) {
                var v = e.data.options.get("selectedTransition");
                var theTransition = transitions[v];
                view.remove();
                if (v < nTransitions) {
                    edit_transition(_this, theTransition);
                } else {
                    edit_initialtransition(_this, theTransition);
                }
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
            return;
        });
    }
    function edit_transition(_this, theTransition) {
        if (emuchartsManager.getIsPIM()) {
            pimEmulink.editTransition(theTransition);
            return;
        }
        displayRename.create({
            header: "Renaming trigger " + theTransition.name.substring(0, maxLen) + "...",
            required: false,
            currentLabel: theTransition.name, // this dialog will show just one transition
            buttons: ["Cancel", "Rename"]
        }).on("rename", function (e, view) {
            var transitionLabel = e.data.labels.get("newLabel");
            if (!transitionLabel) { transitionLabel = ""; }
            emuchartsManager.rename_transition(theTransition.id, transitionLabel);
            view.remove();
            _this.set({ transitions: emuchartsManager.getTransitions() });
        }).on("cancel", function (e, view) {
            // just remove rename window
            emuchartsManager.refresh_transition(theTransition.id, { color: "black" });
            view.remove();
        });
    }
    function edit_constant (_this, theConstant) {
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
                _this.set({ constants: emuchartsManager.getConstants() });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }
    function edit_datatype (_this, theDatatype) {
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
                _this.set({ datatypes: emuchartsManager.getDatatypes() });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }
    function edit_machinestate(_this, theState) {
        if (emuchartsManager.getIsPIM()) {
            return pimEmulink.editState(theState);
        }
        displayRenameState.create({
            header: "Renaming mode " + theState.name.substring(0, maxLen) + "...",
            textLabel: {
                newStateName: "Mode name",
                newStateColor: "Mode color",
                newStateEnter: "Mode entry actions",
                newStateExit:  "Mode exit actions"
            },
            curr_value: {
                newStateName: theState.name,
                newStateColor: theState.color,
                newStateEnter: theState.enter,
                newStateExit: theState.exit
            },
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newStateName = e.data.labels.get("newStateName");
            var newStateColor = e.data.labels.get("newStateColor");
            var newStateEnter = e.data.labels.get("newStateEnter");
            var newStateExit = e.data.labels.get("newStateExit");
            if (newStateName && newStateName !== "") {
                emuchartsManager.edit_state(
                    theState.id,
                    { name: newStateName,
                      color: newStateColor,
                      enter: newStateEnter,
                      exit: newStateExit }
                );
                view.remove();
                _this.set({ machinestates: emuchartsManager.getStates() });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }
    function edit_initialtransition(_this, theTransition) {
        displayRename.create({
            header: "Renaming initial trigger " + theTransition.name.substring(0, maxLen) + "...",
            required: false,
            currentLabel: theTransition.name,
            buttons: ["Cancel", "Rename"]
        }).on("rename", function (e, view) {
            var transitionLabel = e.data.labels.get("newLabel");
            if (!transitionLabel) { transitionLabel = ""; }
            emuchartsManager.rename_initial_transition(theTransition.id, transitionLabel);
            view.remove();
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    }
    function edit_machinestate_color(_this, theState) {
        if (emuchartsManager.getIsPIM()) {
            return pimEmulink.editState(theState);
        }
        displayChangeStateColor.create({
            header: "Renaming mode " + theState.name.substring(0, maxLen) + "...",
            textLabel: {
                newStateName: "Mode name",
                newStateColor: "Mode color",
                newStateEnter: "Mode entry actions",
                newStateExit:  "Mode exit actions"
            },
            curr_value: {
                newStateName: theState.name,
                newStateColor: theState.color,
                newStateEnter: theState.enter,
                newStateExit: theState.exit
            },
            buttons: ["Cancel", "Ok"]
        }).on("ok", function (e, view) {
            var newStateName = e.data.labels.get("newStateName");
            var newStateColor = e.data.labels.get("newStateColor");
            var newStateEnter = e.data.labels.get("newStateEnter");
            var newStateExit = e.data.labels.get("newStateExit");
            if (newStateColor && newStateColor !== "") {
                emuchartsManager.edit_state(
                    theState.id,
                    { name: newStateName,
                      color: newStateColor,
                      enter: newStateEnter,
                      exit: newStateExit }
                );
                view.remove();
                _this.set({ machinesstates: emuchartsManager.getStates() });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        });
    }

    function add_machinestate() {
        document.getElementById("menuStates").children[1].style.display = "none";
        var label = emuchartsManager.getFreshStateName();
        displayAddState.create({
            header: "Please enter a label for new mode",
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
    }
    function add_transition() {
        document.getElementById("menuTransitions").children[1].style.display = "none";
        var states = emuchartsManager.getStates();
        var labels = [];
        states.forEach(function (state) {
            labels.push(state.name + "  (id: " + state.id + ")");
        });
        displayAddTransition.create({
            header: "Please enter label for new trigger",
            textLabel: "New trigger",
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
    }

    EmuchartsTables.prototype.add = function (data) {
        if (data) {
            // if (data.statevariable) { return edit_variable(data.statevariable); }
            if (data.machinestate) { add_machinestate(); }
            if (data.transition) { add_transition(); }
            // if (data.constant) { return edit_constant(data.constant); }
            // if (data.datatype) { return edit_datatype(data.datatype); }
        }
        return this;
    };

    EmuchartsTables.prototype.edit = function (data) {
        if (data) {
            if (data.statevariable) { edit_variable(this, data.statevariable); }
            if (data.machinestate) {
                if (data.aux) {
                    edit_machinestate_aux(this);
                } else {
                    edit_machinestate(this, data.machinestate);
                }
            }
            if (data.machinestate_color) {
                edit_machinestate_color(this, data.machinestate_color);
            }
            if (data.transition) {
                if (data.aux) {
                    edit_transition_aux(this);
                } else {
                    edit_transition(this, data.transition);
                }
            }
            if (data.initialtransition) {
                edit_initialtransition(this, data.initialtransition);
            }
            if (data.constant) { edit_constant(this, data.constant); }
            if (data.datatype) { edit_datatype(this, data.datatype); }
        }
        return this;
    };

    EmuchartsTables.prototype.highlight = function (data) {
        if (data) {
            if (data.transition) {
                this.transitionsTable.select(data.transition);
            }
        }
        return this;
    };

    EmuchartsTables.prototype.select = function (data) {
        if (data) {
            if (data.transition) {
                this.transitionsTable.scrollTop(data.transition);
                this.transitionsTable.select(data.transition);
            }
        }
        return this;
    };

    EmuchartsTables.prototype.deselect = function (data) {
        if (data) {
            if (data.transition) {
                this.transitionsTable.deselect(data.transition);
            }
        }
        return this;
    };

    EmuchartsTables.prototype.set = function (data) {
        if (data) {
            if (data.statevariables) { this.stateVariablesTable.setElements(data.statevariables); }
            if (data.machinestates) { this.machineStatesTable.setElements(data.machinestates); }
            if (data.transitions) { this.transitionsTable.setElements(data.transitions); }
            if (data.constants) { this.constantsTable.setElements(data.constants); }
            if (data.datatypes) { this.datatypesTable.setElements(data.datatypes); }
        }
        return this;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new EmuchartsTables();
            }
            return instance;
        }
    };
});
