/**
 *
 * @author Paolo Masci
 * @date 25/05/14 6:39:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher         = require("util/eventDispatcher"),
        emuchartsManager        = require("plugins/emulink/EmuchartsManager").getInstance(),
        displayAddVariable      = require("plugins/emulink/forms/displayAddVariable"),
        displaySelectVariable   = require("plugins/emulink/forms/displaySelectVariable"),
        displayAddConstant      = require("plugins/emulink/forms/displayAddConstant"),
        displaySelectConstant   = require("plugins/emulink/forms/displaySelectConstant"),
        displayAddDatatype      = require("plugins/emulink/forms/displayAddDatatype"),
        displaySelectDatatype   = require("plugins/emulink/forms/displaySelectDatatype");

    var instance;

    function ContextMenus () {
        eventDispatcher(this);
    }

    ContextMenus.prototype.add_constant = function () {
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
    };
    ContextMenus.prototype.edit_constant = function () {
        var _this = this;
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
                _this.fire({
                    type: "ContextMenus.editConstant",
                    constant: theConstant
                });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
            return;
        });
    };
    ContextMenus.prototype.delete_constant = function () {
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
    };

    ContextMenus.prototype.add_statevariable = function () {
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
    };
    ContextMenus.prototype.edit_statevariable = function () {
        var _this = this;
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
                _this.fire({
                    type: "ContextMenus.editVariable",
                    variable: theVariable
                });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
            return;
        });
    };
    ContextMenus.prototype.delete_statevariable = function () {
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
    };

    ContextMenus.prototype.add_datatype = function () {
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
    };
    ContextMenus.prototype.edit_datatype = function () {
        var _this = this;
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
                _this.fire({
                    type: "ContextMenus.editDatatype",
                    datatype: theDatatype
                });
            }
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
            return;
        });
    };
    ContextMenus.prototype.delete_datatype = function () {
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
    };

    ContextMenus.prototype.createHtmlElements = function () {
        var _this = this;
        //-- Context menu -----------------------------------------------------------
        d3.select("#menuContext").on("mouseover", function () {
            document.getElementById("menuContext").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewVariable").on("click", function () {
            _this.add_statevariable();
        });
        d3.select("#btn_menuEditVariable").on("click", function () {
            _this.edit_statevariable();
        });
        d3.select("#btn_menuDeleteVariable").on("click", function () {
            _this.delete_statevariable();
        });
        d3.select("#btn_menuNewConstant").on("click", function () {
            _this.add_constant();
        });
        d3.select("#btn_menuEditConstant").on("click", function () {
            _this.edit_constant();
        });
        d3.select("#btn_menuDeleteConstant").on("click", function () {
            _this.delete_constant();
        });
        d3.select("#btn_menuNewDatatype").on("click", function () {
            _this.add_datatype();
        });
        d3.select("#btn_menuEditDatatype").on("click", function () {
            _this.edit_datatype();
        });
        d3.select("#btn_menuDeleteDatatype").on("click", function () {
            _this.delete_datatype();
        });
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new ContextMenus();
            }
            return instance;
        }
    };
});
