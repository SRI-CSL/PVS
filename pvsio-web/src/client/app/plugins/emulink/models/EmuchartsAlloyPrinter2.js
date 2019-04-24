/** @module EmuchartsAlloyPrinter */
/**
 * EmuchartsAlloyPrinter provides functions to generate Alloy models from Emucharts
 * @author Paolo Masci
 * @version 3.0
 * @date June 2, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var printer_version = "2.0";
    var GenericPrinter = require("plugins/emulink/models/EmuchartsGenericPrinter");

    var projectManager = require("project/ProjectManager").getInstance();

    var alloy_module_template = require("text!plugins/emulink/models/alloy/templates/alloy_module.handlebars"),
        alloy_enumerated_type_template = require("text!plugins/emulink/models/alloy/templates/alloy_enumerated_type.handlebars"),
        alloy_init_function_template = require("text!plugins/emulink/models/alloy/templates/alloy_init_function.handlebars"),
        alloy_predicates_template = require("text!plugins/emulink/models/alloy/templates/alloy_predicates.handlebars"),
        alloy_record_type_template = require("text!plugins/emulink/models/alloy/templates/alloy_record_type.handlebars");

    var alloy_disclaimer_template = require("text!plugins/emulink/models/alloy/templates/alloy_disclaimer.handlebars");

    var mode_attribute = "mde";

    /**
     * Constructor
     */
    function EmuchartsAlloyPrinter(name) {
        this.module_name = name;
        this.genericPrinter = new GenericPrinter();
        return this;
    }


    // This function "flattens" Object expressions into a string
    // and converts the name of operators when necessary
    // TODO: move this function to GenericPrinter
    function convert_expression(expr, emuchart, opt) {
        function preProcess (term, emuchart) {
            function preprocessFunction(term, emuchart) {
                if (term.type === "function") {
                    var ans = "";
                    for (var i = 0; i < term.val.length; i++) {
                        ans += preprocessFunction(term.val[i], emuchart);
                    }
                    return ans;
                } else if (term.type === "binop") {
                    return " " + term.val + " ";
                } else if (typeof term.val === "string") {
                    return term.val;
                }
                return term;
            }
            if (term) {
                if (term.type === "function") {
                    term.val = preprocessFunction(term, emuchart);
                }
            }
            return term;
        }
        opt = opt || [];
        var tmp = [];
        expr.forEach(function (term) {
                term = preProcess(term, emuchart);
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }

    function convert_condition(expr, emuchart, opt) {
        function preProcess (term, emuchart) {
            function preprocessFunction(term, emuchart) {
                if (term.type === "function") {
                    var ans = "";
                    for (var i = 0; i < term.val.length; i++) {
                        ans += preprocessFunction(term.val[i], emuchart);
                    }
                    return ans;
                } else if (term.type === "identifier") {
                    if (term.isVariable) {
                        return "st1." + term.val;
                    }
                    return term.val;
                } else if (term.type === "binop") {
                    return " " + term.val + " ";
                } else if (typeof term.val === "string") {
                    return term.val;
                }
                return term;
            }
            if (term) {
                if (term.type === "binop") {
                    if (term.val === "AND") {
                        term.val = "&&";
                    } else if (term.val === "OR") {
                        term.val = "AND";
                    } else if (term.val === "==") {
                        term.val = "=";
                    }
                } else if (term.type === "unaryop") {
                    if (term.val === "NOT") {
                        term.val = "!";
                    }
                } else if (term.type === "identifier") {
                    if (term.isVariable) {
                        term.val = "st1." + term.val;
                    }
                } else if (term.type === "function") {
                    term.val = preprocessFunction(term, emuchart);
                }
            }
            return term;
        }
        opt = opt || [];
        var tmp = [];
        expr.forEach(function (term) {
                term = preProcess(term, emuchart);
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }

    /**
     * Prints the entire alloy module
     * When opt.interactive is true, a dialog is shown to the user to select compilation parameters.
     */
    EmuchartsAlloyPrinter.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        opt.interactive = true;
        var _this = this;
        function finalize(resolve, reject, par) {
            var model = _this.genericPrinter.print(emuchart, {
                convert_expression: convert_expression,
                convert_condition: convert_condition
            });
            var init_function = (model && model.init) ?
                        Handlebars.compile(alloy_init_function_template, { noEscape: true })({
                            comment: "init function",
                            variables: model.init.variables
                        }) : "";
            var triggers = (model && model.triggers) ?
                        Handlebars.compile(alloy_predicates_template, { noEscape: true })({
                            comment: "triggers",
                            functions: model.triggers.functions,
                            current_mode: model.enter_leave.current_mode,
                            //previous_mode: model.enter_leave.previous_mode,
                            mode_attribute: mode_attribute,
                            state_type: model.enter_leave.state_type
                        }) : "";
            var variables_declaration = (model && model.state_variables) ?
                        Handlebars.compile(alloy_record_type_template, { noEscape: true })({
                            comment: "state attributes",
                            recordtype: [ model.state_variables ]
                        }) : "";
            var modes_declaration = (model && model.modes) ?
                        Handlebars.compile(alloy_enumerated_type_template, { noEscape: true })({
                            comment: "operating modes",
                            enumtype: [ model.modes ]
                        }) : "";
            var disclaimer = Handlebars.compile(alloy_disclaimer_template, { noEscape: true })({
                printer_version: printer_version,
                www: "http://www.pvsioweb.org"
            });
            var alloy_module = Handlebars.compile(alloy_module_template, { noEscape: true })({
                name: emuchart.name,
                modes: modes_declaration,
                init: init_function,
                variables: variables_declaration,
                triggers: triggers,
                disclaimer: disclaimer
            });

            //-- write data to disk
            var overWrite = {overWrite: true};
            var folder = "alloy/";
            projectManager.project().addFile(folder + emuchart.name + ".als", alloy_module, overWrite);
            resolve(true);
        }
        return new Promise (function (resolve, reject) {
            if (opt.interactive) {
                return _this.genericPrinter.get_params().then(function (par) {
                    finalize(resolve, reject, par);
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            }
            return finalize(resolve, reject);
        });
    };

    module.exports = EmuchartsAlloyPrinter;
});
