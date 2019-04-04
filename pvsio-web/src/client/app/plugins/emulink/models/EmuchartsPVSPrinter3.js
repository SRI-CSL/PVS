/** @module EmuchartsPVSPrinter */
/**
 * EmuchartsPVSPrinter provides functions to generate PVS models from Emucharts
 * @author Paolo Masci
 * @version 3.0
 * @date June 2, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var printer_version = "3.0";
    var GenericPrinter = require("plugins/emulink/models/EmuchartsGenericPrinter");

    var projectManager = require("project/ProjectManager").getInstance();

    var leave_enter_function_template = require("text!plugins/emulink/models/pvs/templates/pvs_leave_enter_functions.handlebars"),
        init_function_template = require("text!plugins/emulink/models/pvs/templates/pvs_init_function.handlebars"),
        triggers_template = require("text!plugins/emulink/models/pvs/templates/pvs_transition_functions.handlebars"),
        enumerated_type_template = require("text!plugins/emulink/models/pvs/templates/pvs_enumerated_type.handlebars"),
        record_type_template = require("text!plugins/emulink/models/pvs/templates/pvs_record_type.handlebars"),
        constants_template = require("text!plugins/emulink/models/pvs/templates/pvs_constants_type.handlebars"),
        pvs_theory_template = require("text!plugins/emulink/models/pvs/templates/pvs_theory.handlebars"),
        pvs_descriptor_template = require("text!plugins/emulink/models/pvs/templates/pvs_descriptor.handlebars"),
        pvs_disclaimer_template = require("text!plugins/emulink/models/pvs/templates/pvs_disclaimer.handlebars"),
        pvs_utils_template = require("text!plugins/emulink/models/pvs/templates/pvs_utils.handlebars");

    var predefined_functions = { leave: "leave", enter: "enter" };

    /**
     * Constructor
     */
    function EmuchartsPVSPrinter(name) {
        this.theory_name = name;
        this.genericPrinter = new GenericPrinter();
        return this;
    }


    // This function "flattens" Object expressions into a string
    // and converts the name of operators in expressions -- needed for && || == != !
    function convert_expression(expr, emuchart, opt) {
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
                        if (term.val.indexOf(".") >= 0) {
                            var v = term.val.split(".");
                            v[0] += "(st)";
                            term.val = v.join("`");
                        } else {
                            term.val += "(st)";
                        }
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
                    if (term.val === "&&") {
                        term.val = "AND";
                    } else if (term.val === "||") {
                        term.val = "OR";
                    } else if (term.val === "==") {
                        term.val = "=";
                    } else if (term.val === "!=") {
                        term.val = "/=";
                    }
                } else if (term.type === "unaryop") {
                    if (term.val === "!") {
                        term.val = "NOT";
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
            if (term.isVariable) {
                term.type = "variable";
                // if (opt.attach_state) {
                    if (term.val.indexOf(".") >= 0) {
                        var v = term.val.split(".");
                        v[0] += "(st)";
                        term.val = v.join("`");
                    } else {
                        term.val += "(st)";
                    }
                // }
            } else {
                term = preProcess(term, emuchart);
            }
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }

    EmuchartsPVSPrinter.prototype.print_descriptor = function (emuchart) {
        return Handlebars.compile(pvs_descriptor_template, { noEscape: true })({
            name: emuchart.name,
            author: emuchart.author,
            description: emuchart.description
        });
    };

    EmuchartsPVSPrinter.prototype.print_disclaimer = function () {
        return Handlebars.compile(pvs_disclaimer_template, { noEscape: true })({
            printer_version: printer_version,
            www: "http://www.pvsioweb.org"
        });
    };


    /**
     * Prints the entire PVS theory
     * When opt.interactive is true, a dialog is shown to the user to select compilation parameters.
     */
    EmuchartsPVSPrinter.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        var _this = this;
        function finalize(resolve, reject) {
            var model = _this.genericPrinter.print(emuchart, {
                convert_expression: convert_expression
            });
            var pvsioweb_utils_theory = "pvsioweb_utils";

            var init_function = (model && model.init) ?
                        Handlebars.compile(init_function_template, { noEscape: true })({
                            comment: "init function",
                            name: model.init.name,
                            override: model.init.override,
                            variables: model.init.variables,
                            DEFAULT_INIT: model.init.DEFAULT_INIT,
                            INIT_WITH_OVERRIDES: model.init.INIT_WITH_OVERRIDES,
                            INIT_MULTI: model.init.INIT_MULTI
                        }) : "";
            var triggers = (model && model.triggers) ?
                        Handlebars.compile(triggers_template, { noEscape: true })({
                            comment: "triggers",
                            functions: model.triggers.functions,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                            full_coverage: true
                        }) : "";
            var enter_leave_functions = Handlebars.compile(leave_enter_function_template, { noEscape: true })({
                            comment: "enter/leave functions",
                            current_mode: model.enter_leave.current_mode,
                            previous_mode: model.enter_leave.previous_mode,
                            state_type: model.enter_leave.state_type,
                            entry_actions: model.enter_leave.entry_actions,
                            leave_actions: model.enter_leave.leave_actions,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                        });
            var modes_declaration = (model && model.modes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "operating modes",
                            enumtype: [ model.modes ]
                        }) : "";
            var variables_declaration = (model && model.state_variables) ?
                        Handlebars.compile(record_type_template, { noEscape: true })({
                            comment: "state attributes",
                            recordtype: [ model.state_variables ]
                        }) : "";
            var constants_declaration = (model && model.constants) ?
                        Handlebars.compile(constants_template, { noEscape: true })({
                            comment: "user-defined constants",
                            constants: model.constants
                        }) : "";
            var datatypes_declaration = (model && model.datatypes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "user-defined datatypes",
                            enumtype: [ model.datatypes ]
                        }) : "";
            var theory = Handlebars.compile(pvs_theory_template, { noEscape: true })({
                name: emuchart.name, // Note: it is important to have the theory name identical to the file name -- otherwise PVSio refuses to evaluate commands!
                importings: emuchart.importings.concat(pvsioweb_utils_theory),
                constants: constants_declaration,
                datatypes: datatypes_declaration,
                modes: modes_declaration,
                variables: variables_declaration,
                init: init_function,
                triggers: triggers,
                enter_leave: enter_leave_functions
            });

            var model_extras = {
                name: pvsioweb_utils_theory, // Note: it is important to have the theory name identical to the file name -- otherwise PVSio refuses to evaluate commands!
                extras: Handlebars.compile(pvs_utils_template, { noEscape: true })()
            };
            var extras = Handlebars.compile(pvs_theory_template, { noEscape: true })(model_extras);

            //-- write data to disk
            var overWrite = {overWrite: true};
            var folder = "pvs/";
            projectManager.project().addFile(folder + emuchart.name + ".pvs", theory, overWrite);
            projectManager.project().addFile(folder + pvsioweb_utils_theory + ".pvs", extras, overWrite);
            resolve(true);
        }
        return new Promise (function (resolve, reject) {
            if (opt.interactive) {
                return _this.genericPrinter.get_params().then(function (par) {
                    finalize(resolve, reject);
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            }
            return finalize(resolve, reject);
        });
    };

    module.exports = EmuchartsPVSPrinter;
});
