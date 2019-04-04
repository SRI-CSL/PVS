/**
 * @author Paolo Masci
 * @date Dec 5, 2017
 *
 * JavaScript code printer for emucharts models.
 * The print function is the main API for model generation. The function uses:
 *  - Generic Printer to create the data structures necessary for code generation.
 *  - convert_expression to convert emucharts operators into JavaScript operators
 *
 */
define(function (require, exports, module) {
    "use strict";
    var printer_version = "2.0";

    var GenericPrinter = require("plugins/emulink/models/EmuchartsGenericPrinter");
    var projectManager = require("project/ProjectManager").getInstance();

    var descriptor_template = require("text!plugins/emulink/models/javascript/templates/JS_descriptor.handlebars");
    var disclaimer_template = require("text!plugins/emulink/models/javascript/templates/JS_disclaimer.handlebars");

    var leave_enter_function_template = require("text!plugins/emulink/models/javascript/templates/JS_leave_enter_functions.handlebars");
    var init_function_template = require("text!plugins/emulink/models/javascript/templates/JS_init_function.handlebars");
    var enumerated_type_template = require("text!plugins/emulink/models/javascript/templates/JS_enumerated_type.handlebars");
    var constants_template = require("text!plugins/emulink/models/javascript/templates/JS_constants.handlebars");

    var triggers_template = require("text!plugins/emulink/models/javascript/templates/JS_transition_functions.handlebars");
    var body_template = require("text!plugins/emulink/models/javascript/templates/JS_body.handlebars");

    var main_template = require("text!plugins/emulink/models/javascript/templates/JS_main.handlebars");

    var predefined_functions = { leave: "leave", enter: "enter" };

    var basicTypes = {
        "bool"  : 1,
        "short" : 2,
        "int"   : 3,
        "long"  : 4,
        "float" : 5,
        "double": 6,
        //--
        "real"  : 7,
        "nat"   : 8,
        "string": 9
    };

    /**
     * Set a number with the properly value's suffix, useful for parsing declaration's variable, with respect to MISRA 1998 rule (Rule 18, advisory)
     * Parameter is current value
     */
    function getSuffix(value, type) {
        if (type) {
            if (type === "nat") {
                return "u";
            } else if (type === "float" || type === "double" || type === "real") {
                if (value) {
                    return (value.indexOf(".") < 0) ? ".0f" : "f";
                }
            }
        }
        return "";
    }

    /**
     * Constructor
     */
    function Printer(name) {
        this.modelName = name;
        this.genericPrinter = new GenericPrinter();
        this.model = { modelName: name, triggers: [] };
    }

    // This function "flattens" Object expressions into a string
    // and converts emucharts operators into C operators (e.g., needed for AND, OR, etc)
    function convert_expression(expr, emuchart, opt) {
        function preProcess (term, emuchart, opt) {
            function preprocessFunction(term, emuchart) {
                opt = opt || {};
                if (term.type === "function") {
                    var ans = "";
                    for (var i = 0; i < term.val.length; i++) {
                        // the first element in the array is the fuction name
                        ans += preprocessFunction(term.val[i], emuchart);
                    }
                    return ans;
                } else if (term.type === "identifier") {
                    if (term.isVariable) {
                        term.val = "st." + term.val;
                        // carry on the type while parsing the expression -- useful to understand the type of the variable later on
                        opt.variable_type = term.variableType;
                    }
                    return term.val;
                } else if (term.type === "number") {
                    return (term.val + getSuffix(term.val, opt.variable_type));
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
                        term.val = "||";
                    } else if (term.val === "=") {
                        term.val = "===";
                    }
                } else if (term.type === "unaryop") {
                    if (term.val === "NOT") {
                        term.val = "!";
                    }
                } else if (term.type === "number") {
                    term.val += getSuffix(term.val, opt.variable_type);
                } else if (term.type === "function") {
                    term.val = preprocessFunction(term, emuchart);
                }
            }
            return term;
        }
        opt = opt || {};
        var tmp = [];
        expr.forEach(function (term) {
            if (term.isVariable) {
                term.val = "st." + term.val;
                // carry on the type while parsing the expression -- useful to understand the type of the variable later on
                opt.variable_type = term.variableType;
            } else if (term.type === "identifier") {
                term.isModeAttribute = (emuchart.states.filter(function (st) { return st.name === term.val; }).length > 0);
                if (term.isModeAttribute) {
                    var modeType = opt.modeType || "Mode";
                    term.val = modeType + "." + term.val;
                } else {
                    var dataType = null;
                    emuchart.datatypes.forEach(function (dt) {
                        if (dt.constructors.filter(function (c) { return c === term.val; }).length > 0) {
                            dataType = dt.name;
                        }
                    });
                    if (dataType && !basicTypes[dataType]) {
                        term.val = dataType + "." + term.val;
                    }
                }
            } else {
                term = preProcess(term, emuchart, opt);
            }
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }

    // this function prints a dummy main file suitable for testing the generated code
    Printer.prototype.print_main = function (emuchart) {
        var data = {};
        data.triggers = this.genericPrinter.get_triggers(emuchart);
        if (data.triggers && data.triggers.functions) {
            var i = 0; // this is used to associate an index to each trigger
            data.triggers = data.triggers.functions.map(function (f) { return { name: f.name, id: i++ }; });
        }
        data.modes = this.genericPrinter.get_modes(emuchart);
        var ans = Handlebars.compile(main_template, { noEscape: true })({
            functions: data.triggers,
            modes: data.modes.modes,
            filename: emuchart.name,
            version: printer_version
        });
        return ans;
    };

    // This is the main API.
    // TODO: When opt.interactive is true, a dialog is shown to the user to select compilation parameters.
    Printer.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        var _this = this;
        function finalize(resolve, reject, opt) {
            var model = _this.genericPrinter.print(emuchart, {
                convert_expression: convert_expression,
            });

            var modes_declaration = (model && model.modes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "operating modes",
                            enumtype: [ model.modes ]
                        }) : "";
            var datatypes_declaration = (model && model.datatypes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "user-defined datatypes",
                            enumtype: [ model.datatypes ]
                        }) : "";
            var constants = (model && model.constants) ?
                        Handlebars.compile(constants_template, { noEscape: true })({
                            comment: "user-defined constants",
                            constants: model.constants
                        }) : "";
            var init_function = (model && model.init) ?
                        Handlebars.compile(init_function_template, { noEscape: true })({
                            comment: "init function",
                            module: emuchart.name,
                            name: model.init.name,
                            override: model.init.override,
                            variables: model.init.variables.map(function (v) {
                                if (!basicTypes[v.type]) {
                                    v.userDefinedType = true;
                                }
                                return v;
                            })
                        }) : "";
            var triggers = (model && model.triggers) ?
                        Handlebars.compile(triggers_template, { noEscape: true })({
                            comment: "triggers",
                            module: emuchart.name,
                            functions: model.triggers.functions,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                            modeType: "Mode", //FIXME
                            full_coverage: true
                        }) : "";
            var enter_leave_functions = Handlebars.compile(leave_enter_function_template, { noEscape: true })({
                            comment: "enter/leave functions",
                            current_mode: model.enter_leave.current_mode,
                            previous_mode: model.enter_leave.previous_mode,
                            state_type: model.enter_leave.state_type,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                        });
            var descriptor = Handlebars.compile(descriptor_template, { noEscape: true})({
                            date: (new Date()).toLocaleDateString(),
                            author: {
                                name: "Paolo Masci",
                                affiliation: "INESC TEC and Universidade do Minho, Campus de Gualtar, Braga, Portugal",
                                contact: "paolo.masci@inesctec.pt"
                            },
                            description: opt.description || ("State machine for " + emuchart.name),
                            example: emuchart.name + ".init(); // this command initialises the state machine"
            });
            var disclaimer = Handlebars.compile(disclaimer_template, { noEscape: true})({
                printer_version: printer_version,
                www: "http://www.pvsioweb.org"
            });
            var body = Handlebars.compile(body_template, { noEscape: true })({
                module: emuchart.name,
                constants: constants,
                datatypes: datatypes_declaration,
                modes: modes_declaration,
                init: init_function,
                triggers: triggers,
                enter_leave: enter_leave_functions,
                descriptor: descriptor,
                disclaimer: disclaimer
            });

            var main = Handlebars.compile(main_template, { noEscape: true })({
                module: emuchart.name,
                includes: [ emuchart.name + ".js" ]
            });

            //-- finally, write everything to disk, in subfolder /misraC of the current project
            var overWrite = {overWrite: true};
            var folder = "/JS";
            projectManager.project().addFile(folder + "/" + emuchart.name + ".js", body, overWrite);

            projectManager.project().addFile(folder + "/index.html", main, overWrite);
            resolve(true);
        }
        return new Promise (function (resolve, reject) {
            if (opt.interactive) {
                return _this.genericPrinter.get_params().then(function (par) {
                    opt.wordsize = "64";
                    finalize(resolve, reject, opt);
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            }
            return finalize(resolve, reject, opt);
        });
    };

    module.exports = Printer;
});
