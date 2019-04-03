/**
 * @author Gioacchino Mauro, Paolo Masci
 * @date June 9, 2017
 *
 * MISRA C code printer for emucharts models.
 * The print function is the main API for model generation. The function uses:
 *  - Generic Printer to create the data structures necessary for code generation.
 *  - convert_expression to convert emucharts operators into C operators
 *  - convert_variable to convert emucharts types and values into MisraC-compliant types and values
 *  - convert_constant to convert emucharts constants and values into MisraC-compliant types and values
 *
 */
define(function (require, exports, module) {
    "use strict";
    var printer_version = "2.0";

    var GenericPrinter = require("plugins/emulink/models/EmuchartsGenericPrinter");
    var projectManager = require("project/ProjectManager").getInstance();

    var leave_enter_function_template = require("text!plugins/emulink/models/misraC/templates/misraC_leave_enter_functions.handlebars");
    var init_function_template = require("text!plugins/emulink/models/misraC/templates/misraC_init_function.handlebars");
    var enumerated_type_template = require("text!plugins/emulink/models/misraC/templates/misraC_enumerated_type.handlebars");
    var constants_template = require("text!plugins/emulink/models/misraC/templates/misraC_constants.handlebars");
    var basic_types_template = require("text!plugins/emulink/models/misraC/templates/misraC_basic_types.handlebars");

    var struct_type_template = require("text!plugins/emulink/models/misraC/templates/misraC_struct_type.handlebars");
    var triggers_template = require("text!plugins/emulink/models/misraC/templates/misraC_transition_functions.handlebars");

    var header_template = require("text!plugins/emulink/models/misraC/templates/misraC_header.handlebars");
    var body_template = require("text!plugins/emulink/models/misraC/templates/misraC_body.handlebars");
    var pvsioweb_utils_template = require("text!plugins/emulink/models/misraC/templates/misraC_pvsioweb_utils.handlebars");
    var makefile_template = require("text!plugins/emulink/models/misraC/templates/misraC_makefile.handlebars");
    var main_template = require("text!plugins/emulink/models/misraC/templates/misraC_main.handlebars");
    // var dbg_utils_template = require("text!plugins/emulink/models/misraC/templates/misraC_dbg_utils.handlebars");

    var predefined_functions = { leave: "leave", enter: "enter" };
    var pvsioweb_utils_filename = "pvsioweb_utils";

    var typesTable = {
        "bool"  : "UI_8",
        "short" : "I_16",
        "int"   : "I_32",
        "long"  : "I_64",
        "float" : "F_32",
        "double": "D_64",
        //--
        "real"  : "D_64",
        "nat"   : "UI_32"
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
                        term.val = "st->" + term.val;
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
                        term.val = "==";
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
                term.val = "st->" + term.val;
                // carry on the type while parsing the expression -- useful to understand the type of the variable later on
                opt.variable_type = term.variableType;
            } else {
                term = preProcess(term, emuchart, opt);
            }
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }

    // This function converts emucharts variables into MisraC-compliant types and values
    function convert_variable(v) {
        if (v) {
            if (v.type && typesTable[v.type]) {
                v.value += getSuffix(v.value, v.type);
                v.originalType = v.type;
                v.type = typesTable[v.type];
            }
        }
        return v;
    }

    // This function converts emucharts constants into MisraC-compliant types and values
    function convert_constant(c) {
        if (c) {
            if (c.type && typesTable[c.type]) {
                c.value += getSuffix(c.value, c.type);
                c.originalType = c.type;
                c.type = typesTable[c.type];
            }
        }
        return c;
    }

    // this function prints the makefile
    Printer.prototype.print_makefile = function (emuchart) {
        var ans = Handlebars.compile(makefile_template, { noEscape: true })({
            filename: emuchart.name,
            version: printer_version
        });
        return ans;
    };

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
                convert_variable: convert_variable,
                convert_constant: convert_constant
            });

            //-- these functions generate the content of the header file
            var init_function_declaration = (model && model.init) ?
                        Handlebars.compile(init_function_template, { noEscape: true })({
                            comment: "init function",
                            name: model.init.name,
                            override: model.init.override,
                            variables: model.init.variables,
                            is_header_file: true
                        }) : "";
            var triggers_declaration = (model && model.triggers) ?
                        Handlebars.compile(triggers_template, { noEscape: true })({
                            comment: "triggers",
                            functions: model.triggers.functions,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                            is_header_file: true
                        }) : "";
            var enter_leave_functions_declaration = Handlebars.compile(leave_enter_function_template, { noEscape: true })({
                            comment: "enter/leave functions",
                            current_mode: model.enter_leave.current_mode,
                            previous_mode: model.enter_leave.previous_mode,
                            state_type: model.enter_leave.state_type,
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                            is_header_file: true
                        });
            var modes_declaration = (model && model.modes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "operating modes",
                            enumtype: [ model.modes ]
                        }) : "";
            var variables_declaration = (model && model.state_variables) ?
                        Handlebars.compile(struct_type_template, { noEscape: true })({
                            comment: "state attributes",
                            recordtype: [ model.state_variables ]
                        }) : "";
            var datatypes_declaration = (model && model.datatypes) ?
                        Handlebars.compile(enumerated_type_template, { noEscape: true })({
                            comment: "user-defined datatypes",
                            enumtype: [ model.datatypes ]
                        }) : "";
            var header = Handlebars.compile(header_template, { noEscape: true })({
                model_name: emuchart.name,
                includes: emuchart.importings.concat(pvsioweb_utils_filename + ".h").concat("misraC_basic_types.h"),
                datatypes: datatypes_declaration,
                modes: modes_declaration,
                variables: variables_declaration,
                init: init_function_declaration,
                triggers: triggers_declaration,
                enter_leave: enter_leave_functions_declaration
            });

            //-- these functions generate the body
            var constants = (model && model.constants) ?
                        Handlebars.compile(constants_template, { noEscape: true })({
                            comment: "user-defined constants",
                            constants: model.constants
                        }) : "";
            var init_function = (model && model.init) ?
                        Handlebars.compile(init_function_template, { noEscape: true })({
                            comment: "init function",
                            name: model.init.name,
                            override: model.init.override,
                            variables: model.init.variables
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
                            enter: predefined_functions.enter,
                            leave: predefined_functions.leave,
                        });
            var body = Handlebars.compile(body_template, { noEscape: true })({
                includes: [ emuchart.name + ".h" ],
                constants: constants,
                init: init_function,
                triggers: triggers,
                enter_leave: enter_leave_functions
            });

            //-- these are utility functions
            var pvsioweb_utils_header = Handlebars.compile(pvsioweb_utils_template, { noEscape: true })({ is_header_file: true });
            var pvsioweb_utils_body = Handlebars.compile(pvsioweb_utils_template, { noEscape: true })({});

            //-- these are basic types definitions (bool, int, real -- renamed using misraC conventions)
            var basic_types = Handlebars.compile(basic_types_template, { noEscape: true })({});

            //-- this is the makefile & dummy main
            var makefile = _this.print_makefile(emuchart);
            var main = _this.print_main(emuchart);

            //-- finally, write everything to disk, in subfolder /misraC of the current project
            var overWrite = {overWrite: true};
            var folder = "/misraC";
            projectManager.project().addFile(folder + "/" + emuchart.name + ".h", header, overWrite);
            projectManager.project().addFile(folder + "/" + emuchart.name + ".c", body, overWrite);
            projectManager.project().addFile(folder + "/misraC_basic_types.h", basic_types, overWrite);

            projectManager.project().addFile(folder + "/" + pvsioweb_utils_filename + ".h", pvsioweb_utils_header, overWrite);
            projectManager.project().addFile(folder + "/" + pvsioweb_utils_filename + ".c", pvsioweb_utils_body, overWrite);

            projectManager.project().addFile(folder + "/Makefile", makefile, overWrite);
            projectManager.project().addFile(folder + "/main.c", main, overWrite);
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
