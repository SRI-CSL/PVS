/** @module EmuchartsFMIPVSPrinter */
/**
 * EmuchartsFMIPVSPrinter provides functions to generate Alloy models from Emucharts
 * @author Paolo Masci
 * @version 3.0
 * @date June 2, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    // var printer_version = "1.0";
    var GenericPrinter = require("plugins/emulink/models/EmuchartsGenericPrinter");
    var projectManager = require("project/ProjectManager").getInstance();
    var skeleton_cpp_template = require("text!plugins/emulink/models/fmi-pvs/templates/skeleton_cpp.handlebars");
    var fmu_h_template = require("text!plugins/emulink/models/fmi-pvs/templates/fmu_h.handlebars");
    var fmu_cpp_template = require("text!plugins/emulink/models/fmi-pvs/templates/fmu_cpp.handlebars");
    var modelDescription_xml_template = require("text!plugins/emulink/models/fmi-pvs/templates/modelDescription_xml.handlebars");
    var Makefile_template = require("text!plugins/emulink/models/fmi-pvs/templates/Makefile.handlebars");
    var fmi2Functions_h = require("text!plugins/emulink/models/fmi-pvs/lib/fmi2Functions.h");
    var fmi2FunctionTypes_h = require("text!plugins/emulink/models/fmi-pvs/lib/fmi2FunctionTypes.h");
    var fmi2TypesPlatform_h = require("text!plugins/emulink/models/fmi-pvs/lib/fmi2TypesPlatform.h");

    /**
     * Constructor
     */
    function EmuchartsFMIPVSPrinter(name) {
        this.module_name = name;
        this.genericPrinter = new GenericPrinter();
        return this;
    }

    var buffer_names = {
        bool: "booleanBuffer",
        int: "intBuffer",
        real: "realBuffer",
        string: "stringBuffer"
    };

    function get_buffer(t, count) {
        switch (t) {
            case "int" : count.int++;  return { printf_type: "%i", buffer_name: buffer_names.int, descriptor: "Int" };
            case "real": count.real++; return { printf_type: "%f", buffer_name: buffer_names.real, descriptor: "Real" };
            case "bool": count.bool++; return { printf_type: "%i", buffer_name: buffer_names.bool, descriptor: "Bool"};
            case "string": count.string++; return { printf_type: "%s", buffer_name: buffer_names.string, descriptor: "String"};
            default: return null;
        }
        return null;
    }

    /**
     * Prints the FMU package
     * When opt.interactive is true, a dialog is shown to the user to enter/select parameters.
     */
    EmuchartsFMIPVSPrinter.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        opt.interactive = false;
        var _this = this;
        function finalize(resolve, reject, par) {
            var count = {
                "int": 0,
                "bool": 0,
                "real": 0,
                "string": 0
            };
            var valueReference = {
                "int": 0,
                "bool": 0,
                "real": 0,
                "string": 0
            };
            var skeleton_cpp = "";
            var fmu_h = "";
            var fmu_cpp = "";
            var modelDescription_xml = "";
            var Makefile = "";
            var model = _this.genericPrinter.print(emuchart);
            if (model && model.state_variables && model.state_variables.variables
                    && model.state_variables.variables.length > 0) {
                // process the array of variables to add information necessary for the fmi
                model.state_variables.variables.forEach(function (v) {
                    v.fmi = get_buffer(v.type, count);
                    if (v.fmi) {
                        v.fmi.variability = "discrete";
                        v.fmi.causality = "input"; //TODO: this information needs to be provided by the emuchart model
                        v.fmi.valueReference = valueReference[v.type];
                        valueReference[v.type]++;
                    }
                });

                try {
                    skeleton_cpp = Handlebars.compile(skeleton_cpp_template, { noEscape: true })({
                        variables: model.state_variables.variables,
                        buffer_names: buffer_names
                    });

                    fmu_h = Handlebars.compile(fmu_h_template, { noEscape: true })({
                        variables: model.state_variables.variables,
                        buffer_names: buffer_names,
                        count: count
                    });

                    fmu_cpp = Handlebars.compile(fmu_cpp_template, { noEscape: true })({
                        variables: model.state_variables.variables,
                        buffer_names: buffer_names
                    });

                    modelDescription_xml = Handlebars.compile(modelDescription_xml_template, { noEscape: true })({
                        variables: model.state_variables.variables,
                        author: (emuchart.author) ? emuchart.author.name : "pvsioweb",
                        date: new Date().toString(),
                        modelName: emuchart.name,
                    });

                    Makefile = Handlebars.compile(Makefile_template, { noEscape: true })({
                        name: emuchart.name
                    });
                } catch(fmi_gen_err) {
                    console.error(fmi_gen_err);
                }
            }
            // var fmu_module = Handlebars.compile(alloy_module_template, { noEscape: true })({
            //     name: emuchart.name,
            //     modes: modes_declaration,
            //     init: init_function,
            //     variables: variables_declaration,
            //     triggers: triggers,
            //     disclaimer: disclaimer
            // });

            //-- write data to disk
            var overWrite = {overWrite: true};
            var folder = "fmu-pvs/";
            projectManager.project().addFile(folder + "skeleton.cpp", skeleton_cpp, overWrite);
            projectManager.project().addFile(folder + "fmu.h", fmu_h, overWrite);
            projectManager.project().addFile(folder + "fmu.cpp", fmu_cpp, overWrite);
            projectManager.project().addFile(folder + "modelDescription.xml", modelDescription_xml, overWrite);
            projectManager.project().addFile(folder + "Makefile", Makefile, overWrite);
            projectManager.project().addFile(folder + "/fmi/fmi2Functions.h", fmi2Functions_h, overWrite);
            projectManager.project().addFile(folder + "/fmi/fmi2FunctionTypes.h", fmi2FunctionTypes_h, overWrite);
            projectManager.project().addFile(folder + "/fmi/fmi2TypesPlatform.h", fmi2TypesPlatform_h, overWrite);
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

    module.exports = EmuchartsFMIPVSPrinter;
});
