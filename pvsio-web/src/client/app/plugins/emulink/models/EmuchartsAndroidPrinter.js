/** @module EmuchartsPVSPrinter */
/**
 * EmuchartsPVSPrinter provides functions to generate PVS models from Emucharts
 * @author xxx
 * @date Feb 23, 2017
*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    //var printer_version = "0.1";

    var EmuchartsMisraCPrinter = require("plugins/emulink/models/EmuchartsMisraCPrinter");
    var projectManager = require("project/ProjectManager").getInstance();

    function print_JNI_code(_this, emuchart) {
        return new Promise(function (resolve, reject) {
            _this.emuchartsMisraCPrinter.print(emuchart).then(function (model) {
                // console.log(model);
                if (model.err) {
                    console.log(model.err);
                    reject(model.err);
                } else if (model.thread && model.header) {
                    var overWrite = {overWrite: true};
                    projectManager.project().addFile("Android_" + emuchart.name + ".c", model.Android_thread, overWrite);
                    projectManager.project().addFile("Android_" + emuchart.name + ".h", model.Android_header, overWrite);
                    resolve(true);
                } else {
                    console.log("Warning, MisraC code is undefined.");
                    reject(null);
                }
            }).catch(function (err) {
                console.log(err);
                reject(err);
            });
        }).catch(function (err) {
            console.log(err);
        });
    }

    function EmuchartsAndroidPrinter() {
        this.emuchartsMisraCPrinter = new EmuchartsMisraCPrinter("emucharts_Android");
        return this;
    }

    /**
     * Prints the entire PVS theory
     */
    EmuchartsAndroidPrinter.prototype.print = function (emuchart) {
        // -------- PUT HERE THE CODE FOR GENERATING THE ANDROID APP --------------
        // In the following you can find example code for processing the JSON file with the widgets definitions
        var widgets_JSON = projectManager.project().getWidgetDefinitionFile().content; // this is the widget file of the current project open in PVSio-web
        var widgets = JSON.parse(widgets_JSON); // here I'm using the JSON parser to create an object reflecting the structure of the JSON file
        console.log(widgets); // enable the javascript tools in the browser to see this output in the console
        var coords = []; // array coords contains the coordinates of the widgets
        for (var i = 0; i < widgets.regionDefs.length; i++) {
            if (widgets.regionDefs[i].coords) {
                var tmp = widgets.regionDefs[i].coords.split(",");
                var values = []; //
                for (var j = 0; j < tmp.length; j++) {
                    values.push(parseFloat(tmp[j]));
                }
                coords.push(values);
            }
        }
        console.log(coords);
        ///..

        //-------------------------------------------------------------------------
        var _this = this;
        return new Promise(function (resolve, reject) {
            print_JNI_code(_this, emuchart).then(function (res) {
                resolve(true);
            }).catch(function (err) {
                console.log(err);
                reject(err);
            });
        });
    };

    module.exports = EmuchartsAndroidPrinter;
});
