/** @module EmuchartsICOPrinter */
/**
 * EmuchartsICOPrinter provides functions for generating ICO models from Emucharts
 * @author Paolo Masci
 * @date Feb 25, 2017
*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    //var printer_version = "0.1";
    var projectManager = require("project/ProjectManager").getInstance();
    var icoTemplate = require("text!plugins/emulink/models/circus/templates/ico.handlebars");

    function write_ICO_file(filename, filecontent) {
        return new Promise(function (resolve, reject) {
            projectManager.project().addFile(filename, filecontent, { overWrite: true }).then(function (res) {
                resolve(true);
            }).catch(function (err) {
                reject(err);
            });
        }).catch(function (err) {
            console.log(err);
        });
    }

    function EmuchartsICOPrinter() {
        return this;
    }

    /**
     * Prints the ICO model
     */
    EmuchartsICOPrinter.prototype.print = function (emuchart) {
        emuchart.initial_transitions.forEach(function (it) {
            emuchart.states = emuchart.states.map(function (state) {
                state.initial_state = (state.id === it.target.id);
                return state;
            });
        });
        emuchart.states = emuchart.states.map(function (state) {
            state.x = parseInt(state.x);
            state.y = parseInt(state.y);
            return state;
        });
        emuchart.transitions = emuchart.transitions.map(function (transition) {
            transition.x = parseInt(transition.x);
            transition.y = parseInt(transition.y);
            transition.controlPoint = transition.controlPoint || {};
            transition.controlPoint = {
                x: parseInt(transition.controlPoint.x) || 100,
                y: parseInt(transition.controlPoint.y) || 100,
            };
            return transition;
        });
        emuchart.filename = emuchart.name + "_ICO";
        var filecontent = Handlebars.compile(icoTemplate, { noEscape: true })(emuchart);
        return write_ICO_file(emuchart.filename + ".xml", filecontent);
    };

    module.exports = EmuchartsICOPrinter;
});
