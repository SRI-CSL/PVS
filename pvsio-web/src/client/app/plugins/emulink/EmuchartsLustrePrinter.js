/** @module EmuchartsLustrerinter */
/**
 * EmuchartsLustrePrinter provides functions to generate a Lustre model from emucharts
 * The printer knows only the theory name, all expressions within the theory are provided as function arguments
 * @author Paolo Masci
 * @date 27/05/14 9:38:13 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
/**
 * [[Description]]
 * @param   {[[Type]]} function (require [[Description]]
 * @param   {[[Type]]} exports           [[Description]]
 * @param   {Object}   module            [[Description]]
 * @returns {Object}   [[Description]]
 */
define(function (require, exports, module) {
    "use strict";

    var lustre_node;

    /**
     * Constructor
     */
    function EmuchartsLustrePrinter(name) {
        lustre_node = name;
        return this;
    }

    EmuchartsLustrePrinter.prototype.print_input_variables = function (emuchart) {
        var ans = "\n";
        if (emuchart && emuchart.variables && emuchart.variables.input
                && emuchart.variables.input.length > 0) {
            ans += " (";
            emuchart.variables.input.forEach(function (ivar) {
                ans += ivar.name + ": " + ivar.type + "; ";
            });
            ans = ans.trim() + ")\n";
        }
        return ans;
    };
    EmuchartsLustrePrinter.prototype.print_output_variables = function (emuchart) {
        var ans = "";
        if (emuchart && emuchart.variables && emuchart.variables.output
                && emuchart.variables.output.length > 0) {
            ans += "returns (";
            emuchart.variables.output.forEach(function (ovar) {
                ans += ovar.name + ": " + ovar.type + "; ";
            });
            ans = ans.trim() + ")\n";
        }
        return ans;
    };
    EmuchartsLustrePrinter.prototype.print_local_variables = function (emuchart) {
        // local variables
        var ans = "";
        if (emuchart && emuchart.variables && emuchart.variables.local
                && emuchart.variables.local.length > 0) {
            ans += "var\n";
            emuchart.variables.local.forEach(function (lvar) {
                ans += " " + lvar.name + ": " + lvar.type + ";\n";
            });
        }
        return ans;
    };
    EmuchartsLustrePrinter.prototype.print_transitions = function (emuchart) {
        // transitions
        var ans = "";
        if (emuchart && emuchart.transitions && emuchart.transitions.length > 0) {
            var transition = emuchart.transitions[0].name;
            var openCur = transition.indexOf("{");
            var closeCur = transition.lastIndexOf("}");
            ans = "let\n";
            ans += transition.substring(openCur + 1, closeCur)
                            .replace(/\:\=/g, "=").replace(/\;/g, ";\n");
            ans = ans.trim() + "\ntel\n";
        }
        return ans;
    };

    EmuchartsLustrePrinter.prototype.print_descriptor = function (emuchart) {
        var ans = "% --------------------------------------------------------------" +
                    "\n%  Node: " + emuchart.name;
        if (emuchart.author) {
            ans += "\n%  Author: " + emuchart.author.name +
                    "\n%          " + emuchart.author.affiliation +
                    "\n%          " + emuchart.author.contact;
        }
        if (emuchart.description) {
            ans += "\n% ---------------------------------------------------------------" +
                    "\n%  " + emuchart.description;
        }
        ans += "\n% ---------------------------------------------------------------\n";
        return ans;
    };

    EmuchartsLustrePrinter.prototype.print_disclaimer = function () {
        var ans = "\n% ---------------------------------------------------------------\n" +
                    "%  Lustre specification generated using PVSio-web LustrePrinter ver 0.1\n" +
                    "%  Tool freely available at http://www.pvsioweb.org" +
                    "\n% ---------------------------------------------------------------\n";
        return ans;
    };

    /**
     * Prints the entire Lustre spec
     */
    EmuchartsLustrePrinter.prototype.print = function (emuchart) {
        var ans = this.print_descriptor(emuchart);
        ans += "\nnode " + emuchart.name;
        ans += this.print_input_variables(emuchart);
        ans += this.print_output_variables(emuchart);
        ans += this.print_local_variables(emuchart);
        ans += this.print_transitions(emuchart);
        ans += this.print_disclaimer();
        return { res: ans };
    };

    module.exports = EmuchartsLustrePrinter;
});
