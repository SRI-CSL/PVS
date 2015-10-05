/** @module EmuchartsPIMPrinter */
/**
 * EmuchartsPIMPrinter provides functions to generate PIM models from Emucharts
 * @author Paolo Masci
 * @date 2014/09/12 10:58:21 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var PIM_model;

    /**
     * Constructor
     */
    function EmuchartsPIMPrinter(name) {
        PIM_model = name;
        return this;
    }

    /**
     * Parser for identifying transition names for transitions given in the
     * form name [ condition ] { actios }, where conditions and actions are optionals
     */
    function parseTransitionName(transitionName) {
        var pos = transitionName.indexOf("[");
        if (pos > 0) { return transitionName.substr(0, pos).trim(); }
        pos = transitionName.indexOf("{");
        if (pos > 0) { return transitionName.substr(0, pos).trim(); }
        return transitionName.trim();
    }

    /**
     * Parser for transitions given in the form name [ condition ] { actios },
     * where name is the transition name, and conditions and actions are optionals
     */
    function parseTransition(transition) {
        var ans = {
            name: "",
            cond: "",
            actions: [],
            from: transition.source.name,
            to: transition.target.name
        };
        var sqOpen = transition.name.indexOf("[");
        var sqClose = transition.name.indexOf("]");
        var curOpen = transition.name.indexOf("{");
        var curClose = transition.name.indexOf("}");
        ans.name = parseTransitionName(transition.name);
        if (sqOpen >= 0 && sqClose > sqOpen) {
            ans.cond = transition.name.substring(sqOpen + 1, sqClose).trim();
        }
        if (curOpen >= 0 && curClose > curOpen) {
            var actions = transition.name.substring(curOpen + 1, curClose).split(";");
            actions.forEach(function (action) {
                var a = action.trim();
                if (a !== "") {
                    ans.actions.push(a);
                }
            });

        }
        return ans;
    }

    /**
     * Prints PIM type definitions
     */
    EmuchartsPIMPrinter.prototype.print_types = function (emuchart) {
        var ans = "\n\\begin{zed}";
        if (emuchart && emuchart.states && emuchart.states.length > 0) {
            ans += "\n  State ::= ";
            emuchart.states.forEach(function (state) {
                ans += state.name + " | ";
            });
            ans = ans.substr(0, ans.lastIndexOf("|"));
            if (emuchart.transitions && emuchart.transitions.length > 0) {
                ans += "\\" + "\\";
            }
        }
        if (emuchart.transitions && emuchart.transitions.length > 0) {
            ans += "\n  Signal ::= ";
            emuchart.transitions.forEach(function (transition) {
                ans += parseTransition(transition).name.replace(new RegExp("_", "g"), "\\_") + " | ";
            });
            ans = ans.substr(0, ans.lastIndexOf("|"));
        }
        ans += "\n\\end{zed}\n";
        return ans;
    };

    /**
     * Prints PIM state schema
     */
    EmuchartsPIMPrinter.prototype.print_state_schema = function (emuchart) {
        var ans = "\n\\begin{schema}{PIMSystem}";
        ans += "\n  currentState : State";
        ans += "\n\\end{schema}\n";

        var initial_transitions = emuchart.initial_transitions;
        if (initial_transitions && initial_transitions.length > 0) {
            ans += "\n\\begin{schema}{Init}";
            ans += "\n  PIMSystem";
            ans += "\n\\where";
            ans += "\n  currentState = " + initial_transitions[0].target.name;
            ans += "\n\\end{schema}\n";
        }
        return ans;
    };

    /**
     * Prints PIM operation schema for Emuchart transitions given in the form transition [condition] {actions}
     */
    EmuchartsPIMPrinter.prototype.print_operation_schema = function (emuchart) {
        var transitions = emuchart.transitions;
        var ans = "";
        if (transitions && transitions.length > 0) {
            emuchart.transitions.forEach(function (transition) {
                var t = parseTransition(transition);
                ans += "\n\\begin{schema}{Transition" + t.from + t.to + "}";
                ans += "\n  \\Delta PIMSystem" + "\\" + "\\";
                ans += "\n  i? : Signal";
                ans += "\n\\where";
                ans += "\n  i? = " + t.name.replace(new RegExp("_", "g"), "\\_") + " \\" + "\\";
                ans += "\n  currentState = " + t.from + " \\" + "\\";
                ans += "\n  currentState' = " + t.to;
                ans += "\n\\end{schema}\n";
            });
        }
        return ans;
    };

    EmuchartsPIMPrinter.prototype.print_descriptor = function (emuchart) {
        var ans = "% --------------------------------------------------------------" +
                    "\n%  PIM: " + emuchart.name;
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

    EmuchartsPIMPrinter.prototype.print_disclaimer = function () {
        var ans = "\n% ---------------------------------------------------------------\n" +
                    "%  PIM model generated using PVSio-web PIMPrinter ver 0.1\n" +
                    "%  Tool freely available at http://www.pvsioweb.org" +
                    "\n% ---------------------------------------------------------------\n";
        return ans;
    };

    /**
     * Prints the PIM model
     */
    EmuchartsPIMPrinter.prototype.print = function (emuchart) {
        var ans = this.print_descriptor(emuchart);
        ans += this.print_types(emuchart);
        ans += this.print_state_schema(emuchart);
        ans += this.print_operation_schema(emuchart);
        ans += this.print_disclaimer();
        return { res: ans };
    };

    module.exports = EmuchartsPIMPrinter;
});
