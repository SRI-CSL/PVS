/** @module EmuchartsCppPrinter */
/**
 * EmuchartsCppPrinter provides functions to generate C++ code from Emucharts
 * @author Paolo Masci
 * @date 2014/09/19 11:59:43 AM
 */
/*jshint unused:false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3 */
define(function (require, exports, module) {
    "use strict";

    var class_name;


    /**
     * Constructor
     */
    function EmuchartsCppPrinter(name) {
        class_name = name;
        return this;
    }

    /**
     * Prints C++ definitions for Emuchart states
     */
    EmuchartsCppPrinter.prototype.print_states = function (emuchart) {
        var states = emuchart.states, stateStr;
        var ans = "  // machine states\n  enum MachineState";
        if (states && states.length > 0) {
            ans += " { ";
            stateStr = states.map(function (s) {
                return s.name;
            }).joing(", ");

            ans = ans.concat(stateStr).concat(" };");
        }
        return ans + "\n";
    };

    /**
     * Parser for identifying transition names for transitions given in the
     * form name [ condition ] { actios }, where conditions and actions are optionals
     */
    function parseTransitionName(transition) {
        var pos = transition.indexOf("[");
        if (pos > 0) { return transition.substr(0, pos).trim(); }
        pos = transition.indexOf("{");
        if (pos > 0) { return transition.substr(0, pos).trim(); }
        return transition.trim();
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
     * Utility function for printing PVS definitions of transition actions
     */
    function printAction(action, variables) {
        var tmp = "";
        // in this version of the tool we can identify state variables in
        // state updates, i.e., in actions given in the form v := expr,
        // where v is a state variable
        if (variables) {
/*            variables.forEach(function (v) {
                var state_update = action.split(":=");
                if (state_update.length === 2) {
                    // check for the presence of state variables
                    // on the right-hand side of the expression
                    var pos = indexOfStateVariableRT(v.name, state_update[1]);
                    if (pos >= 0) {
                        state_update[1] = state_update[1].replace(v.name, v.name + "(st)");
                    }
                    action = state_update[0].trim() + " := " + state_update[1].trim();
                }
            });*/
        }
        tmp += ",\n           new_st = new_st WITH [ " + action + " ]";
        return tmp;
    }


    /**
     * Utility function for identifying and grouping together cases that make up the body of a transition
     */
    function parseCases(transitions) {
        var transitionsSpec = d3.map();
        transitions.forEach(function (transition) {
            var t = parseTransition(transition);
            // extract transition signature
            var signature = t.name + "(" + "st: (per_" + t.name + ")): State";
            // collect cases that will make up the body of the transition
            var cases = transitionsSpec.has(signature) ?
                         transitionsSpec.get(signature)
                         : [];
            cases.push({
                cond: (t.cond === "") ? null : t.cond,
                actions: t.actions,
                from: t.from,
                to: t.to
            });
            transitionsSpec.set(signature, cases);
        });
        return transitionsSpec;
    }

    /**
     * Prints PVS definitions of utility functions used in Emuchart
     */
    function print_utils() {
        var ans = "  %-- utility functions";
        ans += "\n  enter_into(ms: MachineState)(st: State): State = st WITH [ current_state := ms ]";
        ans += "\n  leave_state(ms: MachineState)(st: State): State = st WITH [ previous_state := ms ]\n";
        return ans;
    }


    /**
     * Prints PVS definitions for Emuchart initial transitions
     */
    EmuchartsCppPrinter.prototype.print_initial_transition = function (emuchart) {
        var initial_transitions = emuchart.initial_transitions;
        var ans = "";
        if (initial_transitions && initial_transitions.length > 0) {
            ans += "  // initial state\n";
            ans += "  void init(x: real) {\n";
            ans += "    current_state  = " + initial_transitions[0].target.name + ";\n";
            ans += "    previous_state = " + initial_transitions[0].target.name + ";\n";
            var variables = emuchart.variables;
            if (variables) {
                variables.forEach(function (variable) {
                    var pos = initial_transitions[0].name.indexOf(variable.name);
                    if (pos >= 0) {
                        var tmp = initial_transitions[0].name.substring(pos);
                        pos = tmp.indexOf(":=");
                        if (pos >= 0) {
                            tmp = tmp.substring(pos + 2);
                            pos = tmp.indexOf(";");
                            if (pos >= 0) {
                                tmp = tmp.substr(0, pos).trim();
                                ans += "    " + variable.name + " = " + tmp + ";\n";
                            }
                        }
                    }
                });
            }
            ans += "  }\n";
        }
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart transitions given in the form transition [condition] {actions}
     */
    EmuchartsCppPrinter.prototype.print_transitions = function (emuchart) {
        var transitions = emuchart.transitions;
        var variables = emuchart.variables;
        var ans = "";/*
        if (transitions && transitions.length > 0) {
            ans += print_utils();
            ans += "  %-- transition functions\n";
            var transitionsSpec = parseCases(transitions);
            // for each transition, print the transition body made out of the identifies cases
            transitionsSpec.forEach(function (signature) {
                // generate permission
                var tmp = "  per_" + signature.substr(0, signature.indexOf("(")) +
                            "(st: State): bool";
                var cases = transitionsSpec.get(signature);
                if (cases && cases.length > 0) {
                    tmp += " = ";
                    var i = 0;
                    for (i = 0; i < cases.length; i++) {
                        tmp += "current_state(st) = " + cases[i].from;
                        if (i < cases.length - 1) {
                            tmp += " OR ";
                        }
                    }
                }
                // generate transition
                tmp += "\n  " + signature;
                if (cases && cases.length > 0) {
                    tmp += " =\n   COND";
                    cases.forEach(function (cs) {
                        // check if the condition uses state variables
                        // -- if so, we need to add parameter (st)
                        if (variables) {
                            variables.forEach(function (v) {
                                // for each state variable, add suffix (st)
                                var pos = indexOfStateVariableLF(v.name, cs.cond);
                                if (pos >= 0) { cs.cond = cs.cond.replace(v.name, v.name + "(st)").trim(); }
                            });
                        }
                        tmp += "\n    current_state(st) = " + cs.from;
                        if (cs.cond) { tmp += " AND " + cs.cond; }
                        tmp += "\n    -> LET new_st = leave_state(" + cs.from + ")(st)";
                        cs.actions.forEach(function (action) {
                            tmp += printAction(action, variables);
                        });
                        tmp += "\n        IN enter_into(" + cs.to + ")(new_st),";
                    });
                    tmp = tmp.substr(0, tmp.length - 1) + "\n   ENDCOND";
                }
                ans += tmp + "\n\n";
            });
        }*/
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart variables
     */
    EmuchartsCppPrinter.prototype.print_variables = function (emuchart) {
        var variables = emuchart.variables;
        var ans = "  // emuchart state\n  struct State {\n" +
                    "   MachineState current_state;\n" +
                    "   MachineState previous_state;\n";
        if (variables && variables.length > 0) {
            variables.forEach(function (v) {
                ans += "   " + v.type + ": " + v.name + ";\n";
            });
        }
        ans += "  };\n";
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart constants
     */
    EmuchartsCppPrinter.prototype.print_constants = function (emuchart) {
        var constants = emuchart.constants;
        var ans = "";
        if (constants && constants.length > 0) {
            ans += "  // constants\n";
            constants.forEach(function (c) {
                ans += "  " + c.type + " " + c.name;
                if (c.value) {
                    ans += " = " + c.value;
                }
                ans += ";\n";
            });
        }
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart constants
     */
    EmuchartsCppPrinter.prototype.print_importings = function (emuchart) {
        var importings = emuchart.importings;
        var ans = "#include <iostream>\n";
        if (importings && importings.length > 0) {
            importings.forEach(function (importing) {
                ans += "#include " + importing + "\n";
            });
        }
        return ans;
    };

    EmuchartsCppPrinter.prototype.print_descriptor = function (emuchart) {
        var ans = "% ---------------------------------------------------------------" +
                    "\n%  Class: " + emuchart.name;
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

    EmuchartsCppPrinter.prototype.print_disclaimer = function () {
        var ans = "\n% ---------------------------------------------------------------\n" +
                    "%  C++ class generated using PVSio-web CppPrinter ver 0.1\n" +
                    "%  Tool freely available at http://www.pvsioweb.org" +
                    "\n% ---------------------------------------------------------------\n";
        return ans;
    };

    /**
     * Prints the entire C++ class
     */
    EmuchartsCppPrinter.prototype.print = function (emuchart) {
        var ans = this.print_descriptor(emuchart) + "\n";
        ans += this.print_importings(emuchart) + "\n";
        ans += "class " + emuchart.name + " {\n";
        ans += this.print_constants(emuchart);
        ans += this.print_states(emuchart);
        ans += this.print_variables(emuchart);
        ans += this.print_initial_transition(emuchart);
        ans += this.print_transitions(emuchart);
        ans += "};\n";
        ans += this.print_disclaimer();
        return { res: ans };
    };

    module.exports = EmuchartsCppPrinter;
});
