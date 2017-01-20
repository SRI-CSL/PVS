/** @module EmuchartsMALPrinter */
/**
 * EmuchartsMALPrinter provides functions to generate MAL models from Emucharts
 * @authors: Paolo Masci
 * @date 2014/22/10 11:00:21 AM
 *
 * Emuchart objects have the following structure:
      emuchart = {
                name: (string),
                author: {
                    name: (string),
                    affiliation: (string),
                    contact: (string)
                },
                importings: (not used for now),
                constants: (array of {
                                name: (string), // the constant identifier
                                type: (string), // the constant type
                                value: (string) // the constant value (can be undefined)
                            }),
                variables: (array of {
                                name: (string), // the variable identifier
                                type: (string), // the variable type
                                scope: (string) // the variable scope, either local or global
                            }),
                states: (array of {
                                name: (string), // the state label
                                id: (string),   // a unique identifier
                            }),
                transitions: (array of {
                                name: (string), // the transition label
                                id: (string),   // a unique identifier
                                source: {
                                    name: (string), // the source state label
                                    id: (string)    // a unique identifier
                                },
                                target: {
                                    name: (string), // the target state label
                                    id: (string)    // a unique identifier
                                },
                            }),
                initial_transitions: (array of {
                                name: (string), // the initial transition label
                                id: (string),   // a unique identifier
                                target: {
                                    name: (string), // the target state label
                                    id: (string)    // a unique identifier
                                },
                            })
      }
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3 */
define(function (require, exports, module) {
	"use strict";
    
    var model_name;
    
    /**
	 * Constructor
	 */
    function EmuchartsMALPrinter(name) {
        model_name = name;
        return this;
    }
    
    /**
     * Prints MAL types
     */
    EmuchartsMALPrinter.prototype.print_types = function (emuchart) {
        var ans = "types\n";
        ans += " int = INT_MIN..INT_MAX\n";
        ans += " nat = 0..INT_MAX\n";
        if (emuchart.states && emuchart.states.length > 0) {
            ans += " MachineState = { ";
            var i = 0;
            while (i < emuchart.states.length) {
                ans += emuchart.states[i].name;
                if (i < emuchart.states.length - 1) { ans += ", "; }
                i++;
            }
            ans += " }\n";
        }
        return ans + "\n";
    };
    

    /**
     * Parser for transitions given in the form name [ condition ] { actios },
     * where name is the transition name, and conditions and actions are optionals
     * @returns a transition object with the following structure: 
     *                  { name: string, // the transition label
     *                    cond: string, // represents a boolean expression
     *                    actions: (array of strings), // each action represents a state update
     *                    from: (string), // source state label
     *                    to: (string) // target state label }
     */
    function parseTransition(transition) {
        var ans = {
            name: "",
            cond: "",
            actions: [],
            from: (transition.source) ? transition.source.name : null,
            to: transition.target.name
        };
        function parseTransitionName(transition) {
            var pos = transition.indexOf("[");
            if (pos > 0) { return transition.substr(0, pos).trim(); }
            pos = transition.indexOf("{");
            if (pos > 0) { return transition.substr(0, pos).trim(); }
            return transition.trim();
        }
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
     * Prints MAL axioms
     */
    EmuchartsMALPrinter.prototype.print_axioms = function (emuchart) {
        var ans = "";
        //.... TODO
        return ans;
    };
    
    /**
     * Prints MAL actions
     * Each transition is a structure with the following fields:
     *                  { name: string, // the transition label
     *                    cond: string, // represents a boolean expression
     *                    actions: (array of strings), // each action represents a state update
     *                    from: (string), // source state label
     *                    to: (string) // target state label }
     */
    EmuchartsMALPrinter.prototype.print_actions = function (emuchart) {
        var ans = " actions\n";
        if (emuchart.transitions && emuchart.transitions.length > 0) {
            emuchart.transitions.forEach(function (t) {
                var transition = parseTransition(t);
                // first, generate the permission
                ans += "  " + transition.name + "\n";
            });
        }
        ans += "\n";
        return ans;
    };

    /**
     * Prints MAL initial transition
     * Each transition is a structure with the following fields:
     *                  { name: string, // the transition label
     *                    cond: string, // represents a boolean expression
     *                    actions: (array of strings), // each action represents a state update
     *                    to: (string) // target state label }
     */
    EmuchartsMALPrinter.prototype.print_initial_transition = function (emuchart) {
        var ans = "";
        if (emuchart.initial_transitions && emuchart.initial_transitions.length > 0) {
            emuchart.initial_transitions.forEach(function (t) {
                var transition = parseTransition(t);
                ans += " [] ";
                ans += "previous_state = " + transition.to + " & ";
                ans += "current_state = " + transition.to;
                transition.actions.forEach(function (action) {
                    ans += " & " + action;
                });
            });
        }
        ans += "\n\n";
        return ans;
    };

    /**
     * Prints MAL transitions
     * Each transition is a structure with the following fields:
     *                  { name: string, // the transition label
     *                    cond: string, // represents a boolean expression
     *                    actions: (array of strings), // each action represents a state update
     *                    from: (string), // source state label
     *                    to: (string) // target state label }
     */
    EmuchartsMALPrinter.prototype.print_transitions = function (emuchart) {
        var ans = "";
        if (emuchart.transitions && emuchart.transitions.length > 0) {
            var visitedTransitions = d3.map();
            emuchart.transitions.forEach(function (t) {
                var transition = parseTransition(t);
                if (visitedTransitions.has(transition.name) === false) {
                    visitedTransitions.set(transition.name, true);
                    // first, print the permission
                    ans += " per(" + transition.name + ") ->";
                    ans += " current_state = " + transition.from;
                    
                    // second, print the transition conditions, if any
                    var trans;
                    emuchart.transitions.forEach(function (t) {
                        trans = parseTransition(t);
                        if (trans.name === transition.name && trans.cond && trans.cond !== "") {
                            ans += " & " + trans.cond;
                        }
                    });
                    // third, print the transition name
                    ans += "\n [" + transition.name + "]";
                    ans += " -> previous_state' = " + transition.from;
                    ans += " & current_state' = " + transition.to;
                    // finally, print the transition body
                    transition.actions.forEach(function (action) {
                        ans += " & " + action;
                    });
                    ans += "\n\n";
                }
            });
        }
        ans += "\n";
        return ans;
    };

    /**
     * Prints MAL attributes
     */
    EmuchartsMALPrinter.prototype.print_attributes = function (emuchart) {
        var ans = " attributes\n";
        ans += "  current_state: MachineState\n";
        ans += "  previous_state: MachineState\n";
        if (emuchart.variables && emuchart.variables.length > 0) {
            emuchart.variables.forEach(function (v) {
                ans += v.name + ": " + v.type + "\n";
            });
        }
        ans += "\n";
        return ans;
    };

    /**
     * Prints MAL constants
     */
    EmuchartsMALPrinter.prototype.print_constants = function (emuchart) {
        var constants = emuchart.constants;
        var ans = "defines\n";
        ans += " INT_MIN = -4\n";
        ans += " INT_MAX = 4\n";
        if (constants && constants.length > 0) {
            constants.forEach(function (c) {
                if (c.value) {
                    ans += c.name + " " + c.value + "\n";
                } else {
                    ans += "# " + c.name + "\n";
                }
            });
        }
        ans += "\n";
        return ans;
    };


    EmuchartsMALPrinter.prototype.print_descriptor = function (emuchart) {
        var ans = "# ---------------------------------------------------------------\n" +
                    "#  MAL Model: " + emuchart.name;
        if (emuchart.author) {
            ans += "\n#  Author: " + emuchart.author.name +
                    "\n#          " + emuchart.author.affiliation +
                    "\n#          " + emuchart.author.contact;
        }
        if (emuchart.description) {
            ans += "\n# ---------------------------------------------------------------" +
                    "\n#  " + emuchart.description;
        }
        ans += "\n# ---------------------------------------------------------------\n";
        return ans;
    };
    
    EmuchartsMALPrinter.prototype.print_disclaimer = function () {
        var ans = "\n# ---------------------------------------------------------------\n" +
                    "#  MAL model generated using PVSio-web MALPrinter ver 0.1\n" +
                    "#  Tool freely available at http://www.pvsioweb.org" +
                    "\n# ---------------------------------------------------------------\n";
        return ans;
    };
    
    /**
     * Prints the entire MAL model
     */
    EmuchartsMALPrinter.prototype.print = function (emuchart) {
        var ans = this.print_descriptor(emuchart) + "\n";
        ans += this.print_constants(emuchart); // "defines" section
        ans += this.print_types(emuchart); // "types" section
        
        ans += "interactor " + emuchart.name + "\n"; // the MAL interactor
        ans += this.print_attributes(emuchart); // MAL attributes
        ans += this.print_actions(emuchart);
        
        ans += this.print_initial_transition(emuchart);
        ans += this.print_transitions(emuchart);
        ans += "\n";
        ans += this.print_disclaimer();
        return { res: ans };
    };
    
    module.exports = EmuchartsMALPrinter;
});