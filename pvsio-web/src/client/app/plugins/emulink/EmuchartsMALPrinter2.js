/** @module EmuchartsMALPrinter */
/**
 * EmuchartsMALPrinter provides functions to generate MAL models from Emucharts
 * @authors: Paolo Masci, Rui Couto
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
    
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    
    var model_name;
    var parser;
    
    /**
	 * Constructor
	 */
    function EmuchartsMALPrinter(name) {
        model_name = name;
        parser = new EmuchartsParser();
        return this;
    }
    
    function preprocessName(name) {
        return name.replace(new RegExp("_", "g"),"");
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
                ans += preprocessName(emuchart.states[i].name);
                if (i < emuchart.states.length - 1) { ans += ", "; }
                i++;
            }
            ans += " }\n";
        }
        return ans + "\n";
    };
    

//    /**
//     * Parser for transitions given in the form name [ condition ] { actios },
//     * where name is the transition name, and conditions and actions are optionals
//     * @returns a transition object with the following structure: 
//     *                  { name: string, // the transition label
//     *                    cond: string, // represents a boolean expression
//     *                    actions: (array of strings), // each action represents a state update
//     *                    from: (string), // source state label
//     *                    to: (string) // target state label }
//     */
//    function parseTransition(transition) {
//        var ans = {
//            name: "",
//            cond: "",
//            actions: [],
//            from: (transition.source) ? transition.source.name : null,
//            to: transition.target.name
//        };
//        function parseTransitionName(transition) {
//            var pos = transition.indexOf("[");
//            if (pos > 0) { return transition.substr(0, pos).trim(); }
//            pos = transition.indexOf("{");
//            if (pos > 0) { return transition.substr(0, pos).trim(); }
//            return transition.trim();
//        }
//        var sqOpen = transition.name.indexOf("[");
//        var sqClose = transition.name.indexOf("]");
//        var curOpen = transition.name.indexOf("{");
//        var curClose = transition.name.indexOf("}");
//        ans.name = parseTransitionName(transition.name);
//        if (sqOpen >= 0 && sqClose > sqOpen) {
//            ans.cond = transition.name.substring(sqOpen + 1, sqClose).trim();
//        }
//        if (curOpen >= 0 && curClose > curOpen) {
//            var actions = transition.name.substring(curOpen + 1, curClose).split(";");
//            actions.forEach(function (action) {
//                var a = action.trim();
//                if (a !== "") {
//                    ans.actions.push(a);
//                }
//            });
//            
//        }
//        return ans;
//    }

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
        var actions = d3.map();
        
        if (emuchart.transitions && emuchart.transitions.length > 0) {
            emuchart.transitions.forEach(function (t) {
                var transition = parser.parseTransition(t.name);
                if (transition.res) {
                    transition = transition.res.val;
                    actions.set(preprocessName(transition.identifier.val), transition.identifier.type);
                }
            });
            var identifiers = actions.keys();
            identifiers.forEach(function (identifier) {
                ans += "  " + identifier + "\n";
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
        var ans = " axioms\n";
        if (emuchart.initial_transitions && emuchart.initial_transitions.length > 0) {
            emuchart.initial_transitions.forEach(function (t) {
                var transition = parser.parseTransition(t.name);
                if (transition.res) {
                    transition = transition.res.val;
                    ans += "  [] ";
                    ans += "previous_state = " + preprocessName(t.target.name) + " & ";
                    ans += "current_state = " + preprocessName(t.target.name);
                    //initialize variables
                    if (emuchart.variables && emuchart.variables.length > 0) {
                        emuchart.variables.forEach(function (v) {
                            ans += " & " + preprocessName(v.name) + " = " + v.value;
                        });
                    }
                    if (transition.actions) {
                        transition.actions.forEach(function (action) {
                            ans += " & " + action;
                        });
                    }
                }
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
        var res = "";
        var actions = d3.map();
        emuchart.transitions.forEach(function (mLabel) {
            var label = preprocessName(mLabel.name);
            var from = preprocessName(mLabel.source.name);
            var to = preprocessName(mLabel.target.name);
            
            if (!label || label === "") {
                return { err: "Unexpected label", res: null };
            }
            var ans = parser.parseTransition(label);
            if (ans.res) {
                var transition = {
                    identifier: ans.res.val.identifier || { type: "identifier", val: "tick" },
                    cond:       ans.res.val.cond || { type: "expression", val: [] },
                    actions:    ans.res.val.actions || { type: "actions", val: [] }
                };
                
                //The transition "Action"
                var action = transition.identifier.val;
                
                //The transiton "Effect"
                var effect = "";
                if (transition.actions.val.length > 0) {
                    effect += transition.actions.val[0].val.identifier.val;
                    var bo = transition.actions.val[0].val.binop.val;
                    if (bo === ":=") {
                        bo = "'=";
                    }
                    effect += bo;
                    transition.actions.val[0].val.expression.val.forEach(function (v) {
                        effect += v.val;
                    });
                }
                
                //The transition "Condition"
                var cond = "";
                if (transition.cond.val.length > 0) {
                    if (transition.cond.val.length === 1) {
                        cond += transition.cond.val[0].val;
                    } else if (transition.cond.val.length === 2) {
                        cond += transition.cond.val[0].val;
                        cond += transition.cond.val[1].val;
                    } else {
                        cond += transition.cond.val[0].val;
                        cond += transition.cond.val[1].val === "==" ? "=" : transition.cond.val[1].val;
                        cond += transition.cond.val[2].val;
                    }
                }
                
                //Create pair (dest, [condition,effect]) -- Transition
                var mtransition = d3.map();
                var ce = [];
                ce.push(cond);
                ce.push(effect);
                mtransition.set(to, ce);
                
                
                //Create pair (from, [<Transition>]) --Condition
                var mval = d3.map();
                var a = [];
                if (actions.get(action) !== undefined) {
                    var maa = actions.get(action);
                    maa.forEach(function (e) {
                        if (e.get(from) !== undefined) {
                            a = e.get(from);
                        }
                    });
                }
                a.push(mtransition);
                mval.set(from, a);
                
                //creating pair (action, <Condition>)
                var aa = [];
                if (actions.get(action) !== undefined) {
                    var i;
                    var del = false;
                    for (i = 0; i < actions.get(action).length; i++) {
                        console.log("-checking [" + i + "] if " + actions.get(action)[i].keys()[0] + " == " + from);
                        if (actions.get(action)[i].keys()[0] === from) {
                            del = true;
                            break;
                        }
                    }
                    if (del) {
                        console.log("-- has equal! removing entry on " + i);
                        actions.get(action).splice(i, 1);
                        console.log("Result: " + JSON.stringify(actions.get(action)));
                    }
                    aa = actions.get(action);
                    
                }
                aa.push(mval);
                actions.set(action, aa);
            }
        });
        //actions contains set (action, (from, [(to, condition)]))
        //
        //rcButton - stopped -> opening, memory=opening
        //                   -> closing, memory=closing
        //opSensor - opening -> open, ""
        //click_on - closed  -> opening, display=100
        //
        
        //stopped is repeated: rcButton has 2 stopped
        console.debug(actions);
        
        actions.keys().forEach(function (taction) { //taction - action
            var trAction = taction;
            var trPerms = "";
            actions.get(taction).forEach(function (a) { //a - (from, [(dest, [cond, eff])])
                //res += "++" + a + "\n";
                var conds = [];
                a.forEach(function (tfrom) { //tfrom - from
                    var trFrom = tfrom;
                    a.get(tfrom).forEach(function (c) { //c - (dest, [cond, eff])
                        c.forEach(function (tdest) {//tdest - dest
                            var trDest = tdest;
                            var trCond = c.get(tdest)[0];
                            var trEff = c.get(tdest)[1];
                            res += "  (current_state=" + trFrom + ")";
                            if (trCond !== "") {
                                res += " & (" + trCond + ")";
                                conds.push(trCond);
                            }
                            res += " -> [" + trAction + "] (current_state'=" + trDest + ")";
                            if (trEff !== "") {
                                res += " & (" + trEff + ")";
                            }
                            res += "\n";
                        });
                    });
                    
                });
                //add condition to permitions
                var trFrom = a;
                trPerms += "( current_state = " + trFrom.keys()[0];
                conds.forEach(function (mp) {
                    trPerms += " & " + mp;
                });
                trPerms +=  " ) | ";
            });
            //TODO: per()
            if (trPerms !== "") {
                res += "  per(" + trAction + ") -> " + trPerms.substring(0, trPerms.length - 2) + "\n\n";
            }
        });
        return res;
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
                ans += "  " + preprocessName(v.name) + ": " + v.type + "\n";
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
                    ans += preprocessName(c.name) + " " + c.value + "\n";
                } else {
                    ans += "# " + preprocessName(c.name) + "\n";
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
                    "#  MAL model generated using PVSio-web MALPrinter2 ver 0.1\n" +
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
        
        //ans += "interactor " + emuchart.name + "\n"; // the MAL interactor
        ans += "interactor main #" + emuchart.name + "\n";
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
