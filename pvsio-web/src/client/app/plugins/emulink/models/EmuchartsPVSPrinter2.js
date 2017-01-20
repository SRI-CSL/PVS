/** @module EmuchartsPVSPrinter */
/**
 * EmuchartsPVSPrinter provides functions to generate PVS models from Emucharts
 * @author Paolo Masci
 * @date Nov 13, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var displayNotificationView  = require("plugins/emulink/forms/displayNotificationView");

    var leave_enter_function_templates = require("text!plugins/emulink/models/pvs/templates/pvs_leave_enter_functions.handlebars"),
        machine_states_templates = require("text!plugins/emulink/models/pvs/templates/pvs_machine_states.handlebars");

    var theory_name;
    var parser;

    var initialMachineState = "initialMachineState";
    var machineStateType = "MachineState";

    var pvsRecordTypePrinter = require("plugins/emulink/models/pvs/pvsRecordTypePrinter");
    var pvsEnumeratedTypePrinter = require("plugins/emulink/models/pvs/pvsEnumeratedTypePrinter");
    var pvsOverrideExpressionPrinter = require("plugins/emulink/models/pvs/pvsOverrideExpressionPrinter");

    var predefined_variables = {
        previous_state: { name: "previous_state", type: machineStateType, value: initialMachineState },
        current_state: { name: "current_state", type: machineStateType, value: initialMachineState }
    };

    var automaticConstants;

    var displayNotification = function (msg, title) {
        title = title || "Notification";
        displayNotificationView.create({
            header: title,
            message: msg,
            buttons: ["Ok"]
        }).on("ok", function (e, view) {
            view.remove();
        });
    };
    var displayError = function (msg) {
        displayNotification(msg, "Compilation Error");
    };
//    var displayWarning = function (msg) {
//        displayNotification(msg, "Warning");
//    };
    /**
     * Constructor
     */
    function EmuchartsPVSPrinter(name) {
        theory_name = name;
        automaticConstants = [];
        parser = new EmuchartsParser();
        pvsRecordTypePrinter = pvsRecordTypePrinter.create();
        pvsEnumeratedTypePrinter = pvsEnumeratedTypePrinter.create();
        pvsOverrideExpressionPrinter = pvsOverrideExpressionPrinter.create();
        return this;
    }

    EmuchartsPVSPrinter.prototype.set_theory_name = function (name) {
        theory_name = name;
        return this;
    };

    /**
     * Prints PVS definitions for Emuchart states
     */
    EmuchartsPVSPrinter.prototype.print_states = function (emuchart) {
        return Handlebars.compile(machine_states_templates, { noEscape: true })({
            indent: "  ",
            states: (emuchart && emuchart.states) ? emuchart.states.map(function (state) { return state.name; }) : []
        });
    };

    /**
     * Prints PVS definitions of utility functions used in Emuchart
     */
    function print_utils() {
        return Handlebars.compile(leave_enter_function_templates, { noEscape: true })({
            indent: "  ",
            current_state: predefined_variables.current_state,
            previous_state: predefined_variables.previous_state
        });
    }

    // utility function for recognising pvs state variables
    function isVariable(name, emuchart) {
        if (name === predefined_variables.current_state.name ||
                name === predefined_variables.previous_state.name) {
            return true;
        }
        if (emuchart.variables) {
            var i = 0;
            for (i = 0; i < emuchart.variables.length; i++) {
                if (name === emuchart.variables[i].name ||
                      emuchart.variables[i].name.indexOf(name + ".") >= 0 ) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * This function converts the name of operators in expressions -- needed for && || == != !
     */
    var preProcess = function (term, emuchart) {
        function printFunction(term) {
            if (term.type === "identifier") {
                if (isVariable(term.val, emuchart)) {
                    if (term.val.indexOf(".") >= 0) {
                        var v = term.val.split(".");
                        v[0] += "(st)";
                        term.val = v.join("`");
                    } else {
                        term.val += "(st)";
                    }
                }
                return term.val;
            } else if (typeof term.val === "string") {
                return term.val;
            } else if (term.type === "function") {
                var ans = "", i = 0;
                for (i = 0; i < term.val.length; i++) {
                    ans += printFunction(term.val[i]);
                }
                return ans;
            }
            return term;
        }
        if (term) {
            if (term.type === "binop") {
                if (term.val === "&&") {
                    term.val = "AND";
                } else if (term.val === "||") {
                    term.val = "OR";
                } else if (term.val === "==") {
                    term.val = "=";
                } else if (term.val === "!=") {
                    term.val = "/=";
                }
            } else if (term.type === "unaryop") {
                if (term.val === "!") {
                    term.val = "NOT";
                }
            } else if (term.type === "function") {
                term.val = printFunction(term);
            }
        }
        return term;
    };


    /**
     * Prints PVS definitions for Emuchart initial transitions
     */
    EmuchartsPVSPrinter.prototype.print_initial_transition = function (emuchart) {
        var ret = { err: null, res: null };
        if (emuchart.initial_transitions && emuchart.initial_transitions.length > 0) {
            var initialTransition = emuchart.initial_transitions[0];
            if (initialTransition.name === "") {
                initialTransition.name = "init";
            }
            var ans = parser.parseTransition(initialTransition.name);
            if (ans.err || ans.res.type !== "transition") {
                ret.err = "Initial transition " + emuchart.initial_transitions[0].name + "\n\n";
                if (ans.err) {
                    ret.err += ans.err;
                } else {
                    ret.err += "Expecting type 'transition', got " + ans.res.type;
                }
                console.log(ans.err);
                return ret;
            }

            var theTransition = {
                identifier: ans.res.val.identifier || { type: "identifier", val: "init" },
                cond:    ans.res.val.cond || { type: "expression", val: [] },
                actions: ans.res.val.actions || { type: "actions", val: [] },
                to:   initialTransition.target.name || initialMachineState
            };
            var pvsFunction = {
                identifier: theTransition.identifier,
                signature:  "init(x: real): State",
                cases: {
                    letExpr: [],
                    inExpr: [ ("st") ]
                }
            };
            theTransition.actions.val.forEach(function (action) {
                // actions involving variables have already been taken into account
                if (isVariable(action.val.identifier.val, emuchart)) {
                    var tmp = [];
                    action.val.expression.val.forEach(function (term) {
                        if (isVariable(term.val, emuchart)) {
                            term = pvsRecordTypePrinter.printRecordAccessor("st." + term.val);
                        } else {
                            term = preProcess(term, emuchart);
                        }
                        tmp.push(term.val);
                    });
                    var letExp = pvsOverrideExpressionPrinter.print({
                        name: "st." + action.val.identifier.val,
                        override: tmp.join(" ")
                    });
                    pvsFunction.cases.letExpr.push(letExp);
                }
            });
            var code = "  %-- initial state\n  ";
            code += pvsFunction.signature;
            var variables = [];
            predefined_variables.current_state.value = theTransition.to;
            predefined_variables.previous_state.value = theTransition.to;
            variables.push(predefined_variables.current_state);
            variables.push(predefined_variables.previous_state);
            variables = variables.concat(emuchart.variables);
            // FIXME: check for name conflicts when adding variables
            // so user has immediate feedback about issues with names
            // or alternatively popup a dialog, highlight the issues and
            // to let the user fix them
            var v = parser.parseVariables(variables, {
                onNameConflict: function (name) {
                    alert("Warning: name conflict for variable '" + name + "'.");
                }
            });
            if (v.err) {
                ret.err = v.err;
            } else {
                var rval = pvsRecordTypePrinter.printRecordValue(v.res);
                if (pvsFunction.cases.letExpr.length) {
                    code += " = \n  LET st = " + rval;
                    code += ",\n     " + pvsFunction.cases.letExpr.join(",\n     ");
                    code += "\n    IN " + pvsFunction.cases.inExpr + "\n\n";
                } else {
                    code += " = " + rval;
                }
                ret.res = code;
            }
        }
        return ret;
    };

    /**
     * Prints PVS definitions for Emuchart transitions given in the form transition [condition] {actions}
     */
    EmuchartsPVSPrinter.prototype.print_transitions = function (emuchart) {
/*        function isVariable(term) {
            if (term.type === "identifier") {
                if (term.val === predefined_variables.current_state.name ||
                        term.val === predefined_variables.previous_state.name) {
                    return true;
                }
                if (emuchart.variables) {
                    var i = 0;
                    for (i = 0; i < emuchart.variables.length; i++) {
                        if (term.val === emuchart.variables[i].name) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }*/
        function printValue(term) {
            if (isVariable(term.val, emuchart)) {
                if (term.val.indexOf(".") >= 0) {
                    var v = term.val.split(".");
                    v[0] += "(st)";
                    term.val = v.join("`");
                } else {
                    term.val += "(st)";
                }
            }
            return term.val;
        }

        var ret = { err: null, res: null };
        // multiple transitions can have the same identifier
        // (because the same transition can originate from different nodes)
        // this keeps track of the transitions we've already processed -- needed to avoid duplicates
        var done = d3.map();

        if (emuchart.transitions && emuchart.transitions.length > 0) {
            var transitions = [];
            emuchart.transitions.forEach(function (t) {
                if (t.name === "") { t.name = "tick"; }
                var ans = parser.parseTransition(t.name);

                if (!ans.err && ans.res && ans.res.type === "transition") {
                    transitions.push({
                        identifier: ans.res.val.identifier || { type: "identifier", val: "tick" },
                        cond:    ans.res.val.cond,
                        actions: ans.res.val.actions,
                        from: t.source.name,
                        to:   t.target.name
                    });
                } else {
                    ret.err = t.name + "\n" + ans.err;
                    console.log(ans.err);
                    return ret;
                }
            });
            var pvsFunctions = []; // this is an array of objects representing pvs functions
            transitions.forEach(function (theTransition) {
                // first, check whether we have already processed the transition
                // if not, add the transition identifier to the list of transitions already processed
                if (done.get(theTransition.identifier.val)) { return; }
                done.set(theTransition.identifier.val, true);

                // permission function
                var permissionFunction = {
                    identifier: "per_" + theTransition.identifier.val,
                    signature : "per_" + theTransition.identifier.val + "(st: State): bool",
                    cases: []
                    // the body of the permission is given by the disjunction of the collected cases
                };
                // transition function
                var transitionFunction = {
                    identifier: theTransition.identifier.val,
                    signature : theTransition.identifier.val + "(st: (" + permissionFunction.identifier + ")): State",
                    cases: []
                    // the body of the function is given by a COND-ENDCOND statement
                    // made up from the expressions collected in array cases
                };

                // generate cases for permission function and transition function
                transitions.forEach(function (transition) {
                    // each case depends on the state from which the transition starts, and the transition conditions
                    // transitions with the same name can start from different states and have different conditions
                    if (transition.identifier.val === theTransition.identifier.val) {
                        // the final expression for pre is the conjunction of all expressions
                        var cond = [ ("(current_state(st) = " + transition.from + ")") ];
                        if (transition.cond && transition.cond.type === "expression" &&
                                transition.cond.val && transition.cond.val.length > 0) {
                            var tmp = [];
                            transition.cond.val.forEach(function (term) {
                                // identifiers of state variables need to be followed by (st)
                                if (term.type === "identifier") {
                                    preProcess(term, emuchart);
                                    if (isVariable(term.val, emuchart)) {
                                        if (term.val.indexOf(".") >= 0) {
                                            var v = term.val.split(".");
                                            v[0] += "(st)";
                                            term.val = v.join("`");
                                        } else {
                                            term.val += "(st)";
                                        }
                                    }
                                    tmp.push(term.val);
                                } else {
                                    preProcess(term, emuchart);
                                    tmp.push(term.val);
                                }
                            });
                            cond.push("(" + tmp.join(" ") + ")");
                        }
                        // the final expression for post is a LET-IN expression
                        // given by the sequence of collected statements separated by commas
                        var letExpr = [ ("LET new_st = leave_state(" + transition.from + ")(st)") ];
                        var inExpr = "";
                        if (transition.actions && transition.actions.val &&
                                transition.actions.val.length > 0) {
                            transition.actions.val.forEach(function (action) {
                                var expr = "new_st = new_st WITH [ ";
                                var brackets = [];
                                if (action.val.identifier.val.indexOf(".") >= 0) {
                                    var v = action.val.identifier.val.split(".");
                                    var i = 0;
                                    for (i = 0; i < v.length; i++) {
                                        expr += v[i];
                                        if (i < v.length - 1) {
                                            expr += " := " + v[i] + "(new_st) WITH [ ";
                                            brackets.push("]");
                                        }
                                    }
                                } else {
                                    expr += action.val.identifier.val;
                                }
                                expr += " := ";
                                var tmp = [];
                                action.val.expression.val.forEach(function (term) {
                                    preProcess(term, emuchart);
                                    tmp.push(printValue(term));
                                });
                                expr += tmp.join(" ") + " ]";
                                brackets.forEach(function (bracket) {
                                    expr += "]";
                                });
                                letExpr.push(expr);
                            });
                        }
                        inExpr = "IN enter_into(" + transition.to + ")(new_st)";

                        permissionFunction.cases.push("(" + cond.join(" AND ") + ")");
                        transitionFunction.cases.push({ cond: cond, letExpr: letExpr, inExpr: inExpr });
                    }
                });

                // store results
                pvsFunctions.push({ per: permissionFunction, tran: transitionFunction });
            });

            var ans = print_utils();
            ans += "\n  %-- transition functions\n";
            pvsFunctions.forEach(function (f) {
                ans += "  " + f.per.signature + " = " + f.per.cases.join(" OR ") + "\n";
                ans += "  " + f.tran.signature + " =\n    COND\n";
                var tmp = [];
                f.tran.cases.forEach(function (c) {
                    var expr = "    " + c.cond.join(" AND ") + "\n     -> ";
                    expr += c.letExpr.join(",\n            ") + "\n         ";
                    expr += c.inExpr;
                    tmp.push(expr);
                });
                ans += tmp.join(",\n") + ",\n    ELSE -> st\n    ENDCOND\n\n";
            });
            ret.res = ans;
        }
        return ret;
    };


    /**
     * Prints the PVS definition for Emuchart variables
     */
    EmuchartsPVSPrinter.prototype.print_variables = function (emuchart) {
        var variables = [];
        variables.push(predefined_variables.current_state);
        variables.push(predefined_variables.previous_state);
        variables = variables.concat(emuchart.variables);
        // todo: create the state variable when adding variables so that user has immediate feedback
        var v = parser.parseVariables(variables, {
            onNameConflict: function (name) {
                alert("Warning: name conflict for variable '" + name + "'.");
            }
        });
        if (v.err) {
            alert(v.err);
            return v.err;
        }
        var ans = "  %-- emuchart state\n";
        ans += pvsRecordTypePrinter.printTypeDefinition({
            name: "State",
            value: v.res
        });
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart constants
     */
    EmuchartsPVSPrinter.prototype.print_constants = function (emuchart) {
        var constants = emuchart.constants;
        var ans = "";
        if (constants && constants.length > 0) {
            ans += "\n  %-- constants\n";
            constants.forEach(function (c) {
                ans += "  " + c.name + ": " + c.type;
                if (c.value) { ans += " = " + c.value; }
                ans += "\n";
            });
        }
        if (automaticConstants.length > 0) {
            ans += "\n  %-- constants generated by PVSPrinter\n";
            automaticConstants.forEach(function (c) {
                ans += "  " + c.name + ": " + c.type;
                if (c.value) { ans += " = " + c.value; }
                ans += "\n";
            });
        }
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart constants
     */
    EmuchartsPVSPrinter.prototype.print_importings = function (emuchart) {
        var importings = emuchart.importings;
        var ans = "";
        if (importings && importings.length > 0) {
            ans += " IMPORTING ";
            importings.forEach(function (importing) {
                ans += importing + ", ";
            });
            ans = ans.substr(0, ans.length - 1) + "\n";
        }
        return ans;
    };

    EmuchartsPVSPrinter.prototype.print_descriptor = function (emuchart) {
        var ans = "% ---------------------------------------------------------------" +
                    "\n%  Theory: " + emuchart.name;
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

    EmuchartsPVSPrinter.prototype.print_disclaimer = function () {
        var ans = "\n% ---------------------------------------------------------------\n" +
                    "%  PVS theory generated using PVSio-web PVSPrinter ver 0.1\n" +
                    "%  Tool freely available at http://www.pvsioweb.org" +
                    "\n% ---------------------------------------------------------------\n";
        return ans;
    };

    /**
     * Prints the entire PVS theory
     */
    EmuchartsPVSPrinter.prototype.print = function (emuchart) {
        automaticConstants = [];
        var ret = { err: null, res: null };

        var ans = this.print_descriptor(emuchart) + "\n";
        ans += emuchart.name + ": THEORY\n BEGIN\n";
        ans += this.print_importings(emuchart);
        ans += this.print_constants(emuchart);
        ans += this.print_states(emuchart);    // -- done using handlebars library
        ans += this.print_variables(emuchart); // -- done using handlebars library

        var initialTransitions = this.print_initial_transition(emuchart);
        var transitions = this.print_transitions(emuchart);
        if (initialTransitions.err || transitions.err) {
            ret.err = initialTransitions.err || transitions.err;
            displayError(ret.err);
            return ret;
        }

        ans += initialTransitions.res || "\n";
        ans += transitions.res || "\n";
        ans += " END " + emuchart.name + "\n";
        ans += this.print_disclaimer();
        ret.res = ans;

        return ret;
    };

    module.exports = EmuchartsPVSPrinter;
});
