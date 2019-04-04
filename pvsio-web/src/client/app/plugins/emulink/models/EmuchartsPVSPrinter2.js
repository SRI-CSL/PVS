/** @module EmuchartsPVSPrinter */
/**
 * EmuchartsPVSPrinter provides functions to generate PVS models from Emucharts
 * @author Paolo Masci
 * @date Jan, 2017
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
                                value: (string) // the initial value of the variable
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
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";
    var printer_version = "2.2";
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var projectManager = require("project/ProjectManager").getInstance();
    var displayAskParameters = require("plugins/emulink/forms/displayAskParameters");

    var leave_enter_function_template = require("text!plugins/emulink/models/pvs/templates/pvs_leave_enter_functions.handlebars"),
        init_function_template = require("text!plugins/emulink/models/pvs/templates/pvs_init_function.handlebars"),
        transition_functions_template = require("text!plugins/emulink/models/pvs/templates/pvs_transition_functions.handlebars"),
        enumerated_type_template = require("text!plugins/emulink/models/pvs/templates/pvs_enumerated_type.handlebars"),
        record_type_template = require("text!plugins/emulink/models/pvs/templates/pvs_record_type.handlebars"),
        constants_type_template = require("text!plugins/emulink/models/pvs/templates/pvs_constants_type.handlebars"),
        pvs_theory_template = require("text!plugins/emulink/models/pvs/templates/pvs_theory.handlebars"),
        pvs_descriptor_template = require("text!plugins/emulink/models/pvs/templates/pvs_descriptor.handlebars"),
        pvs_disclaimer_template = require("text!plugins/emulink/models/pvs/templates/pvs_disclaimer.handlebars"),
        pvs_utils_template = require("text!plugins/emulink/models/pvs/templates/pvs_utils.handlebars");

    var theory_name;
    var parser;

    var undef = "UNDEF";
    var machineStateType = "MachineState";
    var stateType = "State";

    var predefined_variables = {
        previous_state: { name: "previous_state", type: machineStateType, value: undef },
        current_state: { name: "current_state", type: machineStateType, value: undef },
        undefined_state: { name: "undef", type: machineStateType, value: undef }
    };

    // var displayNotificationView  = require("plugins/emulink/forms/displayNotificationView");
    // var displayNotification = function (msg, title) {
    //     title = title || "Notification";
    //     displayNotificationView.create({
    //         header: title,
    //         message: msg,
    //         buttons: ["Ok"]
    //     }).on("ok", function (e, view) {
    //         view.remove();
    //     });
    // };
    // var displayError = function (msg) {
    //     displayNotification(msg, "Compilation Error");
    // };
//    var displayWarning = function (msg) {
//        displayNotification(msg, "Warning");
//    };
    /**
     * Constructor
     */
    function EmuchartsPVSPrinter(name) {
        theory_name = name;
        parser = EmuchartsParser.getInstance();
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
        if (emuchart && emuchart.states) {
            return Handlebars.compile(enumerated_type_template, { noEscape: true })({
                comment: "machine states",
                enumtype: [{
                    name: machineStateType,
                    constructors: emuchart.states.map(function (state) {
                        return state.name;
                    }).concat(predefined_variables.undefined_state.value)
                }]
            });
        }
        return "";
    };

    /**
     * Prints PVS definitions for Emuchart states
     */
    EmuchartsPVSPrinter.prototype.print_datatypes = function (emuchart) {
        if (emuchart && emuchart.datatypes && emuchart.datatypes.length > 0) {
            return Handlebars.compile(enumerated_type_template, { noEscape: true })({
                comment: "user defined datatypes",
                enumtype: emuchart.datatypes
            });
        }
        return "";
    };

    function split_variables(emuchart) {
        var basic = [];
        var records = d3.map(); //hashtable used for grouping together variables in the form l1.a l1.b --- these will be part of the same record type.
                                //   key: name (string); value: Array of { l1_name: (string), l2_name: (string), type: (string) }
        emuchart.variables.forEach(function (variable) {
            var tmp = variable.name.split(".");
            if (tmp.length === 2) {
                var l1 = records.get(tmp[0]) || [];
                l1.push({
                    name: tmp[1],
                    type: variable.type,
                    value: variable.value
                });
                records.set(tmp[0], l1);
            } else {
                basic.push({ name: variable.name, type: variable.type, value: variable.value });
            }
        });
        return {
            basic: basic,
            records: records
        };
    }

    /**
     * Prints the PVS definition for Emuchart variables
     */
    EmuchartsPVSPrinter.prototype.print_variables = function (emuchart) {
        var variables = [];
        variables.push({ name: predefined_variables.current_state.name, type: predefined_variables.current_state.type });
        if (predefined_variables.previous_state) {
            variables.push({ name: predefined_variables.previous_state.name, type: predefined_variables.previous_state.type });
        }
        var x = split_variables(emuchart);
        x.basic.forEach(function (basic) {
            variables.push({ name: basic.name, type: basic.type });
        });
        x.records.keys().forEach(function (key) {
            variables.push({
                name: key,
                children: x.records.get(key)
            });
        });
        return Handlebars.compile(record_type_template, { noEscape: true })({
            comment: "emuchart state",
            indent: "  ",
            recordtype: [{
                name: stateType,
                variables: variables
            }]
        });
    };

    /**
     * Prints PVS definitions of utility functions used in Emuchart
     */
    EmuchartsPVSPrinter.prototype.print_enter_exit = function (emuchart) {
        function process_actions(actions, state, opt) {
            opt = opt || [];
            var x = (opt.enter) ? state.enter : state.exit;
            var ans = parser.parseTransition("{" + x + "}");
            if (ans.res && ans.res.type === "transition") {
                var action_sequence = [];
                ans.res.val.actions.val.forEach(function (action) {
                    var tmp = action.val.identifier.val.split(".");
                    // this printer supports up to 2 hierarchical levels
                    if (tmp.length === 2) {
                        action_sequence.push({
                            l1_name: tmp[0], // variable name in the form l1_name.l2_name
                            l2_name: tmp[1],
                            value: print_expression(action.val.expression.val, emuchart, { attach_state: true }) // variable value
                        });
                    } else {
                        action_sequence.push({
                            name: tmp[0], // variable name
                            value: print_expression(action.val.expression.val, emuchart, { attach_state: true }) // variable value
                        });
                    }
                });
                actions.push({
                    state: state.name,
                    action_sequence: action_sequence
                });
            }
        }
        var entry_actions = null, exit_actions = null;
        var states_with_entry_actions = emuchart.states.filter(function (state) {
            return state.enter && state.enter !== "";
        });
        if (states_with_entry_actions.length > 0) {
            entry_actions = [];
            states_with_entry_actions.forEach(function (state) {
                process_actions(entry_actions, state, { enter: true });
            });
        }
        var states_with_exit_actions = emuchart.states.filter(function (state) {
            return state.exit && state.exit !== "";
        });
        if (states_with_exit_actions.length > 0) {
            exit_actions = [];
            states_with_exit_actions.forEach(function (state) {
                process_actions(exit_actions, state, { exit: true });
            });
        }
        var data = {
            entry_actions: entry_actions,
            exit_actions: exit_actions,
            current_state: predefined_variables.current_state,
            full_coverage: true
        };
        if (predefined_variables.previous_state) {
            data.previous_state = predefined_variables.previous_state;
        }
        return Handlebars.compile(leave_enter_function_template, { noEscape: true })(data);
    };

    // utility function for recognising pvs state variables
    function isVariable(name, emuchart) {
        if ((predefined_variables.current_state && name === predefined_variables.current_state.name) ||
                (predefined_variables.previous_state && name === predefined_variables.previous_state.name)) {
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
    // This function "flattens" Object expressions into a string
    // and converts the name of operators in expressions -- needed for && || == != !
    function print_expression(expr, emuchart, opt) {
        function preProcess (term, emuchart) {
            function preprocessFunction(term, emuchart) {
                if (term.type === "identifier") {
                    if (isVariable(term.val, emuchart) && !opt.return_expression) {
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
                        ans += preprocessFunction(term.val[i], emuchart);
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
                    term.val = preprocessFunction(term, emuchart);
                }
            }
            return term;
        }
        opt = opt || [];
        var tmp = [];
        expr.forEach(function (term) {
            if (isVariable(term.val, emuchart)) {
                term.type = "variable";
                if (opt.attach_state) {
                    if (term.val.indexOf(".") >= 0) {
                        var v = term.val.split(".");
                        v[0] += "(st)";
                        term.val = v.join("`");
                    } else {
                        term.val += "(st)";
                    }
                }
            } else {
                term = preProcess(term, emuchart);
            }
            tmp.push(term.val);
        });
        return tmp.join(" ");
    }


    /**
     * Prints PVS definitions for Emuchart initial transitions
     */
    EmuchartsPVSPrinter.prototype.print_initial_transition = function (emuchart) {
        var ans = "";
        if (emuchart.initial_transitions && emuchart.initial_transitions.length > 0) {
            // the name of the initial transition is always init, regardless of what the emucharts says
            var theTransition = { name: "init", init: [], override: [], variables: [] };
            // the initial part of the init function is the initialisation of the state variables
            var x = split_variables(emuchart);
            theTransition.variables = x.basic;
            x.records.keys().forEach(function (l1_name) {
                theTransition.variables = theTransition.variables.concat({
                    name: l1_name,
                    children: x.records.get(l1_name)
                });
            });
            // the second part is the override, if any
            var has_cond = false;
            emuchart.initial_transitions.forEach(function (initial_transition) {
                var ans = parser.parseTransition(initial_transition.name);
                if (ans.res) {
                    var actions = (ans.res.val.actions) ? ans.res.val.actions.val.map(function (action) {
                        var x = action.val.identifier.val.split(".");
                        if (x.length === 2) {
                            return {
                                variable_name: x[0],
                                variable_name_l2: x[1],
                                override_expression: print_expression(action.val.expression.val, emuchart)
                            };
                        }
                        return {
                            variable_name: x[0],
                            override_expression: print_expression(action.val.expression.val, emuchart)
                        };
                    }) : null;
                    if (ans.res.val.cond) {
                        if (emuchart.initial_transitions.length > 1) {
                            // we need to force the override of current_state when the emuchart has multiple initial transitions
                            // because each one of them will lead to a different initial state
                            actions = actions || [];
                            actions = actions.concat({
                                variable_name: predefined_variables.current_state.name,
                                override_expression: initial_transition.target.name
                            });
                        }
                        // print_expression will take care of printing variable names and operators according to the pvs syntax
                        // option attach_state instructs print_expression to concatenate (st) to variable names
                        theTransition.override.push({
                            cond: print_expression(ans.res.val.cond.val, emuchart, { attach_state: true }),
                            actions: actions
                        });
                        has_cond = true;
                    } else {
                        if (actions) {
                            theTransition.override.push({
                                cond: "true",
                                actions: actions
                            });
                        }
                    }
                } else {
                    console.error(ans.err);
                }
            });
            var data = {
                comment: "initialisation function",
                name: theTransition.name,
                args: [{ name: "x",type: "real"}],
                init: theTransition.init,
                override: theTransition.override,
                variables: [{
                    name: predefined_variables.current_state.name,
                    type: predefined_variables.current_state.type,
                    value: (emuchart.initial_transitions.length === 1) ? emuchart.initial_transitions[0].target.name : predefined_variables.current_state.value
                }].concat(theTransition.variables)
            };
            if (predefined_variables.previous_state) {
                data.variables = [{
                    name: predefined_variables.previous_state.name,
                    type: predefined_variables.previous_state.type,
                    value: predefined_variables.current_state.value
                }].concat(data.variables);
            }
            if (emuchart.initial_transitions.length === 1 && theTransition.override.length === 0) {
                data.DEFAULT_INIT = true;
            } else if (theTransition.override.length > 0 && !has_cond) {
                data.INIT_WITH_OVERRIDES = true;
            } else {
                data.INIT_MULTI = true;
            }
            ans = Handlebars.compile(init_function_template, { noEscape: true })(data);
        }
        return ans;
    };

    /**
     * Prints PVS definitions for Emuchart transitions given in the form transition [condition] {actions}
     */
    EmuchartsPVSPrinter.prototype.print_transitions = function (emuchart, opt) {
        opt = opt || {};
        var ans = "";
        // multiple transitions can have the same identifier
        // (because the same transition can originate from different nodes)
        // this keeps track of the transitions we've already processed -- needed to avoid duplicates
        var done = d3.map();

        if (emuchart.transitions && emuchart.transitions.length > 0) {
            var transitions = [];
            // parse transition labels, to extract transition name, guard, and actions
            emuchart.transitions.forEach(function (t) {
                if (t.name === "") { t.name = "tick"; }
                var ans = parser.parseTransition(t.name);
                if (ans.res && ans.res.type === "transition") {
                    transitions.push({
                        identifier: ans.res.val.identifier || { type: "identifier", val: "tick" },
                        cond:    ans.res.val.cond,
                        actions: ans.res.val.actions,
                        from: t.source.name,
                        to:   t.target.name
                    });
                } else {
                    console.error("COULD NOT PARSE " + t.name + " SOURCE:" + t.source.name + " TARGET:" + t.target.name + " ERROR:" + ans.err);
                    transitions.push({
                        identifier: "COULD_NOT_PARSE_" + t.name,
                        from: t.source.name,
                        to:   t.target.name
                    });
                }
            });
            // pvsFunctions is an array of objects representing pvs functions.
            // Each object is in the form: { name: (string), permission: (string), actions: (Array of strings) }
            var pvsFunctions = [];
            transitions.forEach(function (theTransition) {
                // first, check whether we have already processed the transition
                // if not, process the transition and add the transition identifier to the list of transitions already processed
                if (done.get(theTransition.identifier.val)) { return; }
                done.set(theTransition.identifier.val, true);

                // transition function
                // the body of the function is given by a COND-ENDCOND statement
                // made up from the expressions collected in array cases
                var transitionFunction = {
                    name: theTransition.identifier.val,
                    cases: []
                };

                // generate cases for permission function and transition function
                transitions.forEach(function (transition) {
                    // each case depends on the state from which the transition starts, and the transition conditions
                    // transitions with the same name can start from different states and have different conditions
                    if (transition.identifier.val === theTransition.identifier.val) {
                        // First, collect the information necessary to print the permission function, i.e.,
                        // which state is left by the transition, and any guard in the transition label
                        var cond = [ ("(" + predefined_variables.current_state.name + "(st) = " + transition.from + ")") ];
                        if (transition.cond && transition.cond.type === "expression" &&
                                transition.cond.val && transition.cond.val.length > 0) {
                            var tmp = print_expression(transition.cond.val, emuchart, { attach_state: true });
                            cond.push("(" + tmp + ")"); // note that the round parentheses are important here to keep the precedence between operators when joining the predicates
                        }
                        // the permission expression is the conjunction of the predicate indicating which state the transition is leaving
                        // and the guards (if any) in the transition. The same predicate will also be used to guard the actions relative
                        // to this transition
                        cond = cond.join(" AND ");
                        // Second, collect the information necessary to print the body of the transition function, i.e.,
                        // the actions indicated in the transition label
                        var actions = [];//("LET new_st = leave_state(" + transition.from + ")(st)") ];
                        var return_expression = null;
                        if (transition.actions && transition.actions.val &&
                                transition.actions.val.length > 0) {
                            transition.actions.val.forEach(function (action) {
                                if (action.type === "assignment") {
                                    if (isVariable(action.val.identifier.val, emuchart)) {
                                        // the action is an assignment in the form var := expr
                                        var tmp = action.val.identifier.val.split(".");
                                        // this printer supports up to 2 hierarchical levels
                                        if (tmp.length === 2) {
                                            actions.push({
                                                l1_name: tmp[0],
                                                l2_name: tmp[1],
                                                value: print_expression(action.val.expression.val, emuchart, { attach_state: true }),
                                            });
                                        } else {
                                            actions.push({
                                                name: tmp[0],
                                                value: print_expression(action.val.expression.val, emuchart, { attach_state: true }),
                                            });
                                        }
                                    } else {
                                        // the action is a local binding
                                        actions.push({
                                            local_binding: true,
                                            name: action.val.identifier.val,
                                            value: print_expression(action.val.expression.val, emuchart, { attach_state: true }),
                                        });
                                    }
                                } else if (action.type === "return") {
                                    // the action is a return statement -- this type of action should only be used in utility functions
                                    return_expression = print_expression(action.val.expression.val, emuchart, { return_expression: true });
                                }
                            });
                        }
                        // the final expression for post is a LET-IN expression
                        // given by the sequence of collected statements separated by commas
                        var data = {
                            cond: cond,
                            actions: actions,
                            return_expr: return_expression,
                            from: transition.from,
                            to: transition.to
                        };
                        transitionFunction.cases.push(data);
                    }
                });
                // store results for the specific transitio
                pvsFunctions.push(transitionFunction);
            });
            var data = {
                comment: "transition functions",
                functions: pvsFunctions,
                full_coverage: true
            };
            ans = Handlebars.compile(transition_functions_template, { noEscape: true })(data);
        }
        return ans;
    };


    /**
     * Prints PVS definitions for Emuchart constants
     */
    EmuchartsPVSPrinter.prototype.print_constants = function (emuchart) {
        var ans = "";
        if (emuchart.constants && emuchart.constants.length > 0) {
            ans = Handlebars.compile(constants_type_template, { noEscape: true })({
                comment: "user defined constants",
                constants: emuchart.constants
            });
        }
        return ans;
    };

    EmuchartsPVSPrinter.prototype.print_descriptor = function (emuchart) {
        return Handlebars.compile(pvs_descriptor_template, { noEscape: true })({
            name: emuchart.name,
            author: emuchart.author,
            description: emuchart.description
        });
    };

    EmuchartsPVSPrinter.prototype.print_disclaimer = function () {
        return Handlebars.compile(pvs_disclaimer_template, { noEscape: true })({
            printer_version: printer_version,
            www: "http://www.pvsioweb.org"
        });
    };


    // printer options
    var modes = {
        current_state: predefined_variables.current_state.name,
        previous_state: predefined_variables.previous_state.name,
        type: machineStateType
    };
    // utility function for getting the parameters from the user
    function get_params() {
        return new Promise (function (resolve, reject) {
            displayAskParameters.create({
                header: "PVS Printer Options",
                params: [{
                    id: "current_state",
                    name: "Current system mode",
                    value: modes.current_state,
                    inputbox: true
                },{
                    id: "previous_state",
                    name: "Previous system mode",
                    value: modes.previous_state,
                    inputbox: true
                },{
                    id: "state_type",
                    name: "System mode type",
                    value: modes.type,
                    inputbox: true
                }],
                buttons: ["Cancel", "Ok"]
            }).on("ok", function (e, view) {
                view.remove();
                resolve({
                    current_state: e.data.labels.get("current_state"),
                    previous_state: e.data.labels.get("previous_state"),
                    state_type: e.data.labels.get("state_type")
                });
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });
    }

    /**
     * Prints the entire PVS theory
     * When opt.interactive is true, a dialog is shown to the user to select compilation parameters.
     */
    EmuchartsPVSPrinter.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        var _this = this;
        function finalize(resolve, reject, par) {
            par = par || {};
            if (par.current_state) {
                predefined_variables.current_state.name = par.current_state;
            }
            if (par.previous_state) {
                predefined_variables.previous_state.name = par.previous_state;
            } else {
                predefined_variables.previous_state = null; // this will disable the creation of this state attribute
            }
            if (par.state_type) {
                machineStateType = par.state_type;
            }
            var extras_theory_name = "pvsioweb_utils";
            var model = {
                descriptor: _this.print_descriptor(emuchart),
                name: emuchart.name, // Note: it is important to have the theory name identical to the file name -- otherwise PVSio refuses to evaluate commands!
                importings: emuchart.importings.concat(extras_theory_name),
                utils: _this.print_enter_exit(emuchart),
                //extras: Handlebars.compile(pvs_utils_template, { noEscape: true })(),
                constants: _this.print_constants(emuchart),
                modes: _this.print_states(emuchart),
                datatypes: _this.print_datatypes(emuchart),
                state_variables: _this.print_variables(emuchart),
                init: _this.print_initial_transition(emuchart),
                transitions: _this.print_transitions(emuchart),
                disclaimer: _this.print_disclaimer()
            };
            var theory = Handlebars.compile(pvs_theory_template, { noEscape: true })(model);

            var model_extras = {
                name: extras_theory_name, // Note: it is important to have the theory name identical to the file name -- otherwise PVSio refuses to evaluate commands!
                extras: Handlebars.compile(pvs_utils_template, { noEscape: true })()
            };
            var extras = Handlebars.compile(pvs_theory_template, { noEscape: true })(model_extras);
            if (theory) {
                var overWrite = {overWrite: true};
                var folder = "/pvs";
                projectManager.project().addFile(folder + "/" + emuchart.name + ".pvs", theory, overWrite).then(function (res){
                    projectManager.project().addFile(folder + "/" + extras_theory_name + ".pvs", extras, overWrite).then(function (res) {
                        resolve(true);
                    }).catch(function (err) {
                        console.log(err);
                        reject(err);
                    });
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            } else {
                console.error("Warning, PVS model could not be generated.");
            }
        }
        return new Promise (function (resolve, reject) {
            if (opt.interactive) {
                return get_params().then(function (par) {
                    resolve(finalize(resolve, reject, par));
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            }
            return resolve(finalize(opt));
        });
    };

    module.exports = EmuchartsPVSPrinter;
});
