/** @module EmuchartsGenericPrinter */
/**
 * EmuchartsGenericPrinter processes an emucharts diagram and creates the data structures necessary for code/model generation
 * @author Paolo Masci
 * @date May 29, 2017
 * Emucharts objects have the following structure:
      emuchart = {
                name: (string),
                author: {
                    name: (string),
                    affiliation: (string),
                    contact: (string)
                },
                importings: (not used for now),
                datatypes: (array of { -- only enumerated types are supported for now
                                name: (string), // the datatype identifier
                                constructors: array of { string }, // the datatype constructors
                            }),
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

The model printed by the Generic Printer has the following structure (the emuchart structure
is mostly maintained, the main different is that triggers are now integrated into a hierarchy,
variables are properly flagged (using a boolean isVariable), and expressions, and constants
are post-processed using specific functions provided by language-specific printers):

 model = {
    enter_leave: {
        current_mode: {
            name: (string),
            type: (string),
            value: (string)
        },
        previous_mode: {
            name: (string),
            type: (string),
            value: (string)
        },
        entry_actions: array of actions --- TODO
        exit_actions: array of actions --- TODO
    }
    constants: array of {
        name: (string)
        type: (string)
        value: (string)
    }
    modes: {
        type: (string), -- type of the state attribute used for storing mode information
        modes: array of { string } -- enumerated constants listing possible modes
    }
    datatypes: array of {
        name: (string) -- name of the datatype
        constructors: array of { string } -- enumerated constants listing the datatype constructors
    }
    state_variables: array of {
        name: (string), -- the variable identifier
        type: (string), -- the variable type
        value: (string) -- the initial value of the variable
    },
    init: {
        name: (string)
        override: array of {
            cond: expression -- this expression is generated from the transition label using emuchartsParser2.parseCondition
            actions: array of {
              variable_type: (string))
              variable_name: (string)
              override_expression: expression -- this expression is generated from the transition label using emuchartsParser2.parseAction
            }
    },
    triggers: {
        functions: {
            name: (string) -- the function name
            cases: array of {
            cond: expression -- this expression is generated from the transition label using emuchartsParser2.parseCondition
            actions: array of { actions } -- the actions are generated from the transition label using emuchartsParser2.parseAction
            from: (string) -- mode name from which the transition originates
            to: (string) -- mode name where the transition goes
        }
    }
    model_name: emuchart.name
    // disclaimer: this.print_disclaimer()
};

***
An example generic model printed by the Generic Printer is as follows:

{
 "enter_leave": {
  "comment": "entry/leave actions",
  "entry_actions": [],
  "leave_actions": [],
  "current_mode": {
   "name": "mode",
   "type": "Mode",
   "value": "off"
  },
  "previous_mode": {
   "name": "previous_mode",
   "type": "Mode",
   "value": "off"
  },
  "state_type": "State",
  "enter": "enter",
  "leave": "leave"
 },
 "constants": [
  {
   "name": "maxDisp",
   "type": "real",
   "value": "10"
  },
  {
   "name": "minDisp",
   "type": "real",
   "value": "0"
  }
 ],
 "modes": {
  "comment": "modes of operation",
  "type": "Mode",
  "modes": [
   "off",
   "on"
  ]
 },
 "datatypes": {
  "comment": "user defined datatypes",
  "datatypes": [
   {
    "id": "DATATYPE_LED",
    "name": "LED",
    "constructors": [
     "green",
     "yellow",
     "red"
    ]
   },
   {
    "id": "DATATYPE_Power",
    "name": "Power",
    "constructors": [
     "ON",
     "OFF"
    ]
   }
  ]
 },
 "state_variables": {
  "comment": "state attributes",
  "state_type": "State",
  "variables": [
   {
    "name": "mode",
    "type": "Mode",
    "value": "off"
   },
   {
    "name": "previous_mode",
    "type": "Mode",
    "value": "off"
   },
   {
    "name": "display",
    "type": "real"
   }
  ]
 },
 "init": {
  "comment": "initialisation function",
  "name": "init",
  "override": [
   {
    "cond": "",
    "actions": [
     {
      "variable_type": "real",
      "variable_name": "display",
      "override_expression": "1"
     }
    ]
   }
  ],
  "variables": [
   {
    "name": "previous_mode",
    "type": "Mode",
    "value": "off"
   },
   {
    "name": "mode",
    "type": "Mode",
    "value": "off"
   },
   {
    "name": "display",
    "type": "real",
    "value": "0"
   }
  ],
  "INIT_WITH_OVERRIDES": true
 },
 "triggers": {
  "comment": "triggers",
  "functions": [
   {
    "name": "tick",
    "cases": [
     {
      "identifier": "tick",
      "cond": "mode(st) = off",
      "actions": [],
      "label": "tick",
      "from": "off",
      "to": "on"
     }
    ]
   },
   {
    "name": "turn_off",
    "cases": [
     {
      "identifier": "turn_off",
      "cond": "mode(st) = on",
      "actions": [],
      "label": "turn_off",
      "from": "on",
      "to": "off"
     }
    ]
   }
  ]
 },
 "model_name": "emucharts_MedtronicMinimed530GSafe"
}

*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";
    // var printer_version = "2.2";
    // var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var EmuchartsParser2 = require("plugins/emulink/EmuchartsParser2");
    // var projectManager = require("project/ProjectManager").getInstance();
    var displayAskParameters = require("plugins/emulink/forms/displayAskParameters");
    var NotificationManager = require("project/NotificationManager");

    // var parser;
    var parser2;

    var undef = "UNINITIALIZED";
    var mode_type = "Mode";
    var state_type = "State";
    var predefined_variables = {
        previous_mode: { name: "previous_mode", type: mode_type, value: undef },
        current_mode: { name: "mode", type: mode_type, value: undef }
    };
    var predefined_functions = { leave: "leave", enter: "enter" };

    /**
     * Constructor
     */
    function EmuchartsGenericPrinter(opt) {
        opt = opt || {};
        // parser = EmuchartsParser.getInstance();
        parser2 = EmuchartsParser2.getInstance();
        return this;
    }

    function get_current_mode(emuchart) {
        return {
            name: predefined_variables.current_mode.name,
            type: predefined_variables.current_mode.type,
            value: (emuchart.initial_transitions.length === 1) ? emuchart.initial_transitions[0].target.name : predefined_variables.current_mode.value
        };
    }
    function get_previous_mode(emuchart) {
        if (predefined_variables && predefined_variables.previous_mode) {
            return {
                name: predefined_variables.previous_mode.name,
                type: predefined_variables.previous_mode.type,
                value: (emuchart.initial_transitions.length === 1) ? emuchart.initial_transitions[0].target.name : predefined_variables.current_mode.value
            };
        }
        return null;
    }

    /**
     * Gets Emuchart modes
     */
    EmuchartsGenericPrinter.prototype.get_modes = function (emuchart) {
        if (emuchart && emuchart.states) {
            return {
                comment: "modes of operation",
                type: mode_type,
                modes: emuchart.states.map(function (state) {
                    return state.name;
                })
            };
        }
        return null;
    };

    /**
     * Gets user-defined datatypes
     */
    EmuchartsGenericPrinter.prototype.get_datatypes = function (emuchart) {
        if (emuchart && emuchart.datatypes && emuchart.datatypes.length > 0) {
            return {
                comment: "user defined datatypes",
                datatypes: emuchart.datatypes
            };
        }
        return null;
    };

    function split_variables(emuchart) {
        var basic = [];
        var records = d3.map(); //hashtable used for grouping together variables in the form l1.a l1.b --- these will be part of the same record type.
                                //   key: name (string); value: Array of { l1_name: (string), l2_name: (string), type: (string) }
        if (emuchart.variables) {
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
        }
        return {
            basic: basic,
            records: records
        };
    }

    /**
     * Gets Emuchart variables
     */
    EmuchartsGenericPrinter.prototype.get_variables = function (emuchart, opt) {
        opt = opt || {};
        var variables = [];
        variables.push(get_current_mode(emuchart));
        if (predefined_variables.previous_mode) {
            variables.push(get_previous_mode(emuchart));
        }
        var x = split_variables(emuchart);
        x.basic.forEach(function (basic) {
            variables.push({ name: basic.name, type: basic.type, value: basic.value });
        });
        x.records.keys().forEach(function (key) {
            variables.push({
                name: key,
                children: x.records.get(key),
                type: "record"
            });
        });
        var data = {
            comment: "state attributes",
            state_type: state_type,
            variables: variables
        };
        // post-processing
        if (opt.convert_variable && typeof opt.convert_variable === "function") {
            data.variables = data.variables.map(function (v) {
                return opt.convert_variable(v);
            });
        }
        return data;
    };

    /**
     * Gets utility functions used in Emuchart
     */
    EmuchartsGenericPrinter.prototype.get_enter_leave = function (emuchart, opt) {
        function process_actions(actions, state, opt) {
            opt = opt || {};
            var x = (opt.enter) ? state.enter : state.exit;
            var trigger = parser2.parseTrigger("{" + x + "}");
            if (trigger.res && trigger.res.val) {
                trigger = trigger.res.val;
                // var trigger_id = (trigger.identifier.type === "identifier") ?
                //                     trigger.identifier
                //                     : { type: "identifier", val: trigger.identifier };
                var trigger_actions = [];
                if (trigger.actions) {
                    trigger.actions.trim().split(";").forEach(function (action) {
                        if (action.trim() !== "") {
                            var a = parser2.parseAction(action);
                            if (a) {
                                if (a.err) {
                                    console.error(a.err);
                                    NotificationManager.error(a.err);
                                } else if (a.res && a.res.val && a.res.val){
                                    trigger_actions.push(shapeAction(a.res.val.val, emuchart));
                                }
                            }
                        }
                    });
                }
                actions.push({
                    state: state.name,
                    action_sequence: trigger_actions
                });
            }
        }
        setVariables(parser2, emuchart);
        var entry_actions = null, leave_actions = null;
        var states_with_entry_actions = emuchart.states.filter(function (state) {
            return state.enter && state.enter !== "";
        });
        if (states_with_entry_actions.length > 0) {
            entry_actions = [];
            states_with_entry_actions.forEach(function (state) {
                process_actions(entry_actions, state, { enter: true });
            });
        }
        var states_with_leave_actions = emuchart.states.filter(function (state) {
            return state.exit && state.exit !== "";
        });
        if (states_with_leave_actions.length > 0) {
            leave_actions = [];
            states_with_leave_actions.forEach(function (state) {
                process_actions(leave_actions, state, { leave: true });
            });
        }
        var data = {
            comment: "entry/leave actions",
            entry_actions: entry_actions || [],
            leave_actions: leave_actions || [],
            current_mode: get_current_mode(emuchart),
            previous_mode: get_previous_mode(emuchart),
            state_type: state_type,
            enter: (opt.enter) ? opt.enter : predefined_functions.enter,
            leave: (opt.leave) ? opt.leave : predefined_functions.leave
        };
        if (predefined_variables.previous_mode) {
            data.previous_mode = predefined_variables.previous_mode;
        }
        // post-processing
        if (opt.convert_expression && typeof opt.convert_expression === "function") {
            data.entry_actions = data.entry_actions.map(function (entry_action) {
                entry_action.action_sequence = entry_action.action_sequence.map(function (action) {
                    action.override_expression = opt.convert_expression(action.override_expression, emuchart, { variable_type: action.variable_type });
                    return action;
                });
                return entry_action;
            });
            data.leave_actions = data.leave_actions.map(function (exit_action) {
                exit_action.action_sequence = exit_action.action_sequence.map(function (action) {
                    action.override_expression = opt.convert_expression(action.override_expression, emuchart, { variable_type: action.variable_type });
                    return action;
                });
                return exit_action;
            });
        }
        // if (opt.convert_variable && typeof opt.convert_variable === "function") {
        //     data.variables = data.variables.map(function (v) {
        //         return opt.convert_variable(v);
        //     });
        // }

        return data;
    };

    function setVariables (parser, emuchart) {
        var variables = [ get_current_mode(emuchart) ].concat(emuchart.variables.map(function (v) {
            return { name: v.name, type: v.type };
        }));
        if (predefined_variables.previous_mode) {
            variables = variables.concat({ name: predefined_variables.previous_mode.name, type: predefined_variables.previous_mode.type });
        }
        parser.setVariables(variables);
    }

    function shapeAction (action, emuchart) {
        if (action) {
            if (action.identifier.isVariable) {
                var x = action.identifier.val.split(".");
                if (x.length === 2) {
                    return {
                        variable_type: action.identifier.variableType,
                        variable_name: x[0],
                        variable_name_l2: x[1],
                        override_expression: action.expression.val
                    };
                }
                return {
                    variable_type: action.identifier.variableType,
                    variable_name: x[0],
                    override_expression: action.expression.val
                };
            } else {
                return {
                    // variable_type: getVariableType(action.identifier), FIXME: need to find a way to identify the type, e.g., use the type of the first term in the override expression
                    local_binding: true,
                    variable_name: action.identifier.val,
                    override_expression: action.expression.val
                };
            }
        }
        return action;
    }

    /**
     * Prints PVS definitions for Emuchart initial transitions
     */
    EmuchartsGenericPrinter.prototype.get_initial_transition = function (emuchart, opt) {
        opt = opt || {};
        if (emuchart.initial_transitions && emuchart.initial_transitions.length > 0) {
            setVariables(parser2, emuchart);
            // the name of the initial transition is always init, regardless of what the emucharts says
            var theTransition = { name: "init", override: [], variables: [] };
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
                if (initial_transition && initial_transition.name && initial_transition.name !== "") {
                    if (initial_transition.name.trim().startsWith("[") === false ||
                            initial_transition.name.trim().startsWith("{") === false) {
                        if (initial_transition.name.indexOf(";") > 0){
                            // it's a sequence of actions, we need to adorn them with curly braces
                            initial_transition.name = "{" + initial_transition.name + "}";
                        }
                    }
                    var ans = parser2.parseTrigger(initial_transition.name);
                    if (ans.res) {
                        var actions = [];
                        if (ans.res.val && ans.res.val.actions) {
                            ans.res.val.actions.trim().split(";").forEach(function (action) {
                                var action_data = parser2.parseAction(action);
                                if (action_data.res && action_data.res.val && action_data.res.val.type === "assignment") {
                                    actions.push(shapeAction(action_data.res.val.val));
                                }
                            });
                        }
                        if (ans.res.val.cond) {
                            if (emuchart.initial_transitions.length > 1) {
                                // we need to force the override of current_mode when the emuchart has multiple initial transitions
                                // because each one of them will lead to a different initial state
                                actions = actions || [];
                                actions = actions.concat({
                                    variable_name: predefined_variables.current_mode.name,
                                    override_expression: initial_transition.target.name
                                });
                            }
                            // convert_expression will take care of printing variable names and operators according to the pvs syntax
                            // option attach_state instructs convert_expression to concatenate (st) to variable names
                            theTransition.override.push({
                                cond: ans.res.val.cond.val,
                                actions: actions
                            });
                            has_cond = true;
                        } else {
                            if (actions && actions.length > 0) {
                                theTransition.override.push({
                                    cond: ["true"],
                                    actions: actions
                                });
                            }
                        }
                    } else {
                        console.error(ans.err);
                    }
                }
            });
            var data = {
                comment: "initialisation function",
                name: theTransition.name,
                override: theTransition.override,
                variables: [get_current_mode(emuchart)].concat(theTransition.variables)
            };
            if (predefined_variables.previous_mode) {
                data.variables = [get_previous_mode(emuchart)].concat(data.variables);
            }
            if (emuchart.initial_transitions.length === 1 && theTransition.override.length === 0) {
                data.DEFAULT_INIT = true;
            } else if (theTransition.override.length > 0 && !has_cond) {
                data.INIT_WITH_OVERRIDES = true;
            } else {
                data.INIT_MULTI = true;
            }
            // post-processing
            if (opt.convert_expression && typeof opt.convert_expression === "function") {
                data.override = data.override.map(function (override) {
                    override.cond = opt.convert_expression(override.cond, emuchart);
                    override.actions = override.actions.map(function (action) {
                        action.override_expression = opt.convert_expression(action.override_expression, emuchart, { variable_type: action.variable_type });
                        return action;
                    });
                    return override;
                });
            }
            if (opt.convert_variable && typeof opt.convert_variable === "function") {
                data.variables = data.variables.map(function (v) {
                    return opt.convert_variable(v);
                });
            }
            return data;
        }
        return null;
    };

    /**
     * Prints definitions for Emuchart transitions given in the form transition [condition] {actions}
     */
    EmuchartsGenericPrinter.prototype.get_transitions = function (emuchart, opt) {
        opt = opt || {};
        // multiple transitions can have the same identifier
        // (because the same transition can originate from different nodes)
        // this keeps track of the transitions we've already processed -- needed to avoid duplicates
        var done = d3.map();

        if (emuchart.transitions && emuchart.transitions.length > 0) {
            setVariables(parser2, emuchart);
            var transitions = [];
            // parse transition labels, to extract transition name, guard, and actions
            emuchart.transitions.forEach(function (t) {
                // add here mode updates
                if (!t.name) { t.name = "tick"; }
                var trigger = parser2.parseTrigger(t.name);
                if (trigger.res && trigger.res.val) {
                    trigger = trigger.res.val;
                    var trigger_id = (trigger.identifier.type === "identifier") ?
                                        trigger.identifier
                                        : { type: "identifier", val: trigger.identifier };
                    var trigger_cond = predefined_variables.current_mode.name + "=" + t.source.name;
                    if (trigger.cond) {
                        trigger_cond += " && (" + trigger.cond.trim() + ")";
                    }
                    trigger_cond = parser2.parseCondition(trigger_cond);
                    var trigger_actions = [];
                    if (trigger.actions) {
                        trigger.actions.trim().split(";").forEach(function (action) {
                            if (action.trim() !== "") {
                                var a = parser2.parseAction(action);
                                if (a) {
                                    if (a.err) {
                                        console.error(a.err);
                                        NotificationManager.error(a.err);
                                    } else if (a.res && a.res.val && a.res.val){
                                        trigger_actions.push(shapeAction(a.res.val.val, emuchart));
                                    }
                                }
                            }
                        });
                    }
                    transitions.push({
                        identifier: trigger_id,
                        cond: (trigger_cond && trigger_cond.err) ? trigger_cond.err
                                : (trigger_cond && trigger_cond.res) ? trigger_cond.res
                                : null,
                        actions: trigger_actions,
                        label: t.name,
                        from: t.source.name,
                        to:   t.target.name
                    });
                }
            });
            // theFunction is an array of objects representing pvs functions.
            // Each object is in the form: { name: (string), permission: (string), actions: (Array of strings) }
            var theFunction = [];
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
                        var data = {
                            identifier: transition.identifier.val,
                            cond: transition.cond.val,
                            actions: transition.actions,
                            label: transition.label,
                            from: transition.from,
                            to: transition.to
                        };
                        transitionFunction.cases.push(data);
                    }
                });
                // store results for the specific transition
                theFunction.push(transitionFunction);
            });
            var data = {
                comment: "triggers",
                functions: theFunction
            };
            if (opt.convert_condition && typeof opt.convert_condition === "function"
                 || opt.convert_expression && typeof opt.convert_expression === "function") {
                data.functions = data.functions.map(function(f) {
                    f.cases = f.cases.map(function (cc) {
                        cc.cond = (opt.convert_condition) ? opt.convert_condition(cc.cond, emuchart) : opt.convert_expression(cc.cond, emuchart);
                        if (opt.convert_expression) {
                            cc.actions = cc.actions.map(function (action) {
                                action.value = opt.convert_expression(action.override_expression, emuchart, { variable_type: action.variable_type });
                                return action;
                            });
                        }
                        return cc;
                    });
                    return f;
                });
            }
            return data;
        }
        return null;
    };
    EmuchartsGenericPrinter.prototype.get_triggers = function (emuchart, opt) {
        return this.get_transitions(emuchart, opt);
    };

    /**
     * Gets Emuchart constants
     */
    EmuchartsGenericPrinter.prototype.get_constants = function (emuchart, opt) {
        opt = opt || {};
        if (emuchart.constants && emuchart.constants.length > 0) {
            var data = emuchart.constants.map(function (c) {
                return {
                    name: c.name,
                    type: c.type,
                    value: c.value
                };
            });
            // post-processing
            if (opt.convert_constant) {
                data = emuchart.constants.map(function (c) {
                    return opt.convert_constant(c);
                });
            }
            return data;
        }
        return null;
    };

    EmuchartsGenericPrinter.prototype.print = function (emuchart, opt) {
        opt = opt || {};
        var model = {
            enter_leave: this.get_enter_leave(emuchart, opt),
            constants: this.get_constants(emuchart, opt),
            modes: this.get_modes(emuchart, opt),
            datatypes: this.get_datatypes(emuchart, opt),
            state_variables: this.get_variables(emuchart, opt),
            init: this.get_initial_transition(emuchart, opt),
            triggers: this.get_transitions(emuchart, opt),
            model_name: emuchart.name
            // disclaimer: this.print_disclaimer()
        };
        console.log(JSON.stringify(model, null, " "));
        return model;
    };

    EmuchartsGenericPrinter.prototype.parseCondition = function (cond) {
        return parser2.parseCondition(cond);
    };
    EmuchartsGenericPrinter.prototype.parseAction = function (cond) {
        return parser2.parseAction(cond);
    };

    // utility function for getting basic printer options
    EmuchartsGenericPrinter.prototype.get_params = function() {
        return new Promise (function (resolve, reject) {
            displayAskParameters.create({
                header: "Printer Options",
                params: [{
                    id: "current_mode",
                    name: "Current system mode",
                    value: predefined_variables.current_mode.name,
                    inputbox: true
                },{
                    id: "previous_mode",
                    name: "Previous system mode",
                    value: (predefined_variables.previous_mode) ? predefined_variables.previous_mode.name : "",
                    inputbox: true
                },{
                    id: "mode_type",
                    name: "System mode type",
                    value: predefined_variables.current_mode.type,
                    inputbox: true
                }],
                buttons: ["Cancel", "Ok"]
            }).on("ok", function (e, view) {
                view.remove();
                var par = ({
                    current_mode: e.data.labels.get("current_mode"),
                    previous_mode: e.data.labels.get("previous_mode"),
                    state_type: e.data.labels.get("state_type")
                });
                if (par.current_mode) {
                    predefined_variables.current_mode.name = par.current_mode;
                }
                if (par.previous_mode) {
                    predefined_variables.previous_mode = {
                        name: par.previous_mode,
                        type: mode_type,
                        value: undef
                    };
                } else {
                    predefined_variables.previous_mode = null; // this will disable the creation of this state attribute
                }
                if (par.state_type) {
                    state_type = par.state_type;
                }
                resolve(par);
            }).on("cancel", function (e, view) {
                // just remove window
                view.remove();
            });
        });
    };

    module.exports = EmuchartsGenericPrinter;
});
