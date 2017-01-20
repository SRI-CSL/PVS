/**
 * @author Paolo Masci
 * @date 04 September 2015
 *
 * ADA printer for emucharts models.
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
define(function (require, exports, module) {
    var threadTemplate = require("text!plugins/emulink/models/bless/templates/thread.handlebars");
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var displayNotificationView  = require("plugins/emulink/forms/displayNotificationView");
    var _parser = new EmuchartsParser();
    
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

    var machineStateType = "state";
    var initialMachineState = "initialMachineState";
    var predefined_variables = {
        previous_state: { name: "previous_state", type: machineStateType, value: initialMachineState },
        current_state: { name: "current_state", type: machineStateType, value: initialMachineState }
    };
    
    function getType (type) {
        var typeMaps = {
            "Time": "BLESS_Types::Time",
            "bool": "Base_Types::Boolean"
        };
        if (type.toLowerCase() === "bool" || type.toLowerCase() === "boolean") {
            type = "bool";
        }
        return typeMaps[type] || type;
    }

//    var constantsMap = {
//        "Initialization_Timeout": "Iso_Properties::Initialization_Timeout"
//    };
    
    function isLocalVariable(name, emuchart) {
        if (name === predefined_variables.current_state.name ||
                name === predefined_variables.previous_state.name) {
            return true;
        }
        if (emuchart.variables && emuchart.variables.local) {
            var i = 0;
            for (i = 0; i < emuchart.variables.local.length; i++) {
                if (name === emuchart.variables.local[i].name) {
                    return true;
                }
            }
        }
        return false;
    }
    function isInputVariable(name, emuchart) {
        if (emuchart.variables && emuchart.variables.input) {
            var i = 0;
            for (i = 0; i < emuchart.variables.input.length; i++) {
                if (name === emuchart.variables.input[i].name) {
                    return true;
                }
            }
        }
        return false;
    }
    function isOutputVariable(name, emuchart) {
        if (emuchart.variables && emuchart.variables.output) {
            var i = 0;
            for (i = 0; i < emuchart.variables.output.length; i++) {
                if (name === emuchart.variables.output[i].name) {
                    return true;
                }
            }
        }
        return false;
    }
    
    function getTerm(term, emuchart) {
        if (isInputVariable(term, emuchart)) {
            term = term + "?";
        } else if (isOutputVariable(term, emuchart)) {
            term = term + "!";
        }
        return term;
    }

    function parseTransition(t, emuchart) {        
        function getExpression(expression, emuchart) {
            var complexActions = ["expression", "assignment", "function"];
            if (expression === undefined || expression === null) {
                return "";
            }
            if (expression.type === "assignment") {
                var name = expression.val.identifier.val;
                if (isLocalVariable(name, emuchart)) {
                    return getTerm(name, emuchart) + " := " +
                            getExpression(expression.val.expression, emuchart);                
                }
                return getTerm(name, emuchart) + "(" +
                        getExpression(expression.val.expression, emuchart) + ")";
            } else {
                if (Array.isArray(expression.val)) {
                    var res = expression.val.map(function (token) {
                        if (complexActions.indexOf(token.val) > -1) {
                            return getExpression(token.val, emuchart);
                        } else {
                            return getTerm(token.val, emuchart);
                        }
                    });
                    return res.join(" ");
                } else {
                    if (complexActions.indexOf(expression.val) > -1) {
                        return getExpression(expression.val, emuchart);
                    } else {
                        return getTerm(expression.val, emuchart);
                    }
                }
            }
        }        
        var name = t.name;
        var functionBody = _parser.parseTransition(name);
        if (functionBody.res) {
            functionBody = functionBody.res.val;
            var id = functionBody.identifier;
            var condition = functionBody.cond;
            var actions = functionBody.actions;
            if (condition) {
                condition = condition.val.map(function (token) {
                    return getExpression(token, emuchart);
                }).join(" ");
            }
            if (actions) {
                actions = actions.val.map(function (a) {
                    return getExpression(a, emuchart);
                });
            }
            return {id: id.val, actions: actions, condition: condition, source: t.source, target: t.target};
        } else if (functionBody.err) {
            displayError(functionBody.err);
            return { erroneousLabel: name, parserError: functionBody.err };
        }
    }
    
    function Printer(name) {
        this.modelName = name;
        this.model = {modelName: name, transitions: []};
    }

    Printer.prototype.constructor = Printer;

    Printer.prototype.print_variables = function (emuchart) {
        if (emuchart.variables) {
            this.model.input_variables = emuchart.variables.input.map(function (v) {
                v.type = getType(v.type);
                return v;
            });
            this.model.output_variables = emuchart.variables.output.map(function (v) {
                v.type = getType(v.type);
                return v;
            });
            this.model.local_variables = emuchart.variables.local.map(function (v) {
                v.type = getType(v.type);
                return v;
            });
        }
    };

    Printer.prototype.print_transitions = function (emuchart) {
        var transitions = [];
        emuchart.transitions.forEach(function (t) {
            var parsedTransition  = parseTransition(t, emuchart);
            if (parsedTransition) {
                transitions.push(parsedTransition);
            }
        });
        if (transitions) {
            this.model.transitions = this.model.transitions.concat(transitions);
        }
    };
    
    Printer.prototype.print_initial_transition = function (emuchart) {
        var initial_transitions = emuchart.initial_transitions,
            transitions = [];
        initial_transitions.forEach(function (t) {
            var parsedInit = parseTransition(t, emuchart);
            if (parsedInit) {
                transitions.push(parsedInit);
            }
        });
        if (transitions) {
            this.model.initial_transitions = transitions;
        }
    };
    

    Printer.prototype.print_types = function (emuchart) {
    };

    Printer.prototype.print_states = function (emuchart) {
        this.model.states = emuchart.states;
    };
    
    Printer.prototype.print_descriptor = function (emuchart) {
        this.model.descriptor = 
            "-- ---------------------------------------------------------------" +
            "\n--  Model: " + emuchart.name;
        if (emuchart.author) {
            this.model.descriptor += 
                "\n--  Author: " + emuchart.author.name +
                "\n--          " + emuchart.author.affiliation +
                "\n--          " + emuchart.author.contact;
        }
        if (emuchart.description) {
            this.model.descriptor += 
                "\n-- ---------------------------------------------------------------" +
                "\n--  " + emuchart.description;
        }
        this.model.descriptor += 
            "\n-- ---------------------------------------------------------------\n";
    };
    
    Printer.prototype.print_disclaimer = function (emuchart) {
        this.model.disclaimer = "\n-- ---------------------------------------------------------------\n" +
                    "--  Bless/AADL model generated using PVSio-web BlessPrinter ver 0.1\n" +
                    "--  Tool freely available at http://www.pvsioweb.org" +
                    "\n-- ---------------------------------------------------------------\n";
    };
    

    Printer.prototype.print = function (emuchart) {
        this.model.transitions = [];
        this.print_variables(emuchart);
        this.print_transitions(emuchart);
        this.print_initial_transition(emuchart);
        this.print_states(emuchart);
        this.print_disclaimer(emuchart);
        this.print_descriptor(emuchart);

        var thread = Handlebars.compile(threadTemplate)(this.model);
        return {thread: thread};
    };

    module.exports = Printer;
});
