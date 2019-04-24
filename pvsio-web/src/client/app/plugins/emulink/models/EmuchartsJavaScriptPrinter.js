/**
 * @author Patrick Oladimeji
 * @date 28/06/2015 6:07:31 AM
 *
 * JavaScript printer for emucharts model.
 * The generated model contains a state variable which is populated with the variables of the emucharts model.
 * In addition, the state has two variables denoting state labels: these are 'currentState' and 'previousState',
 * both are of type 'string'.
 *
 * The generated model has the following structure
 * function () {
 *      //declaration of machine states
 *      var machineState = [];//array of strings reprensenting state labels of the model
 *      //an object containing the variables used in the model. This is really not needed as the real state of the
 *      //model is passed around in function parameters and initialised in the init function.
 *      var state = {currentState: '', previousState: ''};
 *      //utility function to enter and leave states in the model
 *      function enter(newStateLabel, st) {
 *          st.currentState = newStateLabel;
 *          return st;
 *      }
 *
 *      function leave(currentStateLabel, st) {
 *          st.previousState = currentStateLabel;
 *          return st;
 *      }
 *
 *      //initialisation function sets up the variables in the state with values
 *      function init() {
 *          var st = {
 *              currentState: '',
 *              previousState: '' //also include all other properties/variables in the model
 *          };
 *          return st;
 *      }
 *      //declare all other functions in the model and example function signature is shown below
 *      function foo(state) {
 *          //do something useful and update the state accordingly
 *          return state;
 *      }
 *
 *      //expose api for using model
 *      return {
 *          init: init//and include all other functions in the model
 *      };
 * }
 * variables are mapped to js variables
 * constants are also mapped to js variables
 *
 * transitions are mapped to Javascript functions
 * transitions affect variables and may change the state of the system.
 * transitions may have a condition attached to them
 * transitions may have a source and target state
 *
 * To get the details of the transition use EmuchartsParser.parseTransition
 */
define(function (require, exports, module) {

    var AbstractPrinter = require("plugins/emulink/models/EmuchartsAbstractPrinter");
    var functionTemplate = require("text!plugins/emulink/models/javascript/templates/function.handlebars"),
        variablesTemplate = require("text!plugins/emulink/models/javascript/templates/variables.handlebars"),
        initialisationTemplate = require("text!plugins/emulink/models/javascript/templates/initial-transition.handlebars"),
        modelTemplate = require("text!plugins/emulink/models/javascript/templates/model.handlebars"),
        stateTemplate = require("text!plugins/emulink/models/javascript/templates/states.handlebars");

    var allTransitions;
    var operatorOverrides = {
        ":=": "=",
        "AND": "&&",
        "OR": "||",
        "and": "&&",
        "or": "||",
        "=": "==="
    };

    function definedValues(d) {
        return d !== null && d !== undefined && d !== "";
    }

    function Printer(name) {
        AbstractPrinter.call(this, name);
        allTransitions = [];
    }

    Printer.prototype = Object.create(AbstractPrinter.prototype);
    Printer.prototype.constructor = Printer;
    Printer.prototype.parentClass = AbstractPrinter.prototype;


    Printer.prototype.getOperator = function (op) {
        return operatorOverrides[op] || op;
    };

    Printer.prototype.print_descriptor = function (emuchart) {
        return "//JS code generated from Emuchart";
    };

    Printer.prototype.print_disclaimer = function (emuchart) {
        return "//--MIT Licenced";
    };

    Printer.prototype.print_types =  function (emuchart) {
        //javacript doesnt do types
        return "";
    };

    Printer.prototype.print_transitions = function (emuchart) {
        var _this = this;
       //generate the javascript function
        var transitions = [], sourceData = {};
        emuchart.transitions.forEach(function (t) {
            var parsedTransition  = _this.parseTransition(t);
            if (parsedTransition) {
                transitions.push(parsedTransition);
            }
        });
        allTransitions = allTransitions.concat(transitions);
        sourceData = {transitions: transitions};
        return  Handlebars.compile(functionTemplate)(sourceData);
    };

    Printer.prototype.print_constants = function (emuchart) {
        var vars = emuchart.constants;
        var str = Handlebars.compile(variablesTemplate)(vars);
        return str;
    };

    Printer.prototype.print_variables = function (emuchart) {
        var vars = emuchart.variables;
        var extraVars = {}, declarations = [];
        //check if variable names contain dots and create additional object variables as needed
        vars.forEach(function (v) {
            if (v.name.indexOf(".") > -1) {
                var heirarchy = v.name.split(".");
                heirarchy.forEach(function (prop, i) {
                    if (i === 0) {
                        //we create an explicit variable for the first property
                        //all others are properties within this property
                        if (!extraVars[prop]) {
                            extraVars[prop] = "{}";
                        }
                    } else {
                        var decl = heirarchy.slice(0, i + 1).join(".") + "= {}";
                        declarations.push(decl);
                    }
                });
            }
        });

        var str = Object.keys(extraVars).map(function (name) {
            var v = extraVars[name];
            return "var " + name + " = " + v + ";";
        }).join("\n");
        str = str.concat("\n").concat(Handlebars.compile(variablesTemplate)(vars));
        //variables are really not needed since everything resides inthe state property
        return "";
    };

    function getVarsString (vars) {
        //recursively parses variables in a record format
        function getVars(vars) {
            var res = {}, value;
            Object.keys(vars).forEach(function (v) {
                if (vars[v].children) {
                    res[v] = getVars(vars[v].children);
                } else if (vars[v].val) {
                    value = vars[v].val.value;
                    res[v] = value !== undefined ? JSON.parse(vars[v].val.value) : value;
                }
            });
            return res;
        }

        var res = getVars(vars);
        //add the default states of current and previous states if not already there
        if (res.currentState === undefined) {
            res.currentState = "";
        }

        if (res.previousState === undefined) {
            res.previousState = "";
        }
        return JSON.stringify(res);
    }

    Printer.prototype.print_initial_transition = function (emuchart) {
        var _this = this;
        var trans = emuchart.initial_transitions,
            transitions = [];
        var vars = this._parser.parseVariables(emuchart.variables);

        trans.forEach(function (t) {
            var parsedTransitions = _this.parseTransition(t);
            if (parsedTransitions) {
                transitions.push(parsedTransitions);
            }
        });
        allTransitions = allTransitions.concat(transitions);
        var str = Handlebars.compile(initialisationTemplate)({transitions: transitions, vars: getVarsString(vars.res)});
        return str;
    };

    Printer.prototype.print_states = function (emuchart) {
        var states = emuchart.states;
        return Handlebars.compile(stateTemplate)(states);
    };

    Printer.prototype.print = function (emuchart) {
        var _this = this;
        var bodyParts = [
            this.print_constants,
            this.print_variables,
            this.print_types,
            this.print_states,
            this.print_initial_transition,
            this.print_transitions
        ];
        function printBody (emuchart) {
            var body = bodyParts.map(function (f) {
                return f.call(_this, emuchart);
            }).filter(definedValues).join("\n");
            var model = Handlebars.compile(modelTemplate)({functions: body, transitions: allTransitions, name: emuchart.name});
            return model;
        }

        var res = [this.print_descriptor, printBody, this.print_disclaimer].map(function (f) {
            return f.call(_this, emuchart);
        }).filter(definedValues).join("\n");
        return {res: res};
    };

    module.exports = Printer;
});
