/**
 * @author Patrick Oladimeji
 * @date 01/07/2015 14:52:38
 *
 * ADA printer for emucharts model.
 * The generated model has the following structure
 * package myModel is
 *  -- declare variables and function signatures
 *  TYPE MachineState is (...);
 *
 *  TYPE State is
 *      record
 *          current_state, previous_state: MachineState,
 *          --enumerate other properties that make up the state *
 *      end record;
 *
 *  function init (x: Float) return State;
 *  function enter (ms: MachineState, st: in out State) return State;
 *  function leave (ms: MachineState, st: in out State) return State;
 *
 *  --define the signature for all transition functions
 *  function foo (st: in out State) return State;
 * private
 *  --define any privat vars
 * end myModel;
 *
 * package body myModel is
 *  -- declare function implementations
 *  function foo (st: in out State) return State
 *  is
 *  begin
 *      --do something useful with/to state
 *      return st;
 *  end foo;
 * end myModel;
 */
define(function (require, exports, module) {
    var specTemplate = require("text!plugins/emulink/models/ada/templates/spec.handlebars");
    var bodyTemplate = require("text!plugins/emulink/models/ada/templates/body.handlebars");
    var AbstractPrinter = require("plugins/emulink/EmuchartsAbstractPrinter");

    var typeMaps = {
        "real": "Integer"
    };

    var operatorMaps = {
        "==": "=",
        "&&": "AND",
        "||": "OR"
    };

    function Printer(name) {
        AbstractPrinter.call(this, name);
        this.modelName = name;
        this.model = {modelName: name, transitions: []};

        AbstractPrinter.prototype.getOperator = this.getOperator;
        AbstractPrinter.prototype.getType = this.getType;
    }

    Printer.prototype = Object.create(AbstractPrinter.prototype);
    Printer.prototype.constructor = Printer;
    Printer.prototype.parentClass = AbstractPrinter.prototype;

    Printer.prototype.getOperator = function (op, emuchart) {
        function isMemberOfState(op) {
            return emuchart.variables.some(function (d) {
                return d.name === op;
            });
        }
        if (emuchart && isMemberOfState(op)) {
            return "st." + op;
        }
        return operatorMaps[op] || op;
    };

    Printer.prototype.getType = function (type, emuchart) {
        return typeMaps[type] || type;
    };

    Printer.prototype.print_descriptor = function (emuchart) {
        return "";
    };

    Printer.prototype.print_disclaimer = function (emuchart) {
        return "";
    };

    Printer.prototype.print_variables = function (emuchart) {
        var _this = this;
        if (emuchart.variables) {
            var vars = emuchart.variables.map(function (v) {
                v.type = _this.getType(v.type);
                return v;
            });

           this.model.variables = vars;
        }
        return "";
    };

    Printer.prototype.print_transitions = function (emuchart) {
        var transitions = this.parentClass.print_transitions(emuchart);
        if (transitions) {
            this.model.transitions = this.model.transitions.concat(transitions);
        }
        return "";
    };

    Printer.prototype.print_types = function (emuchart) {
        return "";
    };

    Printer.prototype.print_initial_transition = function (emuchart) {
        var transitions = this.parentClass.print_initial_transition(emuchart);
        if (transitions) {
            this.model.transitions = this.model.transitions.concat(transitions);
        }
        return "";
    };

    Printer.prototype.print_states = function (emuchart) {
        this.model.states = emuchart.states;
        return "";
    };

    Printer.prototype.print = function (emuchart) {
        this.model.transitions = [];
        this.print_variables(emuchart);
        this.print_transitions(emuchart);
        this.print_initial_transition(emuchart);
        this.print_states(emuchart);

        var spec = Handlebars.compile(specTemplate)(this.model);
        var body = Handlebars.compile(bodyTemplate)(this.model);
        return {spec: spec, body: body};
    };

    module.exports = Printer;
});
