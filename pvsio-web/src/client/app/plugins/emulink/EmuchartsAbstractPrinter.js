/**
 * @author Patrick Oladimeji
 * @date 01/07/2015 14:52:38
 *
 * Abstract printer for emucharts model.
 * Other specific printers should inherit from this model.
 */
define(function (require, exports, module) {
    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var complexActions = ["expression", "assignment", "function"];

    function Printer(name) {
        this.modelName = name;
    }

    function definedValues(d) {
        return d !== null && d !== undefined && d !== "";
    }

    Printer.prototype.modelName = undefined;
    Printer.prototype._parser = new EmuchartsParser();

    Printer.prototype.getExpression = function (expression, emuchart) {
        var _this = this;
        if (expression === undefined || expression === null) {
            return "";
        }
        if (expression.type === "assignment") {
            var args = [
                expression.val.identifier.val,
                _this.getExpression(expression.val.binop, emuchart),
                _this.getExpression(expression.val.expression, emuchart)];
            return args.join(" ");
        } else {
            if (Array.isArray(expression.val)) {
                var res = expression.val.map(function (token) {
                    if (complexActions.indexOf(token.val) > -1) {
                        return _this.getExpression(token.val, emuchart);
                    } else {
                        return _this.getOperator(token.val, emuchart);
                    }
                });

                return res.join(" ");
            } else {
                if (complexActions.indexOf(expression.val) > -1) {
                    return _this.getExpression(expression.val, emuchart);
                } else {
                    return _this.getOperator(expression.val, emuchart);
                }
            }
        }
    };

    Printer.prototype.getOperator = function (op, emuchart) {
        return op;
    };

    Printer.prototype.getType = function (type, emuchart) {
        return type;
    };

    Printer.prototype.parseTransition = function (t, emuchart) {
        var _this = this;
        var name = t.name;
        var functionBody = _this._parser.parseTransition(name);
        var id, condition, actions;
        if (functionBody.res) {
            functionBody = functionBody.res.val;
            id = functionBody.identifier;
            condition = functionBody.cond;
            actions = functionBody.actions;
            if (condition) {
                condition = condition.val.map(function (token) {
                    return _this.getExpression(token, emuchart);
                }).join(" ");
            }
            //add the condition that enforces we are in the right state to carry out the actions
            if (t.source) {
                condition = [condition, "st.current_state " + _this.getOperator("==")
                             + " " + t.source.name ].filter(definedValues).join(" " + _this.getOperator("&&") +" ");
            }
            if (actions) {
                actions = actions.val.map(function (a) {
                    return _this.getExpression(a, emuchart);
                });
            }
            return {id: id.val, actions: actions, condition: condition, source: t.source, target: t.target};
        }
    };


    Printer.prototype.print_initial_transition = function (emuchart) {
        var _this = this;
        var trans = emuchart.initial_transitions,
            transitions = [];
        trans.forEach(function (t) {
            var parsedTransitions = _this.parseTransition(t, emuchart);
            if (parsedTransitions) {
                transitions.push(parsedTransitions);
            }
        });
        return transitions;
    };

    Printer.prototype.print_transitions = function (emuchart) {
        var _this = this;
        var transitions = [];
        emuchart.transitions.forEach(function (t) {
            var parsedTransition  = _this.parseTransition(t, emuchart);
            if (parsedTransition) {
                transitions.push(parsedTransition);
            }
        });
        return transitions;
    };

    module.exports = Printer;
});
