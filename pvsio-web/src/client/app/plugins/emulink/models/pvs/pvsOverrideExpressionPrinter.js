/**
 * @author Paolo Masci
 * @date 19/11/14 7:29:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";

    var pvsOverrideExpression, pvsAssignmentExpression, pvsBindingExpression;

    function PVSOverrideExpressionPrinter() {
        /**
         * Template pvsOverrideExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsOverrideExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsOverrideExpression = Handlebars.compile(require("text!./templates/pvsOverrideExpression.handlebars"));
        /**
         * Template pvsAssignmentExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsAssignmentExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsAssignmentExpression = Handlebars.compile(require("text!./templates/pvsAssignmentExpression.handlebars"));
        /**
         * Template pvsBindingExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsBindingExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsBindingExpression = Handlebars.compile(require("text!./templates/pvsBindingExpression.handlebars"));
        return this;
    }

    function print_aux(data) {
        function override_rec(name, override, original, depth) {
            if (name.indexOf(".") < 0) {
                return pvsAssignmentExpression({
                    data: {
                        name: name,
                        value: override
                    }
                });
            }
            var names = name.split(".");
            var first = names.splice(0, 1);
            depth++;
            var accessor = original.split(".").splice(0, depth).join("`");
            var f = (depth === 1) ? pvsBindingExpression : pvsAssignmentExpression;
            return f({
                data: {
                    name: first,
                    value: pvsOverrideExpression({
                        data: {
                            name: accessor,
                            override: override_rec(names.join("."), override, original, depth)
                        }
                    })
                }
            });
        }
        return override_rec(data.name, data.override, data.name, 0);
    }

    /**
     * Translates an object state into a pvs record
     * @param data = { name: (string), override: (string) }
     */
    PVSOverrideExpressionPrinter.prototype.print = function (data) {
        return print_aux(data);
    };

    module.exports = {
        create: function () {
            return new PVSOverrideExpressionPrinter();
        }
    };
});
