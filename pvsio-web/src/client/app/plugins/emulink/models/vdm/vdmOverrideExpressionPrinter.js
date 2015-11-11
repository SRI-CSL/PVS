/**
 * @author Paolo Masci
 * @date 10/03/15 12:41:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global*/
define(function (require, exports, module) {
    "use strict";

    var vdmOverrideExpression, vdmAssignmentExpression, vdmBindingExpression;

    function VDMOverrideExpressionPrinter() {
        /**
         * Template vdmOverrideExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmOverrideExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmOverrideExpression = Handlebars.compile(require("text!./templates/vdmOverrideExpression.handlebars"));
        /**
         * Template vdmAssignmentExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmAssignmentExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmAssignmentExpression = Handlebars.compile(require("text!./templates/vdmAssignmentExpression.handlebars"));
        /**
         * Template vdmBindingExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmBindingExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmBindingExpression = Handlebars.compile(require("text!./templates/vdmBindingExpression.handlebars"));
        return this;
    }

    function print_aux(data) {
        function override_rec(name, override, original, depth) {
            if (name.indexOf(".") < 0) {
                return vdmAssignmentExpression({
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
            var f = (depth === 1) ? vdmBindingExpression : vdmAssignmentExpression;
            return f({
                data: {
                    name: first,
                    value: vdmOverrideExpression({
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
     * Translates an object state into a vdm record
     * @param data = { name: (string), override: (string) }
     */
    VDMOverrideExpressionPrinter.prototype.print = function (data) {
        return print_aux(data);
    };

    module.exports = {
        create: function () {
            return new VDMOverrideExpressionPrinter();
        }
    };
});
