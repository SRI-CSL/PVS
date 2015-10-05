/**
 * @author Paolo Masci
 * @date 20/11/14 4:28:11 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var pvsEnumeratedType, pvsTypeDefinition;

    function PVSEnumeratedTypePrinter() {
        /**
         * Template
         */
        Handlebars.registerHelper("pvsEnumeratedType", function (context, options) {
            if (!context) { return ""; }
            var ans = "";
            var tmp = [];
            context.forEach(function (val) {
                tmp.push({ value: val, separator: ", "});
            });
            tmp[tmp.length - 1].separator = "";
            tmp.forEach(function (name) {
                ans += options.fn(name);
            });
            return "{ " + ans + " }";
        });
        pvsEnumeratedType =
            Handlebars.compile(require("text!./templates/pvsEnumeratedType.handlebars"));
        /**
         * Template pvsTypeDefinition expects context = { name: (string), value: (string) }
         */
        Handlebars.registerHelper("pvsTypeDefinition", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsTypeDefinition = Handlebars.compile(require("text!./templates/pvsTypeDefinition.handlebars"));
        return this;
    }

    /**
     * Translates an object state into a pvs record
     * @param state = { type: { name: (string), value: (string) } }
     */
    PVSEnumeratedTypePrinter.prototype.fromArray = function (eName, eValues) {
        var x = eValues;
        if (typeof x === "object") {
            x = pvsEnumeratedType({data: x});
        }
        return pvsTypeDefinition({data: {name: eName, type: x}});
    };

    module.exports = {
        create: function () {
            return new PVSEnumeratedTypePrinter();
        }
    };
});
