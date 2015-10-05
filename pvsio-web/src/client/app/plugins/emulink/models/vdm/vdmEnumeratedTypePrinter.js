/**
 * @author Paolo Masci
 * @date 10/03/15 12:41:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global*/
define(function (require, exports, module) {
    "use strict";
    var vdmEnumeratedType, vdmTypeDefinition;

    function VDMEnumeratedTypePrinter() {
        /**
         * Template
         */
        Handlebars.registerHelper("vdmEnumeratedType", function (context, options) {
            if (!context) { return ""; }
            var ans = "";
            var tmp = [];
            context.forEach(function (val) {
                tmp.push({ value: "<" + val + ">", separator: " | "});
            });
            tmp[tmp.length - 1].separator = "";
            tmp.forEach(function (name) {
                ans += options.fn(name);
            });
            return ans + ";";
        });
        vdmEnumeratedType =
            Handlebars.compile(require("text!./templates/vdmEnumeratedType.handlebars"));
        /**
         * Template vdmRecordTypeDefinition expects context = { name: (string), value: (string) }
         */
        Handlebars.registerHelper("vdmTypeDefinition", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmTypeDefinition = Handlebars.compile(require("text!./templates/vdmTypeDefinition.handlebars"));
        return this;
    }

    /**
     * Translates an object state into a vdm record
     * @param state = { type: { name: (string), value: (string) } }
     */
    VDMEnumeratedTypePrinter.prototype.fromArray = function (eName, eValues) {
        var x = eValues;
        if (typeof x === "object") {
            x = vdmEnumeratedType({data: x});
        }
        return vdmTypeDefinition({data: {name: eName, type: x}});
    };

    module.exports = {
        create: function () {
            return new VDMEnumeratedTypePrinter();
        }
    };
});
