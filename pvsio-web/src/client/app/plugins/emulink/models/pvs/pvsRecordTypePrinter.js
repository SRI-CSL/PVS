/**
 * @author Paolo Masci
 * @date 19/11/14 9:20:12 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global*/
define(function (require, exports, module) {
    "use strict";

    var pvsRecord, pvsTypeDefinition, pvsAssignmentExpression, pvsFieldDeclaration;

    function PVSRecordPrinter() {
        /**
         * Template pvsRecord expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsRecord", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsRecord =
            Handlebars.compile(require("text!./templates/pvsRecord.handlebars"));
        /**
         * Template pvsTypeDefinition expects context = { name: (string), value: (string) }
         */
        Handlebars.registerHelper("pvsTypeDefinition", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsTypeDefinition =
            Handlebars.compile(require("text!./templates/pvsTypeDefinition.handlebars"));
        /**
         * Template pvsAssignmentExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsAssignmentExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsAssignmentExpression =
            Handlebars.compile(require("text!./templates/pvsAssignmentExpression.handlebars"));
        /**
         * Template pvsFieldDeclaration expects context = [ ... ]
         */
        Handlebars.registerHelper("pvsFieldDeclaration", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        pvsFieldDeclaration =
            Handlebars.compile(require("text!./templates/pvsFieldDeclaration.handlebars"));
        return this;
    }

    function blanks(n) {
        var ans = "";
        if (n <= 0) { return ans; }
        while (n-- > 0) {
            ans += " ";
        }
        return ans;
    }
    var getValue = function (data) {
        if (!data.value) {
            console.log("Warning: value for " + data.name + " was undefined.");
            if (data.type.toLowerCase() === "bool" ||
                    data.type.toLowerCase() === "boolean") {
                return "false";
            }
            return "0";
        }
        return data.value;
    };
    var getType = function (data) {
        return data.type;
    };

    var tabsize = 4;
    function print_aux(data, f, g, opt) {
        function record_rec(data, depth) {
            depth++;
            var keys = Object.keys(data);
            if (keys.length === 0) { return ""; }
            var ans = "", i = 0;
            for (i = 0; i < keys.length; i++) {
                var key = keys[i];
                if (data[key].type === "variable") {
                    ans += f({
                        data: {
                            name: data[key].val.name,
                            value: (opt.isValue) ? getValue(data[key].val) : getType(data[key].val),
                            separator: (i < keys.length - 1) ? "," : "",
                            whitespace: (i < keys.length - 1) ? blanks(depth * tabsize) : ""
                        }
                    });
                } else { // data[key].type === "selector"
                    ans += f({
                        data: {
                            name: key,
                            value: record_rec(data[key].children, depth),
                            separator: (i < keys.length - 1) ? "," : "",
                            whitespace: (i < keys.length - 1) ? blanks(depth * tabsize) : ""
                        }
                    });
                }
            }
            ans = g({
                data: {
                    val: ans,
                    DL: (opt.isValue) ? "(#" : "[#", // delimiter left
                    DR: (opt.isValue) ? "#)" : "#]", // delimiter right
                    newlineAfterDL: (depth === 1) ? true : false,
                    newlineWithin: (depth === 1) ? true : false,
                    //newlineBeforeDelimiterRight: (depth === 1) ? true : false,
                    whiteSpace: (depth === 1) ? blanks(depth * tabsize) : " ",
                    whiteSpaceBeforeDR: (depth === 1) ? blanks(tabsize / 2) : " "
                }
            });
            return ans;
        }
        return record_rec(data, 0);
    }


    PVSRecordPrinter.prototype.printRecordValue = function (data) {
        return print_aux(data, pvsAssignmentExpression, pvsRecord, {isValue: true});
    };
    PVSRecordPrinter.prototype.printRecordType = function (data) {
        return print_aux(data, pvsFieldDeclaration, pvsRecord, {isType: true});
    };
    PVSRecordPrinter.prototype.printRecordAccessor = function (name) {
        return name.replace(new RegExp("\\.", "g"), "`");
    };

    /**
     * Translates an object state into a pvs record
     * @param state = { type: { name: (string), val: (string) } }
     */
    PVSRecordPrinter.prototype.printTypeDefinition = function (data) {
        return pvsTypeDefinition({
            data: {
                name: data.name,
                type: (typeof data.value === "string") ? data.value
                        : print_aux(data.value, pvsFieldDeclaration, pvsRecord, {isType: true})
            }
        });
    };

    module.exports = {
        create: function () {
            return new PVSRecordPrinter();
        }
    };
});
