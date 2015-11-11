/**
 * @author Paolo Masci
 * @date 10/03/15 12:41:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global*/
define(function (require, exports, module) {
    "use strict";

    var vdmType, vdmTypeDefinition, vdmAssignmentExpression, vdmFieldDeclaration;

    function VDMRecordPrinter() {
        /**
         * Template vdmType expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmType", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmType =
            Handlebars.compile(require("text!./templates/vdmType.handlebars"));
        /**
         * Template vdmTypeDefinition expects context = { name: (string), value: (string) }
         */
        Handlebars.registerHelper("vdmTypeDefinition", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmTypeDefinition =
            Handlebars.compile(require("text!./templates/vdmTypeDefinition.handlebars"));
        /**
         * Template vdmAssignmentExpression expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmAssignmentExpression", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmAssignmentExpression =
            Handlebars.compile(require("text!./templates/vdmAssignmentExpression.handlebars"));
        /**
         * Template vdmFieldDeclaration expects context = [ ... ]
         */
        Handlebars.registerHelper("vdmFieldDeclaration", function (context, options) {
            if (!context) { return ""; }
            return options.fn(context);
        });
        vdmFieldDeclaration =
            Handlebars.compile(require("text!./templates/vdmFieldDeclaration.handlebars"));
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


    VDMRecordPrinter.prototype.printRecordValue = function (data) {
        return print_aux(data, vdmAssignmentExpression, vdmType, {isValue: true});
    };
    VDMRecordPrinter.prototype.printRecordType = function (data) {
        return print_aux(data, vdmFieldDeclaration, vdmType, {isType: true});
    };
    VDMRecordPrinter.prototype.printRecordAccessor = function (name) {
        return name.replace(new RegExp("\\.", "g"), "`");
    };

    /**
     * Translates an object state into a vdm record
     * @param state = { type: { name: (string), val: (string) } }
     */
    VDMRecordPrinter.prototype.printTypeDefinition = function (data) {
        return vdmTypeDefinition({
            data: {
                name: data.name,
                type: (typeof data.value === "string") ? data.value
                        : print_aux(data.value, vdmFieldDeclaration, vdmType, {isType: true})
            }
        });
    };

    module.exports = {
        create: function () {
            return new VDMRecordPrinter();
        }
    };
});
