/**
 *
 * @author Paolo Masci
 * @date May 13, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var Table = require("plugins/emulink/tools/GenericTable"),
        table_element = require("text!plugins/emulink/tools/tables/table_element.handlebars");

    var table;
    var table_name = "StateVariables";
    var attributes = [
        { name: "Variable Name" },
        { name: "Variable Type" },
        { name: "Initial Value" }
    ];
    function add(e) {
        return Handlebars.compile(table_element, { noEscape: true })({
            table_name: table_name,
            attributes: [
                { name: e.name },
                { name: e.type },
                { name: e.value }
            ]
        });
    }

    function StateVariablesTable(div) {
        if (!table) {
            table = new Table(table_name, div, attributes, add);
        }
        return table;
    }

    StateVariablesTable.prototype.createHtmlElements = function () {
        table.createHtmlElements();
    };

    module.exports = StateVariablesTable;
});
