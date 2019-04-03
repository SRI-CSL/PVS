/**
 * @author Paolo Masci
 * @date May 12, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var Table = require("plugins/emulink/tools/GenericTable"),
        // contextMenus = require("plugins/emulink/menus/ContextMenus").getInstance(),
        table_element = require("text!plugins/emulink/tools/tables/table_element.handlebars");

    var table;
    var table_name = "MachineStates";
    var attributes = [
        { name: "Name" },
        { name: "Color Code" },
        { name: "Entry Actions" },
        { name: "Exit Actions" }
    ];
    function add(e) {
        return Handlebars.compile(table_element, { noEscape: true })({
            table_name: table_name,
            attributes: [
                { name: e.name },
                { name: e.color, box: true, backgroundColor: e.color, border: "1px", width: "20px" },
                { name: e.enter },
                { name: e.exit }
            ]
        });
    }

    function MachineStatesTable(div) {
        if (!table) {
            table = new Table(table_name, div, attributes, add, { isVisible: true });
        }
        return table;
    }

    MachineStatesTable.prototype.createHtmlElements = function () {
        table.createHtmlElements();
    };

    module.exports = MachineStatesTable;
});
