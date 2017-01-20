/**
 *
 * @author Paolo Masci
 * @date 28/10/15
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var _this,
        eventDispatcher = require("util/eventDispatcher");

    function ConstantsTable() {
        eventDispatcher(this);
        _this = this;
        return this;
    }

    ConstantsTable.prototype.addConstant = function(tableElements) {
        function installTableHandlers() {
            d3.selectAll("#RemoveConstant").on("click", function () {
                _this.removeConstant(this.parentElement.parentElement.id);
            });
            d3.select("#AddConstant").on("click", function () {
                d3.select("#btn_menuNewConstant").node().click();
            });
            d3.selectAll("#EditConstant").on("click", function () {
                _this.editConstant(this.parentElement.parentElement.id);
            });
        }
        function addElement(e) {
            var newConstant = d3.select("#ConstantsTemplate").node().cloneNode(true);
            newConstant.children[0].innerHTML = newConstant.name = e.name;
            newConstant.children[1].innerHTML = newConstant.name = e.type;
            newConstant.children[2].innerHTML = newConstant.value = e.value;
            newConstant.type = e.type;
            newConstant.id = e.id;
            newConstant.scope = e.scope;
            d3.select("#ConstantsTable").select("tbody").node().appendChild(newConstant);
        }
        tableElements.forEach(function (e) {
            addElement(e);
        });
        installTableHandlers();
        return _this;
    };

    ConstantsTable.prototype.removeConstant = function(elementID) {
        var table = d3.select("#ConstantsTable").select("tbody").node();
        var theConstant = document.getElementById(elementID);
        table.removeChild(theConstant);
        _this.fire({
            type: "ConstantsTable_deleteConstant",
            constant: {
                id: theConstant.id
            }
        });
        return _this;
    };

    ConstantsTable.prototype.editConstant = function(elementID) {
        var theConstant = document.getElementById(elementID);
        _this.fire({
            type: "ConstantsTable_editConstant",
            constant: {
                id: theConstant.id
            }
        });
        return _this;
    };

    ConstantsTable.prototype.setConstants = function(tableElements) {
        function clearTable() {
            var table = d3.select("#ConstantsTable").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }
        clearTable();
        this.addConstant(tableElements);
        return _this;
    };

    module.exports = ConstantsTable;
});
