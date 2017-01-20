/** @module EmuchartsSelector */
/**
 *
 * @author Paolo Masci
 * @date Nov 17, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var _this,
        eventDispatcher = require("util/eventDispatcher");

    function EmuchartsSelector() {
        eventDispatcher(this);
        _this = this;
        return this;
    }

    EmuchartsSelector.prototype.addItem = function(toolbar) {
        function installSelectionHandlers() {
            d3.selectAll("#RemoveItem").on("click", function () {
                _this.removeDatatype(this.parentElement.parentElement.id);
            });
        }
        function addElement(e) {
            var newItem = d3.select("#SelectorTemplate").node().cloneNode(true);
            newItem.children[1].innerHTML = newItem.value = e.value;
            newItem.id = e.id;
            d3.select("#EmuchartsSelector").select("tbody").node().appendChild(newItem);
        }
        toolbar.forEach(function (e) {
            addElement(e);
        });
        installSelectionHandlers();
        return _this;
    };

    EmuchartsSelector.prototype.removeDatatype = function(elementID) {
        var table = d3.select("#EmuchartsSelector").select("tbody").node();
        var theDatatype = document.getElementById(elementID);
        table.removeChild(theDatatype);
        _this.fire({
            type: "EmuchartsSelector_deleteDatatype",
            datatype: {
                id: theDatatype.id
            }
        });
        return _this;
    };

    EmuchartsSelector.prototype.editDatatype = function(elementID) {
        var theDatatype = document.getElementById(elementID);
        _this.fire({
            type: "EmuchartsSelector_editDatatype",
            datatype: {
                id: theDatatype.id
            }
        });
        return _this;
    };

    EmuchartsSelector.prototype.setDatatypes = function(tableElements) {
        function clearTable() {
            var table = d3.select("#EmuchartsSelector").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }
        clearTable();
        this.addDatatype(tableElements);
        return _this;
    };

    module.exports = EmuchartsSelector;
});
