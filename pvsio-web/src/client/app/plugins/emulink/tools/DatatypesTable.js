/**
 *
 * @author Paolo Masci
 * @date August 2, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var _this,
        eventDispatcher = require("util/eventDispatcher");

    function installTableHandlers() {
        d3.selectAll("#RemoveDatatype").on("click", function () {
            _this.removeDatatype(this.parentElement.parentElement.id);
        });
        d3.select("#AddDatatype").on("click", function () {
            d3.select("#btn_menuNewDatatype").node().click();
        });
        d3.selectAll("#EditDatatype").on("click", function () {
            _this.editDatatype(this.parentElement.parentElement.id);
        });
    }

    function DatatypesTable() {
        eventDispatcher(this);
        installTableHandlers();
        _this = this;
        return this;
    }

    DatatypesTable.prototype.addDatatype = function(tableElements) {
        function addElement(e) {
            var newDatatype = d3.select("#DatatypeTemplate").node().cloneNode(true);
            newDatatype.children[0].innerHTML = newDatatype.name = e.name;
            newDatatype.constructors = e.constructors;
            newDatatype.children[1].innerHTML = newDatatype.constructors.join(", ");
            newDatatype.id = e.id;
            d3.select("#DatatypesTable").select("tbody").node().appendChild(newDatatype);
        }
        tableElements.forEach(function (e) {
            addElement(e);
        });
        installTableHandlers();
        return _this;
    };

    DatatypesTable.prototype.removeDatatype = function(elementID) {
        var table = d3.select("#DatatypesTable").select("tbody").node();
        var theDatatype = document.getElementById(elementID);
        table.removeChild(theDatatype);
        _this.fire({
            type: "DatatypesTable_deleteDatatype",
            datatype: {
                id: theDatatype.id
            }
        });
        return _this;
    };

    DatatypesTable.prototype.editDatatype = function(elementID) {
        var theDatatype = document.getElementById(elementID);
        _this.fire({
            type: "DatatypesTable_editDatatype",
            datatype: {
                id: theDatatype.id
            }
        });
        return _this;
    };

    DatatypesTable.prototype.setDatatypes = function(tableElements) {
        function clearTable() {
            var table = d3.select("#DatatypesTable").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }
        clearTable();
        this.addDatatype(tableElements);
        return _this;
    };

    module.exports = DatatypesTable;
});
