/**
 * @author Paolo Masci
 * @date May 12, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var table_template = require("text!plugins/emulink/tools/tables/table_template.handlebars"),
        eventDispatcher = require("util/eventDispatcher");

    /**
     * @function GenericTable
     * @description Constructor.
     * @param name {String} A unique name for the table (this name will be used as table identifier in the DOM)
     * @param div {String} The ID of the div element where the table is to be attached,
     * @param attributes {Array} An array of objects in the form { name: (String) }, where name is the name of a column in the table.
     * @param add {function} Function for adding an element in the table
     * @memberof module:GenericTable
     * @instance
     */
    function GenericTable(name, div, attributes, add, opt) {
        opt = opt || {};
        this.table_name = name;
        this.div = div;
        this.attributes = attributes;
        this.add = add;
        this.opt = opt;
        return eventDispatcher(this);
    }

    GenericTable.prototype.createHtmlElements = function () {
        var _this = this;
        _this.clearListeners();
        if (!d3.select("#" + _this.table_name + "Table").node()) {
            var table = Handlebars.compile(table_template, { noEscape: true })({
                table_name: _this.table_name,
                attributes: _this.attributes
            });
            d3.select("#" + _this.div).append("div")
                .attr("id", _this.table_name + "Table")
                .attr("class", "table-editable EmuchartsTableContent")
                .style("display", (_this.opt.isVisible)? "block" : "none").html(table);
            d3.select("#Add" + _this.table_name).on("click", function () {
                return _this.fire({
                    type: _this.table_name + "Table_add"
                });
            });
        }
    };

    GenericTable.prototype.addElement = function(tableElements) {
        var _this = this;
        function installTableHandlers() {
            d3.selectAll("#Remove" + _this.table_name).on("click", function () {
                _this.removeElement(this.parentElement.parentElement.id.replace(_this.table_name, ""));
            });
            d3.selectAll("#Edit" + _this.table_name).on("click", function () {
                _this.editElement(this.parentElement.parentElement.id.replace(_this.table_name, ""));
            });
            d3.select("#Add" + _this.table_name).on("click", function () {
                return _this.fire({
                    type: _this.table_name + "Table_add"
                });
            });
        }
        tableElements.forEach(function (e) {
            var elem = _this.add(e);
            var t = d3.select("#" + _this.table_name + "Table tbody");
            if (t) {
                t.append("tr").attr("id", _this.table_name + e.id).html(elem);
            }
        });
        installTableHandlers();
        return this;
    };

    GenericTable.prototype.removeElement = function(elementID) {
        var _this = this;
        elementID = _this.table_name + elementID;
        var theElement = document.getElementById(elementID); //d3.select("#" + _this.table_name + "Table tbody").select("#" + elementID).node();
        var table = d3.select("#" + _this.table_name + "Table tbody").node();
        table.removeChild(theElement);
        return this.fire({
            type: _this.table_name + "Table_delete",
            id: theElement.id.replace(_this.table_name, "")
        });
    };

    GenericTable.prototype.editElement = function(elementID) {
        var _this = this;
        elementID = _this.table_name + elementID;
        var theElement = document.getElementById(elementID); //d3.select("#" + _this.table_name + "Table tbody").select("#" + elementID).node();
        return this.fire({
            type: _this.table_name + "Table_edit",
            id: theElement.id.replace(_this.table_name, "")
        });
    };

    GenericTable.prototype.setElements = function(tableElements) {
        var _this = this;
        function clearTable() {
            // d3.selectAll("#Remove" + _this.table_name).on("click", null);
            // d3.selectAll("#Edit" + _this.table_name).on("click", null);
            var table = d3.select("#" + _this.table_name + "Table tbody").node();
            while (table && table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
        }
        clearTable();
        return this.addElement(tableElements);
    };

    GenericTable.prototype.deselectAll = function () {
        var _this = this;
        var elems = d3.select("#" + _this.table_name + "Table tbody");
        if (elems.node()) {
            elems.selectAll("td").attr("style", "background-color:white; color:black;");
            this.fire({
                type: _this.table_name + "Table_deselectAll"
            });
        }
        return this;
    };

    GenericTable.prototype.deselect = function (elementID) {
        var _this = this;
        elementID = _this.table_name + elementID;
        var theElement = document.getElementById(elementID); //d3.select("#" + _this.table_name + "Table tbody").select("#" + elementID);
        if (theElement) {
            d3.select(theElement).selectAll("td").attr("style", "background-color:white; color:black;");
            this.fire({
                type: _this.table_name + "Table_deselect",
                id: elementID.replace(_this.table_name, "")
            });
        }
        return this;
    };

    GenericTable.prototype.select = function (elementID) {
        var _this = this;
        elementID = _this.table_name + elementID;
        var theElement = document.getElementById(elementID); //d3.select("#" + _this.table_name + "Table tbody").select("#" + elementID);
        if (theElement) {
            this.deselectAll();
            d3.select(theElement).selectAll("td").attr("style", "background-color:steelblue; color:white;");
            this.fire({
                type: _this.table_name + "Table_select",
                id: elementID.replace(_this.table_name, "")
            });
        }
        return this;
    };

    GenericTable.prototype.scrollTop = function (elementID) {
        var _this = this;
        elementID = _this.table_name + elementID;
        function getScrollHeight(elementID) {
            var height = 0;
            var children = d3.select("#" + _this.table_name + "Table tbody").node().children;
            for (var i = 1; i < children.length; i++) { // the first child is always the header
                if (children[i].id === elementID) {
                    return height;
                }
                height += children[i].scrollHeight;
            }
            return height;
        }
        function scrollTopTween(scrollTop) {
            return function() {
                var i = d3.interpolateNumber(this.scrollTop, scrollTop);
                return function(t) { this.scrollTop = i(t); };
            };
        }
        d3.select("#" + _this.table_name + "Table").transition()
            .duration(500)
            .tween("TTScrollTop", scrollTopTween(getScrollHeight(elementID)));
        return this;
    };

    module.exports = GenericTable;
});
