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

    function ContextTable() {
        eventDispatcher(this);
        _this = this;
        return this;
    }

    ContextTable.prototype.addContextVariables = function(tableElements) {
        function installTableHandlers() {
            d3.selectAll("#StateAttributeUP").on("click", function () {
                var theNode = d3.select(this.parentNode.parentNode).node();
                var previous = d3.select(this.parentNode.parentNode.previousElementSibling).node();
                if (previous.id !== "heading") {
                    d3.select("#StateAttributes").select("tbody").node()
                        .removeChild(d3.select(this.parentNode.parentNode).node());
                    d3.select("#StateAttributes").select("tbody").node().insertBefore(theNode, previous);
                    d3.selectAll("#RemoveStateAttribute").on("click", function () {
                        d3.select("#StateAttributes").select("tbody").node()
                            .removeChild(d3.select(this.parentNode.parentNode).node());
                    });
                }
            });
            d3.selectAll("#StateAttributeDOWN").on("click", function () {
                function insertAfter(newNode, referenceNode) {
                    referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
                }
                var theNode = d3.select(this.parentNode.parentNode).node();
                var next = d3.select(this.parentNode.parentNode.nextElementSibling).node();
                if (next) {
                    d3.select("#StateAttributes").select("tbody").node()
                        .removeChild(d3.select(this.parentNode.parentNode).node());
                    insertAfter(theNode, next);
                    d3.selectAll("#RemoveStateAttribute").on("click", function () {
                        d3.select("#StateAttributes").select("tbody").node()
                            .removeChild(d3.select(this.parentNode.parentNode).node());
                    });
                }
            });
            d3.selectAll("#RemoveStateAttribute").on("click", function () {
                _this.removeContextVariable(this.parentElement.parentElement.id);
            });
            d3.select("#AddStateAttribute").on("click", function () {
                d3.select("#btn_menuNewVariable").node().click();
            });
            d3.selectAll("#EditStateAttribute").on("click", function () {
                _this.editContextVariable(this.parentElement.parentElement.id);
            });
        }
        function addElement(e) {
            var newVariable = d3.select("#StateVariableTemplate").node().cloneNode(true);
            newVariable.children[0].innerHTML = newVariable.name = e.name;
            newVariable.children[1].innerHTML = newVariable.name = e.type;
            newVariable.children[2].innerHTML = newVariable.value = e.value;
            newVariable.type = e.type;
            newVariable.id = e.id;
            newVariable.scope = e.scope;
            d3.select("#StateAttributes").select("tbody").node().appendChild(newVariable);
        }
        tableElements.forEach(function (e) {
            addElement(e);
        });
        installTableHandlers();
        return _this;
    };

    ContextTable.prototype.removeContextVariable = function(elementID) {
        var table = d3.select("#StateAttributes").select("tbody").node();
        var theVariable = document.getElementById(elementID);
        table.removeChild(theVariable);
        _this.fire({
            type: "ContextTable_deleteVariable",
            variable: {
                id: theVariable.id
            }
        });
        return _this;
    };

    ContextTable.prototype.editContextVariable = function(elementID) {
        var theVariable = document.getElementById(elementID);
        _this.fire({
            type: "ContextTable_editVariable",
            variable: {
                id: theVariable.id
            }
        });
        return _this;
    };

    ContextTable.prototype.setContextVariables = function(tableElements) {
        function clearTable() {
            var table = d3.select("#StateAttributes").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }
        clearTable();
        this.addContextVariables(tableElements);
        return _this;
    };

    module.exports = ContextTable;
});
