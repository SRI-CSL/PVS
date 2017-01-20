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

    function MachineStatesTable() {
        d3.select("#AddMachineState").on("click", function () {
            d3.select("#btn_menuNewState").node().click();
        });
        eventDispatcher(this);
        _this = this;
        return this;
    }

    MachineStatesTable.prototype.addMachineStates = function(tableElements) {
        function installTableHandlers() {
            d3.selectAll("#RemoveMachineState").on("click", function () {
                _this.removeMachineState(this.parentElement.parentElement.id);
            });
            d3.selectAll("#RenameMachineState").on("click", function () {
                _this.renameMachineState(this.parentElement.parentElement.id);
            });
            d3.selectAll("#MachineStateName").on("click", function () {
                _this.renameMachineState(this.parentElement.parentElement.id);
            });
            d3.selectAll("#MachineStateColor").on("click", function () {
                _this.changeStateColor(this.parentElement.parentElement.id);
            });
        }
        function addElement(e) {
            var newState = d3.select("#MachineStateTemplate").node().cloneNode(true);
            newState.name = e.name;
            newState.color = e.color;
            newState.id = e.id;
            newState.children[0].children[0].innerHTML = newState.name;
//            newState.children[0].children[0].style.width = (newState.name.length * 10) + "px";
            newState.children[1].children[0].style.backgroundColor = newState.color;
            d3.select("#MachineStates").select("tbody").node().appendChild(newState);
        }
        tableElements.forEach(function (e) {
            addElement(e);
        });
        installTableHandlers();
        return _this;
    };

    MachineStatesTable.prototype.removeMachineState = function(elementID) {
        var table = d3.select("#MachineStates").select("tbody").node();
        var theState = d3.select("#MachineStates").select("#" + elementID).node();
        table.removeChild(theState);
        _this.fire({
            type: "MachineStatesTable_deleteState",
            state: {
                id: theState.id
            }
        });
        return _this;
    };

    MachineStatesTable.prototype.renameMachineState = function(elementID) {
        var theState = d3.select("#MachineStates").select("#" + elementID).node();
        _this.fire({
            type: "MachineStatesTable_renameState",
            state: {
                id: theState.id
            }
        });
        return _this;
    };

    MachineStatesTable.prototype.changeStateColor = function(elementID) {
        var theState = d3.select("#MachineStates").select("#" + elementID).node();
        _this.fire({
            type: "MachineStatesTable_changeStateColor",
            state: {
                id: theState.id
            }
        });
        return _this;
    };

    MachineStatesTable.prototype.setMachineStates = function(tableElements) {
        function clearTable() {
            var table = d3.select("#MachineStates").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }
        clearTable();
        this.addMachineStates(tableElements);
        return _this;
    };


    module.exports = MachineStatesTable;
});
