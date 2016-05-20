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
    
    function TransitionsTable() {
        d3.select("#AddTransition").on("click", function () {
            d3.select("#btn_menuNewTransition").node().click();
        });
        eventDispatcher(this);
        _this = this;
        return this;
    }
    
    TransitionsTable.prototype.addTransitions = function(tableElements) {
        function installTableHandlers() {
            d3.selectAll("#RemoveTransition").on("click", function () {
                _this.removeTransition(this.parentElement.parentElement.id);
            });
            d3.selectAll("#RenameTransition").on("click", function () {
                _this.renameTransition(this.parentElement.parentElement.id);
            });
            d3.selectAll("#TransitionLabel").on("click", function () {
                _this.renameTransition(this.parentElement.parentElement.id);
            });
        }        
        function addElement(e) {
            var newTransition = d3.select("#TransitionTemplate").node().cloneNode(true);
            newTransition.name = e.name;
            newTransition.source = e.source;
            newTransition.target = e.target;
            newTransition.id = e.id;
            newTransition.children[0].children[0].innerHTML = newTransition.name;
//            newTransition.children[0].children[0].style.width = (newTransition.name.length * 10) + "px";
            newTransition.children[1].children[0].innerHTML = newTransition.source.name;
            newTransition.children[2].children[0].innerHTML = newTransition.target.name;
            d3.select("#TransitionsTable").select("tbody").node().appendChild(newTransition);
        }
        tableElements.forEach(function (e) {
            addElement(e);
        });
        installTableHandlers();
        return _this;
    };
    
    TransitionsTable.prototype.removeTransition = function(elementID) {
        var table = d3.select("#TransitionsTable").select("tbody").node();
        var theTransition = d3.select("#Transitions").select("#" + elementID).node();
        table.removeChild(theTransition);
        _this.fire({
            type: "TransitionsTable_deleteTransition",
            transition: {
                id: theTransition.id
            }
        });
        return _this;
    };
        
    TransitionsTable.prototype.renameTransition = function(elementID) {
        var theTransition = d3.select("#TransitionsTable").select("#" + elementID).node();
        _this.fire({
            type: "TransitionsTable_renameTransition",
            transition: {
                id: theTransition.id
            }
        });
        return _this;
    };
        
    TransitionsTable.prototype.setTransitions = function(tableElements) {
        function clearTable() {
            var table = d3.select("#TransitionsTable").select("tbody").node();
            while (table.lastChild && table.lastChild.id !== "heading") {
                table.removeChild(table.lastChild);
            }
            return _this;
        }        
        clearTable();
        this.addTransitions(tableElements);
        return _this;
    };
    
    
    module.exports = TransitionsTable;
});
