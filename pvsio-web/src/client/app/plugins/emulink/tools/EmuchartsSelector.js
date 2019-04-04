/** @module EmuchartsSelector */
/**
 *
 * @author Paolo Masci
 * @date Feb 03, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var instance = null,
        eventDispatcher = require("util/eventDispatcher"),
        selector_template = require("text!./emuchartselector/selector_template.handlebars");

    function EmuchartsSelector() {
        eventDispatcher(this);
        return this;
    }

    EmuchartsSelector.prototype.render = function(emucharts) {
        var _this = this;
        if (emucharts) {
            emucharts = emucharts.sort(function (a,b) {
                return a.emuchart_name < b.emuchart_name;
            });
            var html_element = Handlebars.compile(selector_template, { noEscape: true })({
                emucharts: emucharts.reverse()
            });
            d3.select("#EmuchartsSelector").html(html_element);
            // install handlers
            d3.select("#EmuchartsSelector").selectAll("a").on("click", function () {
                if (this.getAttribute("class") !== "selected") {
                    _this.fire({
                        type: "EmuchartsSelector_select",
                        emuchart: {
                            id: this.id
                        }
                    });
                }
            });
        }
        return this;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new EmuchartsSelector();
            }
            return instance;
        }
    };
});
