/**
 * Draws halo over buttons when the button is clicked
 */
define(function (require, exports, module) {

    var instance;
    var d3        = require("d3/d3");

    function ButtonHalo() {
        this._keyCode2widget = {}; // this table stores information about the relation between keyCodes and widgets
        return this;
    }

    ButtonHalo.prototype.installKeypressHandler = function (widget, eventKeyCode) {
        instance._keyCode2widget[eventKeyCode] = widget;
        if (d3.select("#btnSimulatorView").empty()) {
            var _this = this;
            d3.select(document).on("keydown", function () {
                _this.handleKeyDownEvent(d3.event);
            });
            d3.select(document).on("keyup", function () {
                _this.handleKeyUpEvent(d3.event);
            });
        }
    };

    ButtonHalo.prototype.removeKeypressHandlers = function () {
        instance._keyCode2widget = {};
    };

    function halo (buttonID) {
        if (d3.select("." + buttonID).node()) {
            var coords = d3.select("#" + buttonID).attr("coords");
            coords = coords.split(",");
            var pos = {x1: +coords[0], y1: +coords[1], x2: +coords[2], y2: coords[3]};
            var w = pos.x2 - pos.x1, hrad = w / 2, h = pos.y2 - pos.y1, vrad = h / 2, brad = hrad + "px " + vrad + "px";
            var mark = d3.select(".animation-halo");
            if (mark.empty()) {
                mark = d3.select("#imageDiv .prototype-image-inner").append("div").attr("class", "animation-halo");
                // mark = d3.select("body").append("div").attr("class", "animation-halo");
            }
            mark.style("top", pos.y1 + "px").style("left", pos.x1 + "px")
                .style("width", (pos.x2 - pos.x1) + "px").style("height", (pos.y2 - pos.y1) + "px")
                .style("border-top-left-radius", brad).style("border-top-right-radius", brad)
                .style("border-bottom-left-radius", brad).style("border-bottom-right-radius", brad);
        }
    }
    function haloOff (buttonID) {
        if (d3.select("." + buttonID).node()) {
            d3.select(".animation-halo").remove();
        }
    }

    ButtonHalo.prototype.handleKeyDownEvent = function (e) {
        var eventKeyCode = e.which;
        if (eventKeyCode) {
            if (!d3.select("#btnSimulatorView").node() || d3.select("#btnSimulatorView").classed("active")) {
                var widget = instance._keyCode2widget[eventKeyCode];
                if (widget && typeof widget.evts === "function" && widget.evts().indexOf('click') > -1) {
                    widget.click({ callback: widget.callback });
                    halo(widget.id());
                    d3.event.preventDefault();
                    d3.event.stopPropagation();
                } else if (widget && typeof widget.evts === "function" && widget.evts().indexOf("press/release") > -1) {
                    widget.pressAndHold({ callback: widget.callback });
                    halo(widget.id());
                    d3.event.preventDefault();
                    d3.event.stopPropagation();
                }
            }
        }
        return this;
    };

    ButtonHalo.prototype.handleKeyUpEvent = function (e) {
        var eventKeyCode = e.which;
        if (eventKeyCode) {
            if (!d3.select("#btnSimulatorView").node() || d3.select("#btnSimulatorView").classed("active")) {
                var widget = instance._keyCode2widget[eventKeyCode];
                if (widget) {
                    if (typeof widget.evts === "function" && widget.evts().indexOf("press/release") > -1) {
                        widget.release({ callback: widget.callback });
                    }
                    haloOff(widget.id());
                    d3.event.preventDefault();
                    d3.event.stopPropagation();
                }
            }
        }
        return this;
    };

    module.exports = {
        getInstance: function () {
            instance = instance || new ButtonHalo();
            return instance;
        }
    };
});
