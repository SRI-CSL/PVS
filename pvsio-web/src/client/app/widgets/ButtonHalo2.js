/**
 * Draws halo over buttons when the button is clicked
 */
define(function (require, exports, module) {

    var instance;
    var d3        = require("d3/d3");

    function ButtonHalo(id, coords, opt) {
        opt = opt || {};
        this._keyCode2widget = {}; // this table stores information about the relation between keyCodes and widgets
        this._widgetsCoords = {}; // this table maintains info about the coordinate and size of the widgets
        this._evts = {};
        this._noHalo = {};
        return this;
    }

    ButtonHalo.prototype.installKeypressHandler = function (widget, opt) {
        opt = opt || {};
        if (opt.keyCode) { instance._keyCode2widget[opt.keyCode] = widget; }
        if (opt.coords) { instance._widgetsCoords[widget.id] = opt.coords; }
        if (opt.evts) { instance._evts[widget.id] = opt.evts; }
        if (opt.noHalo) { instance._noHalo[widget.id] = opt.noHalo; }
        if (d3.select("#btnSimulatorView").empty()) {
            d3.select(document).on("keydown", function () {
                instance.handleKeyDownEvent(d3.event);
            });
            d3.select(document).on("keyup", function () {
                instance.handleKeyUpEvent(d3.event);
            });
        }
    };

    ButtonHalo.prototype.removeKeypressHandlers = function () {
        instance._keyCode2widget = {};
        instance._widgetsCoords = {};
        instance._evts = {};
    };

    ButtonHalo.prototype.halo = function(widgetID) {
        var coords = instance._widgetsCoords[widgetID];
        if (coords) {
            var mark = d3.select(".animation-halo");
            if (mark.empty()) {
                //mark = d3.select("#imageDiv .prototype-image-inner").append("div").attr("class", "animation-halo");
                mark = d3.select("body").append("div").attr("class", "animation-halo");
            }

            var hrad = coords.width / 2;
            var vrad = coords.height / 2;
            var brad = hrad + "px " + vrad + "px ";

            mark.style("top", coords.top + "px").style("left", coords.left + "px")
                .style("width", coords.width + "px").style("height", coords.height + "px")
                .style("border-top-left-radius", brad).style("border-top-right-radius", brad)
                .style("border-bottom-left-radius", brad).style("border-bottom-right-radius", brad)
                .style("z-index", 9999);
        }
        return this;
    };

    ButtonHalo.prototype.haloOff = function(widgetID) {
        d3.select(".animation-halo").remove();
        return this;
    };

    ButtonHalo.prototype.handleKeyDownEvent = function (e) {
        var eventKeyCode = e.which;
        if (eventKeyCode) {
            if (!d3.select("#btnSimulatorView").node() || d3.select("#btnSimulatorView").classed("active")) {
                var widget = instance._keyCode2widget[eventKeyCode];
                if (widget) {
                    var id = (typeof widget.id === "function") ? widget.id() : widget.id;
                    var evts = instance._evts[id];
                    if (evts && (evts.click || typeof evts.indexOf === 'function' && evts.indexOf('click') > -1)) {
                        widget.click({ callback: widget.callback });
                        if (!instance._noHalo[id]) {
                            instance.halo(id);
                        }
                        d3.event.preventDefault();
                        d3.event.stopPropagation();
                    } else if (widget && evts && (evts["press/release"] || typeof evts.indexOf === 'function' && evts.indexOf("press/release") > -1)) {
                        widget.pressAndHold({ callback: widget.callback });
                        if (!instance._noHalo[id]) {
                            instance.halo(id);
                        }
                        d3.event.preventDefault();
                        d3.event.stopPropagation();
                    }
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
                    var id = (typeof widget.id === "function") ? widget.id() : widget.id;
                    var evts = instance._evts[id];
                    if (evts && (evts["press/release"] || typeof evts.indexOf === 'function' && evts.indexOf("press/release") > -1)) {
                        widget.release({ callback: widget.callback });
                    }
                    if (!instance._noHalo[id]) {
                        instance.haloOff(widget);
                    }
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
