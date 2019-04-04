/**
 * @module WidgetManager
 * @desc WidgetManager deals with interacting with user interface widgets used for prototyping picture based uis.
 * @author Patrick Oladimeji
 * @date 10/30/13 21:42:56 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, _ */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        uidGenerator    = require("util/uuidGenerator"),
        EditWidgetView  = require("pvsioweb/forms/editWidget"),
        BaseWidgetManager  = require("pvsioweb/BaseWidgetManager"),
        Button          = require("widgets/Button"),
        TouchscreenButton  = require("widgets/TouchscreenButton"),
        TouchscreenDisplay = require("widgets/TouchscreenDisplay"),
        LED             = require("widgets/LED"),
        BasicDisplay    = require("widgets/BasicDisplay"),
        NumericDisplay  = require("widgets/NumericDisplay"),
        // Storyboard      = require("pvsioweb/Storyboard"),
        EmuTimer        = require("widgets/EmuTimer"),
        StateParser     = require("util/PVSioStateParser"),
        ButtonActionsQueue = require("widgets/ButtonActionsQueue").getInstance(),
        PreferenceKeys  = require("preferences/PreferenceKeys"),
        Preferences     = require("preferences/PreferenceStorage").getInstance();
    var wm;

   ///TODO this should be moved out of this file and promoted to a property, or a function parameter in createImageMap
    function renderResponse(err, res) {
        if (!err) {
            var state = StateParser.parse(res.data[0]);
            wm.getAllWidgets().forEach(function (w) {
                w.render(state);
            });
        } else {
            if (err.failedCommand && err.failedCommand.indexOf("tick(") === 0) {
                wm.stopTimers();
                console.log("wallclock paused (tick function not implemented in the selected prototype)");
            }
        }
    }
    function createImageMap(widget) {
        if (widget.needsImageMap()) {
            widget.createImageMap({
                callback: renderResponse
            });
        }
    }
    function handleTimerEdit(emuTimer, wm) {
        EditWidgetView.create(emuTimer)
            .on("ok", function (e, view) {
                view.remove();
                emuTimer.updateWithProperties(e.data);
                // fire event widget created
                wm.trigger("TimerModified", { action: "create", timer: emuTimer });
            }).on("cancel", function (e, view) {
                view.remove();
            });
    }
    function halo (buttonID) {
        var coords = d3.select("." + buttonID).attr("coords");
        coords = coords.split(",");
        var pos = {x1: +coords[0], y1: +coords[1], x2: +coords[2], y2: coords[3]};
        var w = pos.x2 - pos.x1, hrad = w / 2, h = pos.y2 - pos.y1, vrad = h / 2, brad = hrad + "px " + vrad + "px";
        var mark = d3.select(".animation-halo");
        if (mark.empty()) {
            mark = d3.select("#imageDiv .prototype-image-inner").append("div").attr("class", "animation-halo");
        }
        mark.style("top", pos.y1 + "px").style("left", pos.x1 + "px")
            .style("width", (pos.x2 - pos.x1) + "px").style("height", (pos.y2 - pos.y1) + "px")
            .style("border-top-left-radius", brad).style("border-top-right-radius", brad)
            .style("border-bottom-left-radius", brad).style("border-bottom-right-radius", brad);
    }
    function haloOff (buttonID) {
        d3.select(".animation-halo").remove();
    }

    /**
        @class WidgetManager
        @classdesc WidgetManager deals with interacting with user interface widgets used for prototyping picture based uis.
    */
    /**
     * Instantiates a new WidgetManager
     * @private
     */
    function WidgetManager() {
        this._widgets = {};
        this._timers = {};
        this._keyCode2widget = {}; // this table stores information about the relation between keyCodes and widgets
        Preferences.addListener("preferenceChanged", function (e) {
            if (e.key === "WALL_CLOCK_INTERVAL" && wm._timers.tick) {
                var timerRate = Preferences.get(PreferenceKeys.WALL_CLOCK_INTERVAL) * 1000;
                wm._timers.tick.updateInterval(timerRate);
                console.log("tick timer interval updated to " + timerRate / 1000 + " secs");
            }
        });
        return this;
    }

    function createWidget(w) {
        var widget = null;
        var x = w.x, y = w.y, height = w.height, width = w.width, scale = w.scale;
        w.type = w.type.toLowerCase();
        if (w.type === "button") {
            widget = new Button(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { callback: renderResponse,
                  keyCode: w.keyCode,
                  keyName: w.keyName,
                  functionText: w.functionText,
                  customFunctionText: w.customFunctionText,
                  visibleWhen: w.visibleWhen,
                  evts: w.evts,
                  buttonReadback: w.buttonReadback });
        } else if (w.type === "display") {
            widget = new BasicDisplay(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { displayKey: w.displayKey,
                  auditoryFeedback: w.auditoryFeedback,
                  visibleWhen: w.visibleWhen,
                  fontsize: w.fontsize,
                  fontColor: w.fontColor,
                  backgroundColor: w.backgroundColor,
                  parent: "imageDiv .prototype-image-inner" });
        } else if (w.type === "numericdisplay") {
            widget = new NumericDisplay(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { displayKey: w.displayKey,
                  cursorName: w.cursorName,
                  auditoryFeedback: w.auditoryFeedback,
                  visibleWhen: w.visibleWhen,
                  fontsize: w.fontsize,
                  fontColor: w.fontColor,
                  backgroundColor: w.backgroundColor,
                  parent: "imageDiv .prototype-image-inner" });
        } else if (w.type === "touchscreendisplay") {
            widget = new TouchscreenDisplay(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { callback: renderResponse,
                  displayKey: w.displayKey,
                  cursorName: w.cursorName,
                  auditoryFeedback: w.auditoryFeedback,
                  visibleWhen: w.visibleWhen,
                  functionText: w.functionText,
                  fontsize: w.fontsize,
                  fontColor: w.fontColor,
                  backgroundColor: w.backgroundColor,
                  parent: "imageDiv .prototype-image-inner" });
        } else if (w.type === "touchscreenbutton") {
            widget = new TouchscreenButton(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { callback: renderResponse,
                  functionText: w.functionText,
                  softLabel: w.softLabel,
                  auditoryFeedback: w.auditoryFeedback,
                  visibleWhen: w.visibleWhen,
                  fontsize: w.fontsize,
                  fontColor: w.fontColor,
                  backgroundColor: w.backgroundColor,
                  parent: "imageDiv .prototype-image-inner" });
        } else if (w.type === "led") {
            widget = new LED(w.id,
                { top: y * scale, left: x * scale, width: width * scale, height: height * scale },
                { ledKey: w.ledKey,
                  color: w.ledColor,
                  visibleWhen: w.visibleWhen,
                  parent: "imageDiv .prototype-image-inner" });
        } else {
            console.log("Warning: unrecognised widget type " + w.type);
        }
        return widget;
    }

    WidgetManager.prototype = Object.create(BaseWidgetManager.prototype);

    WidgetManager.prototype.initialiseWidgets = function () {
        ButtonActionsQueue.sendINIT(renderResponse);
        return this;
    };

    /**
        Restores the widget definitions passed in the parameter.
        @param {Object} defs JSOn specification for the widget definitions to restore
        @memberof WidgetManager
     */
    WidgetManager.prototype.restoreWidgetDefinitions = function (defs) {
        var wm = this;
        this.clearWidgets();
        if (defs && defs.widgetMaps && defs.regionDefs) {
            // sanity check
            if (defs.widgetMaps.length !== defs.regionDefs.length) {
                // there's an issue with the file -- a widget is not associated to any region
                console.log("WARNING: Corrupted widgets definition file. Fixing the definitions...");
                var curruptedRegionDefs = [];
                defs.regionDefs.forEach(function (regionDef) {
                    if (defs.widgetMaps.filter(function (map) { return map.id === regionDef.class; }).length === 0) {
                        console.log("Found offending region definition: " + regionDef.class);
                        curruptedRegionDefs.push(regionDef.class);
                    }
                });
                var curruptedWidgetMaps = [];
                defs.widgetMaps.forEach(function (map) {
                    if (defs.regionDefs.filter(function (def) { return def.class === map.id; }).length === 0) {
                        console.log("Found offending widget map: " + map.displayKey + " (id: " + map.id + ")");
                        curruptedWidgetMaps.push(map.id);
                    }
                });
                // removing corrupted elements
                curruptedRegionDefs.forEach(function (x) {
                    defs.regionDefs = defs.regionDefs.filter(function (d) { return d.class !== x; });
                });
                curruptedWidgetMaps.forEach(function (x) {
                    defs.widgetMaps = defs.widgetMaps.filter(function (d) { return d.id !== x; });
                });
                if (defs.widgetMaps.length === defs.regionDefs.length) {
                    console.log("Definitions fixed successfully!");
                } else {
                    console.error("Failed to fix the widget definitions :((");
                }
            }
            wm._keyCode2widget = {};
            var widget;
            _.each(defs.widgetMaps, function (w, i) {
                defs.regionDefs = defs.regionDefs || [];
                var coords = ((i < defs.regionDefs.length) && defs.regionDefs[i].coords) ? defs.regionDefs[i].coords.split(",") : [0,0,0,0];
                w.height = parseFloat(coords[3]) - parseFloat(coords[1]);
                w.width  = parseFloat(coords[2]) - parseFloat(coords[0]);
                w.x = parseFloat(coords[0]);
                w.y = parseFloat(coords[1]);
                w.scale = (d3.select("#imageDiv svg > g").node()) ?
                             +(d3.select("#imageDiv svg > g").attr("transform").replace("scale(", "").replace(")", "")) || 1 : 1;
                var widget = createWidget(w);
                if (widget) {
                    wm.addWidget(widget);
                    if (typeof widget.keyCode === "function" && widget.keyCode() && widget.type() === "button") {
                        wm._keyCode2widget[widget.keyCode()] = widget;
                    }
                }
            });

            //create div
            if (defs.regionDefs) {
                defs.regionDefs.forEach(function (d) {
                    widget = wm.getWidget(d["class"]);
                    var coords = d.coords.split(",").map(function (d) {
                        return parseFloat(d);
                    });
                    var h = parseFloat(coords[3]) - parseFloat(coords[1]),
                        w = parseFloat(coords[2]) - parseFloat(coords[0]),
                        x = parseFloat(coords[0]),
                        y = parseFloat(coords[1]);
                    var coord = {x: x, y: y, width: w, height: h};
                    wm.trigger("WidgetRegionRestored", widget, coord);
                    createImageMap(widget);
                });
            }
        }
    };
    WidgetManager.prototype.handleKeyDownEvent = function (e) {
        // d3.select(document).on("keydown", function () {
        if (d3.select("#btnSimulatorView").node() && d3.select("#btnSimulatorView").classed("active")) {
            var eventKeyCode = d3.event.which;
            var widget = wm._keyCode2widget[eventKeyCode];
            if (widget && typeof widget.evts === "function" && widget.evts().indexOf('click') > -1) {
                widget.click({ callback: renderResponse });
                halo(widget.id());
                d3.event.preventDefault();
                d3.event.stopPropagation();
            } else if (widget && typeof widget.evts === "function" && widget.evts().indexOf("press/release") > -1) {
                widget.pressAndHold({ callback: renderResponse });
                halo(widget.id());
                d3.event.preventDefault();
                d3.event.stopPropagation();
            }
        }
        // });
    };
    WidgetManager.prototype.handleKeyUpEvent = function (e) {
        // d3.select(document).on("keyup", function () {
        if (d3.select("#btnSimulatorView").node() && d3.select("#btnSimulatorView").classed("active")) {
            var eventKeyCode = d3.event.which;
            var widget = wm._keyCode2widget[eventKeyCode];
            if (widget && typeof widget.evts === "function" && widget.evts().indexOf("press/release") > -1) {
                widget.release({ callback: renderResponse });
            }
            haloOff();
        }
        // });
    };
    WidgetManager.prototype.addWallClockTimer = function () {
        //pop up the timer edit dialog
        var id = "tick";
        var timerRate = Preferences.get(PreferenceKeys.WALL_CLOCK_INTERVAL) * 1000;
        var emuTimer = new EmuTimer(id, { timerEvent: id, timerRate: timerRate, callback: renderResponse });
        this._timers[emuTimer.id()] = emuTimer;
        // fire event widget created
        wm.trigger("TimerModified", { action: "create", timer: emuTimer });

    };
    WidgetManager.prototype.editTimer = function (emuTimer) {
        handleTimerEdit(emuTimer, wm);
    };

    /**
        Edits the specified widget.
        @param {Widget} widget The widget to be edited.
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.editWidget = function (widget, data) {
        widget.updateWithProperties(data);
        widget.updateStyle(data);
        widget.renderSample({ visibleWhen: "true" });
        //create an interactive image area only if there isnt one already
        createImageMap(widget);
        if (data.keyCode) {
            wm._keyCode2widget[data.keyCode] = widget;
        }
        // fire event widget modified
        wm.trigger("WidgetModified");
    };
    WidgetManager.prototype.editTimer = function (emuTimer) {
        // the only timer type supported in the current implementation is EmuTimer
        handleTimerEdit(emuTimer, wm);
    };
    WidgetManager.prototype.startTimers = function () {
        _.each(this._timers, function (timer) {
            timer.start();
        });
    };
    WidgetManager.prototype.stopTimers = function () {
        _.each(this._timers, function (timer) {
            timer.stop();
        });
    };

    /**
     * Creates a new widget and adds it to the WidgetManager
     * @param {object} data Data object from a NewWidgetView callback
     * @param {object} coord Object with top, left, width, height properties specifying the widget position
     * @param {function} onCreate Called once the widget has been created, but before it is added to the manager
     * @return {Widget} The new widget
     */
    WidgetManager.prototype.addNewWidget = function (data, coord, onCreate) {
        var name = data.functionText || data.displayKey || data.ledKey || "";
        data.id = data.type + "_" + name + "_" + uidGenerator();
        // the ID should not contain "."
        data.id = data.id.replace(/\./g,"-");

        data.height = coord.height;
        data.width = coord.width;
        data.x = coord.left;
        data.y = coord.top;
        data.visibleWhen = (!data.visibleWhen || data.visibleWhen.length === 0) ? "true" : data.visibleWhen;

        var widget = createWidget(data);
        if (widget) {
            if (onCreate) {
                onCreate(widget, renderResponse);
            }
            widget.updateWithProperties(data);
            this.addWidget(widget);
            if (typeof widget.keyCode === "function" && widget.keyCode() && widget.type() === "button") {
                wm._keyCode2widget[widget.keyCode()] = widget;
            }
            this.trigger("WidgetModified", {action: "create", widget: widget});
        }
        return widget;
    };

    /**
        Removes all the widgets on the interface
     */
    WidgetManager.prototype.clearWidgets = function () {
        _.each(this._widgets, function (value) {
            value.remove();//remove the widgets from the interface
        });
        this._widgets = {};
        require("widgets/ButtonHalo").getInstance().removeKeypressHandlers();
    };

    /**
        Gets a list of all display widgets loaded on the page.
        @returns {BasicDisplay[]}
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.getAllDisplays = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "display" || w.type() === "numericdisplay" || w.type() === "touchscreendisplay";
        });
    };
    /**
        Gets a list of all display widgets loaded on the page.
        @returns {BasicDisplay[]}
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.getDisplayWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "display";
        });
    };
    /**
        Gets a list of all display widgets loaded on the page.
        @returns {NumericDisplay[]}
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.getNumericDisplayWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "numericdisplay";
        });
    };
    /**
        Gets a list of all LED widgets loaded on the page.
        @returns {LED[]}
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.getLEDWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "led";
        });
    };
    /**
        Gets a list of all button widgets loaded on the page.
        @returns {Button[]}
        @memberof WidgetManager
     */
    WidgetManager.prototype.getButtonWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "button";
        });
    };
    /**
        Gets a list of all button widgets loaded on the page.
        @returns {TouchscreenButton[]}
        @memberof WidgetManager
     */
    WidgetManager.prototype.getTouchscreenButtonWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "touchscreenbutton";
        });
    };
    /**
        Gets a list of all button widgets loaded on the page.
        @returns {TouchscreenDisplay[]}
        @memberof WidgetManager
     */
    WidgetManager.prototype.getTouchscreenDisplayWidgets = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "touchscreendisplay";
        });
    };
    // /**
    //     Gets a list of storyboard widgets
    //     @returns {Storyboard}
    //     @memberof WidgetManager
    //  */
    // WidgetManager.prototype.getStoryboardWidgets = function () {
    //     return _.filter(this._widgets, function (w) {
    //         return w.type() === "storyboard";
    //     });
    // };

    /**
        Gets a list of all the widgets loaded on the page. The returned array contains all
        widget types
        @returns {Widget[]}
        @memberof WidgetManager
    */
    WidgetManager.prototype.getAllWidgets = function () {
        // return an array sorted by widget type
        return this.getDisplayWidgets()
            .concat(this.getNumericDisplayWidgets())
            .concat(this.getTouchscreenDisplayWidgets())
            .concat(this.getTouchscreenButtonWidgets())
            .concat(this.getButtonWidgets())
            .concat(this.getLEDWidgets());
    };
    WidgetManager.prototype.getAllTimers = function () {
        return _.filter(this._widgets, function (w) {
            return w.type() === "timer";
        });
    };

    /**
     * Returns a JSON object representing widget definitions for the currently open project
       @memberof WidgetManager
     */
    WidgetManager.prototype.getWidgetDefinitions = function () {
        var scale = (d3.select("#imageDiv svg > g").node()) ?
                        +(d3.select("#imageDiv svg > g").attr("transform").replace("scale(", "").replace(")", "")) || 1 : 1;
        var widgets = [], regionDefs = [];
        _.each(this._widgets, function (widget) {
            widgets.push(widget.toJSON());
            var a = widget.imageMap();
            if (a) {
                var scaledCoords = a.attr("coords").split(",").map(function (c) {
                    return +c / scale;
                }).join(",");
                regionDefs.push({"class": a.attr("class"), shape: a.attr("shape"),
                            coords: scaledCoords, href: a.attr("href")});
            }
        });
        return {widgetMaps: widgets, regionDefs: regionDefs};
    };


    module.exports = {
        /**
         * Returns a singleton instance of the WidgetManager
         */
        getWidgetManager: function () {
            if (!wm) {
                wm = new WidgetManager();
            }
            return wm;
        }
    };
});
