/**
 * @module PumpMonitorDisplay
 * @version 1.0
 * @description Renders a basic pump monitor display, including: current value, label, alarms, units, tracings.
 *              This module provide APIs for changing the look and feel of the display element, such as position, 
 *              font color, size, and tracings speed.
 * @author Paolo Masci
 * @date May 24, 2015
 *
 * @example <caption>Typical use of PumpMonitorDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses PumpMonitorDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.vtbi = new PumpMonitorDisplay("vtbi",
 *                         { top: 56, left: 164, height: 30, width: 100 });
 *     device.vtbi.render(10); // the display renders 10
 *     device.vtbi.alarm("ALARM"); // the display shows an alarm
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    var SingleDisplay = require("widgets/SingleDisplay");
    var TracingsDisplay = require("widgets/medical/TracingsDisplayEVO");
    var d3 = require("d3/d3");

    /**
     * @function <a name="PumpMonitorDisplay">PumpMonitorDisplay</a>
     * @description Constructor.
     * @param id {String} The ID of the display.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 200, height: 80 }.
     * @param opt {Object} Options:
     *          <li>backgroundColor (String): background display color (default is black, "#000")</li>
     *          <li>font (String): display font type (default is "sans-serif")</li>
     *          <li>fontColor (String): display font color (default is white, "#fff")</li>
     *          <li>align (String): text alignment (default is "center")</li>
     *          <li>inverted (Bool): if true, the text has inverted colors, 
     *              i.e., fontColor becomes backgroundColor, and backgroundColor becomes fontColor (default is false)</li>
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>                
     * @memberof module:PumpMonitorDisplay
     * @instance
     */
    function PumpMonitorDisplay(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.label = opt.label || id;
        this.parent = (opt.parent && opt.parent !== "body") ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;
        this.font = [this.height, "px ", (opt.font || "sans-serif")];
        this.smallFont = (this.height * 0.8) + "px " + (opt.font || "sans-serif");
        this.align = opt.align || "center";
        this.backgroundColor = opt.backgroundColor || "#000";
        this.fontColor = opt.fontColor || "#fff";
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.textBaseline = "middle";
        this.range = opt.range || { max: 100, min: 0 };
        this.speed = opt.speed || 1;
        this.scanBar = opt.scanBar || { width: 4 };
        this.keep = opt.keepLastState || true;
        this.py = this.height * 0.5;
        this.px = 0;
        this.opx = this.px;
        this.opy = this.py;
        opt.range = opt.range || {};
        this.range = {
            max: opt.range.max || 100,
            min: opt.range.min || 0
        };
        this.range.med = opt.range.med || ((this.range.max - this.range.min) / 2);
        this._alarm = {
            max: (opt.alarm && opt.alarm.max) ? opt.alarm.max : 0,
            min: (opt.alarm && opt.alarm.min) ? opt.alarm.min : 0
        };
        this._units = opt.units || "";
        this.refreshRate = opt.refreshRate || 160;
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("background-color", this.backgroundColor)
                        .style("display", "block").attr("id", id).attr("class", id);
        
        opt.tracings = opt.tracings || {};
        opt.tracings.labels = opt.tracings.labels || {};
        opt.tracings.labels.width = opt.tracings.labels.width || this.width * 0.1;
        opt.tracings.labels.height = opt.tracings.labels.height || this.height / 6;
        opt.tracings.height = opt.tracings.height || this.height * 1.6;
        opt.tracings.width = opt.tracings.width || this.width * 0.85;
        opt.tracings.left = opt.tracings.left || opt.tracings.labels.width + this.width * 0.01;
        
        opt.value = opt.value || {};
        opt.value.height = opt.value.height || this.height * 0.7;
        opt.value.width = opt.value.width || this.width * 0.5;
        
        opt.label = opt.label || {};
        opt.label.width = opt.label.width || this.width * 0.3;

        opt.units = opt.label || {};
        opt.units.width = opt.label.width || this.width * 0.2;

        opt.align = "left";
        opt.parent = (this.div.node()) ? this.div.node().id : "body";
        this.disp_label = new SingleDisplay(id + "_label",
                                     { top: opt.value.height / 3,
                                       left: 0,
                                       height: opt.value.height * 0.4,
                                       width: opt.label.width }, opt);
        opt.align = "right";
        this.disp_value = new SingleDisplay(id + "_value",
                                     { top: 0,
                                       left: opt.label.width,
                                       height: opt.value.height,
                                       width: opt.value.width }, opt);
        this.disp_units = new SingleDisplay(id + "_units",
                                     { top: this.disp_label.top,
                                       left: this.disp_value.left + this.disp_value.width * 1.2,
                                       height: this.disp_label.height,
                                       width: opt.units.width },
                                     { parent: opt.parent, align: "left",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });        

        this.disp_fail = new SingleDisplay(id + "_fail",
                                     { top: opt.value.height * 0.2,
                                       left: opt.tracings.width + opt.tracings.labels.width,
                                       height: opt.value.height * 0.5,
                                       width: opt.value.width },
                                     { parent: opt.parent, font: "Times", fontColor: "red" });
        this.disp_alarm = new SingleDisplay(id + "_alarm",
                                     { top: 0,
                                       left: this.disp_value.left + this.disp_value.width,
                                       height: this.disp_value.height / 4,
                                       width: this.disp_value.width / 4 },
                                     { parent: opt.parent, align: "left", font: "Times", fontColor: "red" });
        this.tracings_label_max = new SingleDisplay(id + "_tracings_label_max",
                                     { top: this.disp_value.top + this.disp_value.height * 1.5, left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.tracings_label_med = new SingleDisplay(id + "_tracings_label_med",
                                     { top: this.tracings_label_max.top + (opt.tracings.height / 2),
                                       left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.tracings_label_min = new SingleDisplay(id + "_tracings_label_min",
                                     { top: this.tracings_label_max.top + opt.tracings.height - opt.tracings.labels.height / 2,
                                       left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.disp_tracings = new TracingsDisplay(id + "_tracings",
                                     { top: this.tracings_label_max.top,
                                       left: opt.tracings.left,
                                       height: opt.tracings.height, width: opt.tracings.width },
                                     { parent: opt.parent,
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor,
                                       range: opt.range });
        return this;
    }

    PumpMonitorDisplay.prototype.render = function (val, opt) {
        opt = opt || {};
        this.disp_value.render(val, opt);
        var units = opt.units || this._units;
        this.disp_units.render(units);
        this.disp_fail.hide();
        var label = opt.label || this.label;
        this.disp_label.render(label);
        opt.range = opt.range || {};
        opt.range.max = opt.range.max || this.range.max;
        opt.range.min = opt.range.min || this.range.min;
        opt.range.med = opt.range.med || (opt.range.max - opt.range.min) / 2;
        this.range = opt.range;
        if (val >= this.range.min && val <= this.range.max) {
            this.disp_tracings.render(val, { range: opt.range });
        } else {
            // display warning Out Of Range
        }
        this.tracings_label_max.render(opt.range.max);
        this.tracings_label_med.render(opt.range.med);
        this.tracings_label_min.render(opt.range.min);
        this.div.style("display", "block");
        return this;
    };
    
    PumpMonitorDisplay.prototype.hide = function () {
        this.disp_value.hide();
        this.disp_units.hide();
        this.disp_fail.hide();
        this.disp_label.hide();
        this.disp_alarm.hide();
        this.disp_tracings.hide();
        this.tracings_label_max.hide();
        this.tracings_label_min.hide();
        this.tracings_label_med.hide();
        this.div.style("display", "none");
        return this;
    };

    PumpMonitorDisplay.prototype.alarm = function (msg) {
        if (msg) {
            if (msg.indexOf("glyphicon") >= 0) {
                this.disp_alarm.renderGlyphicon(msg, { blinking: true });
            } else if(msg === "off") {
                this.disp_alarm.hide();
            } else {
                this.disp_alarm.render(msg);
            }
        }
    };
    
    PumpMonitorDisplay.prototype.set_units = function (data) {
        if (data) {
            this._units = data.units;
        }
    };
    
    PumpMonitorDisplay.prototype.set_range = function (data) {
        if (data) {
            this.range.max = data.max || this.range.max;
            this.range.min = data.min || this.range.min;
            this.range.med = (this.range.max + this.range.min) / 2;
        }
    };
    
    PumpMonitorDisplay.prototype.fail = function (msg) {
        this.disp_fail.render("FAIL", { fontColor: "red" });
        this.disp_value.hide();
        this.disp_alarm.renderGlyphicon("glyphicon-bell", { fontColor: "red", blinking: true });
        this.disp_tracings.pauseTrace();
        this.tracings_label_max.render(this.range.max);
        this.tracings_label_med.render(this.range.med);
        this.tracings_label_min.render(this.range.min);
        return this;        
    };

    PumpMonitorDisplay.prototype.move = function (data) {
        data = data || {};
        if (data.top) {
            this.top = data.top;
            this.div.style("top", this.top + "px");
        }
        if (data.left) {
            this.left = data.left;
            this.div.style("left", this.left + "px");
        }
        return this;
    };
    
    PumpMonitorDisplay.prototype.pause = function () {
        this.disp_tracings.pauseTrace();
        return this;
    };

    module.exports = PumpMonitorDisplay;
});
