/**
 * @module PatientMonitorDisplay
 * @version 1.0
 * @description Renders a basic patient monitor display, including: current value, label, alarms, max/min values, tracings.
 *              This module provide APIs for changing the look and feel of the display element, such as position, 
 *              font color, size, and tracings speed.
 * @author Paolo Masci
 * @date May 10, 2015
 *
 * @example <caption>Typical use of PatientMonitorDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses PatientMonitorDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.spo2 = new PatientMonitorDisplay("spo2",
 *                         { top: 56, left: 164, height: 30, width: 100 });
 *     device.spo2.render(10); // the display renders 10
 *     device.spo2.alarm("ALARM"); // the display shows an alarm
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    var SingleDisplay = require("widgets/SingleDisplay");
    var TracingsDisplay = require("widgets/TracingsDisplay");
    var d3 = require("d3/d3");

    /**
     * @function <a name="PatientMonitorDisplay">PatientMonitorDisplay</a>
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
     * @memberof module:PatientMonitorDisplay
     * @instance
     */
    function PatientMonitorDisplay(id, coords, opt) {
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
        opt.tracings.labels.width = opt.tracings.labels.width || this.width * 0.07;
        opt.tracings.labels.height = opt.tracings.labels.height || this.height / 6;
        opt.tracings.height = opt.tracings.height || this.height * 0.8;
        opt.tracings.width = opt.tracings.width || this.width * 0.62;
        opt.tracings.left = opt.tracings.left || opt.tracings.labels.width + this.width * 0.01;
        
        opt.value = opt.value || {};
        opt.value.height = opt.value.height || this.height * 0.6;
        opt.value.width = opt.value.width || this.width * 0.3;
        
        opt.range = opt.range || {};
        opt.range.max = opt.range.max || 100;
        opt.range.min = opt.range.min || 0;

        opt.parent = (this.div.node()) ? this.div.node().id : "body";
        this.disp_value = new SingleDisplay(id + "_value",
                                     { top: 0,
                                       left: opt.tracings.width + opt.tracings.labels.width,
                                       height: opt.value.height,
                                       width: opt.value.width }, opt);
        this.disp_fail = new SingleDisplay(id + "_fail",
                                     { top: opt.value.height * 0.2,
                                       left: opt.tracings.width + opt.tracings.labels.width,
                                       height: opt.value.height * 0.5,
                                       width: opt.value.width },
                                     { parent: opt.parent, font: "Times", fontColor: "red" });
        this.disp_label = new SingleDisplay(id + "_label",
                                     { top: opt.value.height,
                                       left: opt.tracings.width + opt.tracings.labels.width,
                                       height: opt.value.height / 4,
                                       width: opt.value.width }, opt);
        this.disp_alarm = new SingleDisplay(id + "_alarm",
                                     { top: 0,
                                       left: this.disp_value.left + this.disp_value.width,
                                       height: this.disp_value.height / 4,
                                       width: this.disp_value.width / 4 },
                                     { parent: opt.parent, align: "left", font: "Times", fontColor: "red" });
        this.disp_alarm_min = new SingleDisplay(id + "_alarm_min",
                                     { top: this.disp_value.height - this.disp_value.height / 4,
                                       left: this.disp_alarm.left,
                                       height: this.disp_alarm.height,
                                       width: this.disp_alarm.width },
                                     { parent: opt.parent, align: "left", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.disp_alarm_max = new SingleDisplay(id + "_alarm_max",
                                     { top: this.disp_alarm_min.top - this.disp_value.height / 4,
                                       left: this.disp_alarm.left,
                                       height: this.disp_alarm.height,
                                       width: this.disp_alarm.width },
                                     { parent: opt.parent, align: "left", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.tracings_label_max = new SingleDisplay(id + "_tracings_label_max",
                                     { top: 0, left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.tracings_label_med = new SingleDisplay(id + "_tracings_label_med",
                                     { top: (opt.tracings.height / 2),
                                       left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.tracings_label_min = new SingleDisplay(id + "_tracings_label_min",
                                     { top: opt.tracings.height - opt.tracings.labels.height / 4,
                                       left: 0,
                                       height: opt.tracings.labels.height,
                                       width: opt.tracings.labels.width },
                                     { parent: opt.parent, align: "right", font: "Times",
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor });
        this.disp_tracings = new TracingsDisplay(id + "_tracings",
                                     { top: opt.tracings.labels.height / 2, left: opt.tracings.left,
                                       height: opt.tracings.height, width: opt.tracings.width },
                                     { parent: opt.parent,
                                       fontColor: opt.fontColor, backgroundColor: opt.backgroundColor,
                                       range: opt.range });
        return this;
    }

    PatientMonitorDisplay.prototype.render = function (val, opt) {
        opt = opt || {};
        this.disp_value.render(val, opt);
        this.disp_fail.hide();
        var label = opt.label || this.label;
        this.disp_label.render(label);
        if (this._alarm.max === 0) {
            this.disp_alarm_max.render("--");
        } else {
            this.disp_alarm_max.render(this._alarm.max);
        }
        this.disp_alarm_min.render(this._alarm.min);
        if (val >= this.range.min && val <= this.range.max) {
            this.disp_tracings.render(val);
        } else {
            // display warning Out Of Range
        }
        this.tracings_label_max.render(this.range.max);
        this.tracings_label_med.render(this.range.med);
        this.tracings_label_min.render(this.range.min);
        this.div.style("display", "block");
        return this;
    };
    
    PatientMonitorDisplay.prototype.hide = function () {
        this.disp_value.hide();
        this.disp_fail.hide();
        this.disp_label.hide();
        this.disp_alarm.hide();
        this.disp_alarm_max.hide();
        this.disp_alarm_min.hide();
        this.disp_tracings.hide();
        this.tracings_label_max.hide();
        this.tracings_label_min.hide();
        this.tracings_label_med.hide();
        this.div.style("display", "none");
        return this;
    };

    PatientMonitorDisplay.prototype.alarm = function (msg) {
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
    
    PatientMonitorDisplay.prototype.set_alarm = function (data) {
        if (data) {
            this._alarm.max = data.max || this._alarm.max;
            this._alarm.min = data.min || this._alarm.min;
        }
    };
    
    PatientMonitorDisplay.prototype.set_range = function (data) {
        if (data) {
            this.range.max = data.max || this.range.max;
            this.range.min = data.min || this.range.min;
            this.range.med = (this.range.max + this.range.min) / 2;
        }
    };
    
    PatientMonitorDisplay.prototype.fail = function (msg) {
        this.disp_fail.render("FAIL", { fontColor: "red" });
        this.disp_value.hide();
        this.disp_alarm.renderGlyphicon("glyphicon-bell", { fontColor: "red", blinking: true });
        this.disp_tracings.pauseTrace();
        this.tracings_label_max.render(this.range.max);
        this.tracings_label_med.render(this.range.med);
        this.tracings_label_min.render(this.range.min);
        return this;        
    };

    PatientMonitorDisplay.prototype.move = function (data) {
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

    module.exports = PatientMonitorDisplay;
});
