/**
 * @module TripleDisplay
 * @version 2.0
 * @description This module renders three displays horizontally aligned next to each other.
 *              It is intended to be used for rendering triples such as (label, val, units), e.g., rate 1.2mL/h.
 * @author Paolo Masci
 * @date Apr 2, 2015
 *
 * @example <caption>Typical use of TripleDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses TripleDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var SingleDisplay = require("widgets/SingleDisplay");
    var Button = require("widgets/Button");

    /**
     * @function <a name="TripleDisplay">TripleDisplay</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (x1,y1,x2,y2) of the display, specifying
     *        the left, top, right, bottom corner of the rectangle (for shape="rect")
     * @param opt {Object}
     * @memberof module:DoubleDisplay
     * @instance
     */
    function TripleDisplay(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.ratio = opt.ratio || 0.32;
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;
        this.backgroundColor = opt.backgroundColor || ""; //transparent
        this.fontColor = opt.fontColor || "#fff"; //white
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.blinking = opt.blinking || false;
        var elemClass = id + " prevent_selection ";
        if (opt.blinking) { elemClass += " blink "; }
        if (opt.touchscreen && opt.touchscreen.classStyle) { elemClass += opt.touchscreen.classStyle; }
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("border-radius", "2px")
                        .style("background-color", this.backgroundColor)
                        .style("display", "none").attr("id", id).attr("class", elemClass);
        var coords_left = {
            top: (opt.left_display && opt.left_display.top) ? opt.left_display.top : 0,
            left: (opt.left_display && opt.left_display.left) ? opt.left_display.left : 0,
            width: (opt.left_display && opt.left_display.width) ? opt.left_display.width : this.width * this.ratio,
            height: (opt.left_display && opt.left_display.height) ? opt.left_display.height : this.height
        };
        var coords_center = {
            top: (opt.center_display && opt.center_display.top)
                    ? opt.center_display.top : 0,
            left: (opt.center_display && opt.center_display.left)
                    ? opt.center_display.left : (coords_left.left + coords_left.width),
            width: (opt.center_display && opt.center_display.width)
                    ? opt.center_display.width
                    : (this.width - coords_left.width) * 0.5,
            height: (opt.center_display && opt.center_display.height)
                    ? opt.center_display.height : coords.height
        };
        var coords_right = {
            top: (opt.right_display && opt.right_display.top)
                    ? opt.right_display.top : 0,
            left: (opt.right_display && opt.right_display.left)
                    ? (coords_center.left + coords_center.width) + opt.right_display.left 
                    : (coords_center.left + coords_center.width),
            width: (opt.right_display && opt.right_display.width)
                    ? opt.right_display.width : (this.width - coords_center.width - coords_left.width),
            height: (opt.right_display && opt.right_display.height) ? opt.right_display.height : coords.height
        };
        var opt_left = {
            parent: id,
            font: opt.font,
            align: (opt.left_display && opt.left_display.align) ? opt.left_display.align : opt.align,
            inverted: opt.inverted
        };
        var opt_center = {
            parent: id,
            font: opt.font,
            align: (opt.center_display && opt.center_display.align) ? opt.center_display.align : opt.align,
            inverted: opt.inverted
        };
        var opt_right = {
            parent: id,
            font: opt.font,
            align: (opt.right_display && opt.right_display.align) ? opt.right_display.align : opt.align,
            inverted: opt.inverted
        };
        this.leftDisplay = new SingleDisplay(id + "_left", coords_left, opt_left);
        this.centerDisplay = new SingleDisplay(id + "_center", coords_center, opt_center);
        this.rightDisplay = new SingleDisplay(id + "_right", coords_right, opt_right);
        if (opt.touchscreen) {
            var touchID = id + "_touchscreen";
            this.button = new Button(touchID, {
                left: this.left, top: this.top, height: this.height, width: this.width
            }, {
                callback: opt.touchscreen.callback || function (err, res) {},
                evts: opt.touchscreen.events || ['click'],
                area: this.div
            });
            this.div.style("cursor", "pointer");
            var _this = this;
            this.div.on("mouseover", function() {
                _this.div.style("background-color", "steelblue").style("color", "white");
            }).on("mouseout", function() {
                _this.div.style("background-color", _this.backgroundColor).style("color", _this.fontColor);
            });            
        }        
        return this;
    }

    TripleDisplay.prototype.getLeftDisplay = function () {
        return this.leftDisplay;
    };

    TripleDisplay.prototype.getRightDisplay = function () {
        return this.rightDisplay;
    };

    TripleDisplay.prototype.getCenterDisplay = function () {
        return this.centerDisplay;
    };

    TripleDisplay.prototype.renderLabel = function (txt) {
        this.leftDisplay.render(txt);
        return this.reveal();
    };

    TripleDisplay.prototype.renderValue = function (val) {
        this.centerDisplay.render(val);
        return this.reveal();
    };

    TripleDisplay.prototype.renderUnits = function (units) {
        this.rightDisplay.render(units);
        return this.reveal();
    };

    TripleDisplay.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    TripleDisplay.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    TripleDisplay.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        this.div.style("background-color", this.backgroundColor);
        this.leftDisplay.invertColors();
        this.centerDisplay.invertColors();
        this.rightDisplay.invertColors();
        return this;
    };

    TripleDisplay.prototype.move = function (data) {
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


    module.exports = TripleDisplay;
});
