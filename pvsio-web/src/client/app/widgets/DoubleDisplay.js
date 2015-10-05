/**
 * @module DoubleDisplay
 * @version 2.0
 * @description This module renders two displays horizontally aligned next to each other.
 *              It is intended to be used for rendering pairs such as (label, val), or (val, units).
 * @author Paolo Masci
 * @date Apr 2, 2015
 *
 * @example <caption>Typical use of DoubleDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses DoubleDisplay.
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

//    var d3 = require("d3/d3");
//    var black, white;
    var SingleDisplay = require("widgets/SingleDisplay");

    /**
     * @function <a name="DoubleDisplay">DoubleDisplay</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (x1,y1,x2,y2) of the display, specifying
     *        the left, top, right, bottom corner of the rectangle (for shape="rect")
     * @param opt {Object}
     * @memberof module:DoubleDisplay
     * @instance
     */
    function DoubleDisplay(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.ratio = opt.ratio || 0.5;
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;
        var coords_left = {
            top: (opt.left_display && opt.left_display.top) ? opt.left_display.top : this.top,
            left: (opt.left_display && opt.left_display.left) ? opt.left_display.left : this.left,
            width: (opt.left_display && opt.left_display.width) ? opt.left_display.width : this.width * this.ratio,
            height: (opt.left_display && opt.left_display.height) ? opt.left_display.height : this.height
        };
        var coords_right = {
            top: (opt.right_display && opt.right_display.top) ? opt.right_display.top : this.top,
            left: (opt.right_display && opt.right_display.left) ? opt.right_display.left : (coords_left.left + coords_left.width),
            width: (opt.right_display && opt.right_display.width) ? opt.right_display.width : (this.width - coords_left.width),
            height: (opt.right_display && opt.right_displat.height) ? opt.right_display.height : coords.height
        };
        var opt_left = {
            parent: opt.parent,
            font: opt.font,
            align: (opt.left_display && opt.left_display.align) ? opt.left_display.align : opt.align,
            inverted: opt.inverted
        };
        var opt_right = {
            parent: opt.parent,
            font: opt.font,
            align: (opt.right_display && opt.right_display.align) ? opt.right_display.align : opt.align,
            inverted: opt.inverted
        };
        this.leftDisplay = new SingleDisplay(id + "_left", coords_left, opt_left);
        this.rightDisplay = new SingleDisplay(id + "_right", coords_right, opt_right);
        this.reveal();
        return this;
    }

    DoubleDisplay.prototype.getLeftDisplay = function () {
        return this.leftDisplay;
    };

    DoubleDisplay.prototype.getRightDisplay = function () {
        return this.rightDisplay;
    };

    DoubleDisplay.prototype.hide = function () {
        this.leftDisplay.hide();
        this.rightDisplay.hide();
        return this;
    };

    DoubleDisplay.prototype.reveal = function () {
        this.leftDisplay.reveal();
        this.rightDisplay.reveal();
        return this;
    };

//    DoubleDisplay.prototype.move = function (data) {
//        this.leftDisplay.move(data);
//        this.rightDisplay.move(data);
//        return this;
//    };

    module.exports = DoubleDisplay;
});
