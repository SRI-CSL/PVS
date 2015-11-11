/**
 * @module LED
 * @version 2.0
 * @description Renders LED light bulbs.
 *              The look and feel of LEDs can be customised, e.g., color, size, shape.
 * @author Paolo Masci
 * @date Apr 2, 2015
 *
 * @example <caption>Typical use of LED APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses LED.
 * define(function (require, exports, module) {
 *     "use strict";
 *
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");

    /**
     * @function <a name="LED">LED</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (x1,y1,x2,y2) of the display, specifying
     *        the left, top, right, bottom corner of the rectangle (for shape="rect")
     * @param opt {Object}
     * @memberof module:SingleDisplay
     * @instance
     */
    function LED(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 10;
        this.height = coords.height || 10;
        this.font = [this.height, "px ", (opt.font || "sans-serif")];
        this.smallFont = (this.height * 0.8) + "px " + (opt.font || "sans-serif");
        this.align = opt.align || "center";
        this.textBaseline = "middle";
        this.radius = opt.radius || 3;
        this.color = opt.color || "#00FF66"; // default is light green
        this.blinking = opt.blinking || false;
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("display", "block").attr("id", id).attr("class", id);
        this.div.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top");
        this.isOn = false;
        return this;
    }

    LED.prototype.render = function (opt) {
        opt = opt || {};
        var context = document.getElementById(this.id + "_canvas").getContext("2d");
        context.beginPath();
        context.globalAlpha = 0.9;
        context.arc(this.width / 2, this.height / 2, this.radius, 0, 2 * Math.PI, false);
        context.fillStyle = opt.color || this.color;
        context.fill();
        if (!opt.noborder) { context.stroke(); }
        var elemClass = this.div.node().getAttribute("class");
        if (opt.blinking || this.blinking) {
            elemClass += " blink";
        } else {
            elemClass = elemClass.replace(" blink", "");
        }
        this.div.node().setAttribute("class", elemClass);
        this.reveal();
        return this;
    };

    LED.prototype.toggle = function (opt) {
        if (this.isOn === true) {
            this.hide();
        } else {
            this.reveal();
        }
        return this;
    };
    
    LED.prototype.on = function (opt) {
        this.isOn = true;
        this.render(opt);
        return this;
    };

    LED.prototype.off = function () {
        this.isOn = false;
        this.hide();
        return this;
    };

    LED.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    LED.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    module.exports = LED;
});
