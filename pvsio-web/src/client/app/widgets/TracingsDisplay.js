/**
 * @module TracingsDisplay
 * @version 1.0
 * @description Renders historical data using tracings-like visualisation.
 *              This module provide APIs for changing the look and feel of the tracings,
 *              including: color, size, and tracings speed.
 * @author Paolo Masci
 * @date May 10, 2015
 *
 * @example <caption>Typical use of TracingsDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses TracingsDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.tracings = new TracingsDisplay("tracings",
 *                         { top: 56, left: 164, height: 30, width: 100 },
 *                         { parent: "prototype", align: "left", speed: 0.4 });
 *     device.tracings.render(10); // the display renders 10
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");

    /**
     * @function <a name="TracingsDisplay">TracingsDisplay</a>
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
     * @memberof module:TracingsDisplay
     * @instance
     */
    function TracingsDisplay(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
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
        this.keep = opt.keepLastState || false;
        this.py = this.height * 0.5;
        this.px = 0;
        this.opx = this.px;
        this.opy = this.py;
        this.refreshRate = opt.refreshRate || 250;
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("display", "block").attr("id", id).attr("class", id);
        this.div.append("span").attr("id", id + "_span").attr("class", id + "_span")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top");
        this.div.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top");
        this.loop = null;
        return this;
    }

    TracingsDisplay.prototype.render = function (val, opt) {    
        function clearContext() {
            context.save();
            context.fillStyle = _this.backgroundColor;
            context.fillRect(0, 0, _this.width, _this.height);
            context.restore();
        }        
        function render_aux() {
            _this.px += _this.speed;
            context.clearRect(_this.px, 0, _this.scanBar.width, _this.height);
            context.beginPath();
            context.moveTo(_this.opx, _this.py);
            context.lineTo(_this.px, _this.py);
            context.stroke();
            _this.opx = _this.px;
            _this.opy = _this.py;
            if (_this.opx > _this.width) {
                _this.px = _this.opx = -(_this.speed);
            }
        }
        var _this = this;
        opt = opt || {};
        opt.range = opt.range || { };
        opt.range.max = opt.range.max || _this.range.max;
        opt.range.min = opt.range.min || _this.range.min;
        this.py = (val > 0) ? this.height - (val / (opt.range.max - opt.range.min)) * this.height : this.height;
        var context = document.getElementById(this.id + "_canvas").getContext("2d");
        if (!this.loop) {
            var align = opt.align || this.align;
            context.textBaseline = this.textBaseline;
            context.strokeStyle = opt.fontColor || _this.fontColor;
            context.lineWidth = 3;
            context.font = this.font.join("");
            render_aux({ val: val, context: context, align: align, height: this.height, width: this.width }, opt);
            this.loop = setInterval(render_aux, _this.refreshRate);
            if (!this.keep) {
                this.px = this.opx = 0;
                clearContext();
            }
            this.reveal();
        }
        return this;
    };
    
    TracingsDisplay.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        var span = document.getElementById(this.id + "_span");
        span.setAttribute("class", "glyphicon " + icon);
        span.style.color = opt.fontColor || this.fontColor;
        this.reveal();
        return this;
    };    

    TracingsDisplay.prototype.hide = function () {
        this.div.style("display", "none");
        if (this.loop) {
            clearInterval(this.loop);
            this.loop = null;
        }
        return this;
    };

    TracingsDisplay.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };
    
    TracingsDisplay.prototype.pauseTrace = function () {
        if (this.loop) {
            clearInterval(this.loop);
            this.loop = null;
        }
        return this;
    };
    
    TracingsDisplay.prototype.move = function (data) {
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

    module.exports = TracingsDisplay;
});
