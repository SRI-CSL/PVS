/**
 * @module TouchScreenButton
 * @version 2.0
 * @description Renders a touchscreen button using bootstrap styles
 * @author Paolo Masci
 * @date May 24, 2015
 *
 * @example <caption>Typical use of TouchScreenButton APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses SingleDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var touchscreen = {};
 *     touchscreen.ok_btn = new TouchScreenButton("ok");
 *     touchscreen.render(); // the display renders 10
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Button = require("widgets/Button");

    /**
     * @function <a name="TouchScreenButton">TouchScreenButton</a>
     * @description Constructor.
     * @param id {String} The ID of the touchscreen button.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 32, height: 20 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     * @memberof module:SingleDisplay
     * @instance
     */
    function TouchScreenButton(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 32;
        this.height = coords.height || 10;
        this.font = [this.height, "px ", (opt.font || "sans-serif")];
        this.smallFont = (this.height * 0.8) + "px " + (opt.font || "sans-serif");
        this.align = opt.align || "center";
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        this.btnClass = opt.btnClass || "primary";
        var elemClass = id + " prevent_selection";
        if (this.blinking) { elemClass += " blink"; }
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("border-width", 0)
                        .style("display", "none").attr("id", id).attr("class", elemClass);
        this.div.append("button").attr("id", id + "_button").attr("class", id + "_button")
                        .attr("width", this.width).attr("height", this.height).style("border-width", 0);
        var touchID = id + "_touchscreen";
        this.touchscreen = new Button(touchID, {
            left: this.left, top: this.top, height: this.height, width: this.width
        }, {
            callback: opt.touchscreen.callback || function (err, res) {},
            evts: opt.touchscreen.events || ['click'],
            area: this.div
        });
        this.div.style("cursor", "pointer");
        this.txt = opt.txt || id;
        return this;
    }
    
    TouchScreenButton.prototype.render = function (txt, opt) {
        opt = opt || {};
        var button = document.getElementById(this.id + "_button");
        this.txt = txt;
        button.setAttribute("class", "btn btn-" + this.btnClass + " center");
        button.style.width = this.width;
        button.style.height = this.height;
        button.style.fontSize = 0.8 * this.height + "px";
        button.textContent = opt.txt || this.txt;
        d3.select("#" + this.id + "_button").style("display", "block");        
        this.reveal();
        return this;
    };

    TouchScreenButton.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        var button = document.getElementById(this.id + "_button");
        this.txt = icon;
        button.setAttribute("class", "glyphicon " + icon + " btn btn-" + this.btnClass + " center");
        button.style.width = this.width;
        button.style.height = this.height;
        button.style.fontSize = 0.8 * this.height + "px";
        button.textContent = opt.txt || this.txt;
        d3.select("#" + this.id + "_button").style("display", "block");        
        this.reveal();
        return this;
    };


    TouchScreenButton.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    TouchScreenButton.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    TouchScreenButton.prototype.move = function (data) {
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

    module.exports = TouchScreenButton;
});
