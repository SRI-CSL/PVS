/**
 * @module SingleDisplay
 * @version 2.0
 * @description Renders a basic digital display.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji
 * @date Apr 1, 2015
 *
 * @example <caption>Typical use of SingleDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses SingleDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.disp = new SingleDisplay("disp", { top: 222, left: 96, height: 8, width: 38 });
 *     device.disp.render(10); // the display renders 10
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
     * @function <a name="SingleDisplay">SingleDisplay</a>
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
     * @memberof module:SingleDisplay
     * @instance
     */
    function SingleDisplay(id, coords, opt) {
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
        this.backgroundColor = opt.backgroundColor || ""; //transparent
        this.fontColor = opt.fontColor || "#fff"; //white
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        var elemClass = id + " prevent_selection ";
        if (opt.touchscreen && opt.touchscreen.classStyle) { elemClass += opt.touchscreen.classStyle; }
        if (this.blinking) { elemClass += " blink"; }
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("border-radius", "2px")
                        .style("background-color", this.backgroundColor)
                        .style("display", "none").attr("id", id).attr("class", elemClass);
        this.div.append("span").attr("id", id + "_span").attr("class", id + "_span")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top").style("border-radius", "2px");
        this.div.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0).style("border-radius", "2px")
                        .style("vertical-align", "top");
        if (opt.touchscreen) {
            var touchID = id + "_touchscreen";
            this.touchscreen = new Button(touchID, {
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
        this.txt = "";
        return this;
    }
    
    SingleDisplay.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id).getAttribute("class").indexOf("blink") >= 0);
        return this.render(this.txt, { blinking: elemIsBlinking });
    };

    SingleDisplay.prototype.invertGlyphiconColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id).getAttribute("class").indexOf("blink") >= 0);
        return this.renderGlyphicon(this.txt, { blinking: elemIsBlinking });
    };
    
    SingleDisplay.prototype.render = function (txt, opt) {
        function renderln(data, opt) {
            opt = opt || {};
            data.context.clearRect(0, 0, data.width, data.height);
            data.context.fillStyle = opt.fontColor || _this.fontColor;
            if (data.align === "left") {
                data.context.textAlign = "start";
                data.context.fillText(data.txt, 0, data.height / 2);
            } else if (data.align === "right") {
                data.context.textAlign = "end";
                data.context.fillText(data.txt, data.width, data.height / 2);
            } else {
                data.context.textAlign = "center";
                data.context.fillText(data.txt, data.width / 2, data.height / 2);
            }
        }
        opt = opt || {};
        var _this = this;
        this.txt = txt;
        // set blinking
        var elemClass = document.getElementById(this.id).getAttribute("class");
        if (opt.blinking || this.blinking) {
            if (elemClass.indexOf("blink") < 0) {
                elemClass = elemClass + " blink";
            }
        } else {
            elemClass = elemClass.replace(" blink", "");
        }
        document.getElementById(this.id).setAttribute("class", elemClass);
        // render content
        var context = document.getElementById(this.id + "_canvas").getContext("2d");
        context.textBaseline = this.textBaseline;
        var align = opt.align || this.align;
        context.font = this.font.join("");
        renderln({ txt: txt, context: context, align: align, height: this.height, width: this.width }, opt);
        d3.select("#" + this.id + "_canvas").style("display", "block");
        d3.select("#" + this.id + "_span").style("display", "none");
        if (this.touchscreen) {
            if (opt.disableTouch) {
                this.div.style("cursor", "default");
            } else {
                this.div.style("cursor", "pointer");
            }
        }
        this.reveal();
        return this;
    };
    
    SingleDisplay.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        this.txt = icon;
        var span = document.getElementById(this.id + "_span");
        if (opt.blinking || this.blinking) {
            span.setAttribute("class", "glyphicon " + icon + " blink");
        } else {
            span.setAttribute("class", "glyphicon " + icon);
        }
        span.style.color = opt.fontColor || this.fontColor;
        span.style.backgroundColor = opt.backgroundColor || this.backgroundColor;
        span.style.borderRadius = "2px";
        span.style.width = this.width;
        span.style.height = this.height;
        span.style.fontSize = 0.8 * this.height + "px";
        d3.select("#" + this.id + "_canvas").style("display", "none");
        d3.select("#" + this.id + "_span").style("display", "block");        
        this.reveal();
        return this;
    };
    
    SingleDisplay.prototype.renderMultiline = function (txt, opt) {
        function clearContext(context, width, height) {
            context.save();
            context.fillStyle = backgroundColor;
            context.fillRect(0, 0, width, height);
            context.restore();
        }
        function renderln(data, opt) {
            opt = opt || {};
            data.context.fillStyle = (opt.inverted) ? _this.fontColor : backgroundColor;
            data.context.fillRect(0, data.y, data.width, data.height);
            data.context.fillStyle = (opt.inverted) ? backgroundColor : _this.fontColor;
            var y_offset = data.y || 0;
            if (data.align === "left") {
                data.context.textAlign = "start";
                data.context.fillText(data.txt, 0, data.height / 2 + y_offset);
            } else if (data.align === "right") {
                data.context.textAlign = "end";
                data.context.fillText(data.txt, data.width, data.height / 2 + y_offset);
            } else {
                data.context.textAlign = "center";
                data.context.fillText(data.txt, data.width / 2, data.height / 2 + y_offset);
            }
        }
        opt = opt || {};
        var _this = this;
        this.txt = txt;
        var backgroundColor = (_this.backgroundColor !== "") ? _this.backgroundColor
                                : (_this.fontColor !== "#000") ? "#000" : "#fff"; //default is black
        var context = document.getElementById(this.id + "_canvas").getContext("2d");
        clearContext(context, this.width, this.height);
        context.textBaseline = this.textBaseline;
        var align = opt.align || this.align;
        if (typeof txt === "object" && txt.length) {
            var fontsize = opt.menuFontSize || (this.height / txt.length);
            var newFont = [ fontsize ].concat(this.font.slice(1));
            context.font = newFont.join("");
            var i = 0;
            for (i = 0; i < txt.length; i++) {
                var offset = (opt.direction === "inverted") ? txt.length - i - 1 : i;
                renderln({
                    txt:  txt[i],
                    context: context,
                    align: align,
                    width: this.width,
                    height: (this.height / txt.length),
                    x: 0,
                    y: (offset * (this.height / txt.length))
                }, { inverted: (+opt.selected === i) ? true : false });
            }
        }
        // set blinking
        var elemClass = document.getElementById(this.id).getAttribute("class");        
        if (opt.blinking || this.blinking) {
            if (elemClass.indexOf("blink") < 0) {
                elemClass = elemClass + " blink";
            }
        } else {
            elemClass = elemClass.replace(" blink", "");
        }
        document.getElementById(this.id).setAttribute("class", elemClass);        
        d3.select("#" + this.id + "_canvas").style("display", "block");
        d3.select("#" + this.id + "_span").style("display", "none");        
        this.reveal();
        return this;
    };

    SingleDisplay.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    SingleDisplay.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    SingleDisplay.prototype.move = function (data) {
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

    module.exports = SingleDisplay;
});
