/**
 * @module BasicDisplay
 * @version 2.0
 * @description Renders a basic digital display.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji
 * @date Apr 1, 2015
 *
 * @example <caption>Typical use of BasicDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses BasicDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.disp = new BasicDisplay("disp", { top: 222, left: 96, height: 8, width: 38 });
 *     device.disp.render(10); // the display renders 10
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, document */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Widget = require("widgets/Widget"),
        StateParser = require("util/PVSioStateParser"),
        Speaker  = require("widgets/TextSpeaker"),
        property = require("util/property");

    /**
     * @function <a name="BasicDisplay">BasicDisplay</a>
     * @description Constructor.
     * @param id {String} The ID of the display.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 200, height: 80 }.
     * @param opt {Object} Options:
     *          <li>backgroundColor (String): background display color (default is black, "#000")</li>
     *          <li>fontfamily (String): display font type (default is "sans-serif")</li>
     *          <li>fontColor (String): display font color (default is white, "#fff")</li>
     *          <li>align (String): text alignment (default is "center")</li>
     *          <li>inverted (Bool): if true, the text has inverted colors,
     *              i.e., fontColor becomes backgroundColor, and backgroundColor becomes fontColor (default is false)</li>
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     * @memberof module:BasicDisplay
     * @instance
     */
    function BasicDisplay(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
        opt.displayKey = opt.displayKey || id;
        opt.auditoryFeedback = (opt.auditoryFeedback) ?
            ((opt.auditoryFeedback.toString() === "enabled" || opt.auditoryFeedback.toString() === "true") ? "enabled" : "disabled") : "disabled";
        opt.visibleWhen = opt.visibleWhen || "true";
        this.id = property.call(this, id);
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;
        this.fontsize = opt.fontsize || (this.height * 0.9);
        this.fontfamily = opt.fontfamily || "sans-serif";
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        this.align = opt.align || "center";
        this.backgroundColor = opt.backgroundColor || "black";
        this.fontColor = opt.fontColor || "white";
        this.cursor = opt.cursor || "default";
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        var elemClass = id + " displayWidget" + " noselect ";
        if (this.blinking) { elemClass += " blink"; }
        opt.position = opt.position || "absolute";
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
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
        var x2 = this.left + this.width;
        var x3 = this.top + this.height;
        this.div.attr("coords", this.left + "," + this.top + "," + x2 + "," + x3)
                .style("cursor", this.cursor);
        this.txt = ""; // txt is used to store the current text rendered on the display
        this.displayKey = property.call(this, opt.displayKey);
        this.auditoryFeedback = property.call(this, opt.auditoryFeedback);
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.example = opt.example || "test"; // example is used in the prototype builder to demonstrate the font style of the display
        Widget.call(this, id, "display");
        return this;
    }
    BasicDisplay.prototype = Object.create(Widget.prototype);
    BasicDisplay.prototype.constructor = BasicDisplay;
    BasicDisplay.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:BasicDisplay
    */
    BasicDisplay.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            displayKey: this.displayKey(),
            auditoryFeedback: this.auditoryFeedback(),
            visibleWhen: this.visibleWhen(),
            fontsize: this.fontsize,
            fontColor: this.fontColor,
            backgroundColor: this.backgroundColor
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    BasicDisplay.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            BasicDisplay.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        }
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        // this.fontsize = this.height * 0.9;
        // this.font = [this.fontsize, "px ", this.fontfamily];
        // this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px").style("font-size", this.fontsize + "px");
        d3.select("div." + this.id()).select("span").attr("width", this.width + "px").attr("height", this.height + "px"); // used for glyphicon
        d3.select("div." + this.id()).select("canvas").attr("width", this.width + "px").attr("height", this.height + "px"); // used for standard text and numbers
        return this.render(this.example, opt);
    };
    BasicDisplay.prototype.updateStyle = function (data) {
        data = data || {};
        this.fontsize = data.fontsize || this.fontsize;
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        this.fontColor = data.fontColor || this.fontColor;
        this.backgroundColor = data.backgroundColor || this.backgroundColor;
        return this;
    };
    /**
     * Removes the widget's div
     */
    BasicDisplay.prototype.remove = function () {
        BasicDisplay.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };
    BasicDisplay.prototype.setColors = function (colors, opt) {
        colors = colors || {};
        opt = opt || {};
        opt.auditoryFeedback = opt.auditoryFeedback || "disabled";
        this.fontColor = colors.fontColor || this.fontColor;
        this.backgroundColor = colors.backgroundColor || this.backgroundColor;
        return this.render(this.txt, opt);
    };
    BasicDisplay.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.render(this.txt, { blinking: elemIsBlinking });
    };
    BasicDisplay.prototype.invertGlyphiconColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.renderGlyphicon(this.txt, { blinking: elemIsBlinking });
    };
    BasicDisplay.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.example;
        return this.render(txt, { visibleWhen: "true" });
    };
    BasicDisplay.prototype.render = function (txt, opt) {
        function renderln(data, opt) {
            opt = opt || {};
            data.context.clearRect(0, 0, data.width, data.height);
            data.context.fillStyle = opt.backgroundColor || _this.backgroundColor;
            if (data.context.fillStyle !== "transparent") {
                data.context.fillRect(0, 0, data.width, data.height);
            }
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
        opt.auditoryFeedback = opt.auditoryFeedback || this.auditoryFeedback();
        var isEnabled = false;
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                isEnabled = true;
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(txt, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str);
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             isEnabled = true;
                    }
                }
            }
        }

        if (isEnabled) {
            txt = txt || "";
            var _this = this;
            if (typeof txt === "object") {
                // txt in this case is a PVS state that needs to be parsed
                var disp = StateParser.resolve(txt, this.displayKey());
                if (disp) {
                    this.txt = StateParser.evaluate(disp);
                    if (typeof this.txt === "string") {
                        this.txt = this.txt.replace(new RegExp("\"", "g"), "");
                    }
                }
            } else {
                this.txt = txt;
            }
            //read out the display value if audio is enabled for this display widget
            if (opt.auditoryFeedback === "enabled") {
                Speaker.speak(this.txt.toString());
            }
            this.example = this.txt;
            // set blinking
            var elemClass = document.getElementById(this.id()).getAttribute("class");
            elemClass = (opt.blinking || this.blinking) ?
                            ((elemClass.indexOf("blink") < 0) ? (elemClass + " blink") : elemClass)
                            : elemClass.replace(" blink", "");
            document.getElementById(this.id()).setAttribute("class", elemClass);
            // render content
            var context = document.getElementById(this.id() + "_canvas").getContext("2d");
            context.textBaseline = this.textBaseline;
            var align = opt.align || this.align;
            context.font = this.font.join("");
            renderln({
                txt: this.txt,
                context: context,
                align: align,
                height: this.height,
                width: this.width
            }, opt);
            d3.select("#" + this.id() + "_canvas").style("display", "block");
            d3.select("#" + this.id() + "_span").style("display", "none");
            return this.reveal();
        }
        return this.hide();
    };

    BasicDisplay.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        this.txt = icon;
        var span = document.getElementById(this.id() + "_span");
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
        span.style.fontSize = 0.7 * this.height + "px";
        d3.select("#" + this.id() + "_canvas").style("display", "none");
        d3.select("#" + this.id() + "_span").style("display", "block");
        this.reveal();
        return this;
    };

    BasicDisplay.prototype.renderMultiline = function (txt, opt) {
        function clearContext(context, width, height) {
            context.save();
            context.fillStyle = opt.backgroundColor;
            context.fillRect(0, 0, width, height);
            context.restore();
        }
        function renderln(data, opt) {
            opt = opt || {};
            data.context.fillStyle = (opt.inverted) ? opt.fontColor : opt.backgroundColor;
            data.context.fillRect(0, data.y, data.width, data.height);
            data.context.fillStyle = (opt.inverted) ? opt.backgroundColor : opt.fontColor;
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
        opt.backgroundColor = opt.backgroundColor || this.backgroundColor;
        opt.fontColor = opt.fontColor || this.fontColor;
        this.txt = txt;
        var context = document.getElementById(this.id() + "_canvas").getContext("2d");
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
        var elemClass = document.getElementById(this.id()).getAttribute("class");
        if (opt.blinking || this.blinking) {
            if (elemClass.indexOf("blink") < 0) {
                elemClass = elemClass + " blink";
            }
        } else {
            elemClass = elemClass.replace(" blink", "");
        }
        document.getElementById(this.id()).setAttribute("class", elemClass);
        d3.select("#" + this.id() + "_canvas").style("display", "block");
        d3.select("#" + this.id() + "_span").style("display", "none");
        this.reveal();
        return this;
    };

    BasicDisplay.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    BasicDisplay.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    BasicDisplay.prototype.move = function (data) {
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

    module.exports = BasicDisplay;
});
