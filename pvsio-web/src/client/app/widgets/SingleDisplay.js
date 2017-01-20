/**
 * @module SingleDisplay *** DEPRECATED: Replaced by BasicDisplay & NumericDisplay *****
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
    var Widget = require("widgets/Widget"),
        StateParser = require("util/PVSioStateParser"),
        Speaker  = require("widgets/TextSpeaker"),
        property = require("util/property");

    /**
     * @function <a name="SingleDisplay">SingleDisplay</a>
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
     * @memberof module:SingleDisplay
     * @instance
     */
    function SingleDisplay(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
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
        this.backgroundColor = opt.backgroundColor || ""; //transparent
        this.fontColor = opt.fontColor || "#fff"; //white
        this.cursor = opt.cursor || "default";
        if (opt.inverted) {
            var tmp = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp;
        }
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        var elemClass = id + " displayWidget" + " noselect ";
        if (opt.touchscreen && opt.touchscreen.classStyle) { elemClass += opt.touchscreen.classStyle; }
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
        this.txt = "";
        opt.displayKey = opt.displayKey || id;
        opt.cursorName = opt.cursorName || "";
        opt.auditoryFeedback = (opt.auditoryFeedback) ? "enabled" : "disabled";
        opt.touchscreenvisibleWhen = (opt.touchscreenvisibleWhen && opt.touchscreenvisibleWhen !== "") ? opt.touchscreenvisibleWhen : "false";
        opt.touchscreenCommand = opt.touchscreenCommand || "";
        this.displayKey = property.call(this, opt.displayKey);
        this.cursorName = property.call(this, opt.cursorName);
        this.auditoryFeedback = property.call(this, opt.auditoryFeedback);
        this.touchscreenEnabled = property.call(this, false); // the first call to function render() will set the value of this property according to this.touchscreenvisibleWhen()
        this.touchscreenvisibleWhen = property.call(this, opt.touchscreenvisibleWhen);
        this.touchscreenCommand = property.call(this, opt.touchscreenCommand);
        this.example = opt.example || ""; // this is used in the prototype builder to demonstrate the font style of the display
        if (opt.touchscreen) {
            var touchID = id;
            this.touchscreenBackgroundColor = opt.touchscreen.backgroundColor || "green";
            this.touchscreenFontColor = opt.touchscreen.fontColor || "white";
            this.touchscreenCursor = opt.touchscreen.cursor || "pointer";
            this.touchscreen = new Button(touchID, {
                left: this.left, top: this.top, height: this.height, width: this.width
            }, {
                callback: opt.touchscreen.callback || function (err, res) {},
                evts: ['click'],
                area: this.div,
                functionText: opt.touchscreenCommand
            });
            var _this = this;
            this.div.on("mouseover", function() {
                if (_this.touchscreenEnabled()) {
                    _this.div.style("cursor", _this.touchscreenCursor);
                    _this.div.style("background-color", _this.touchscreenBackgroundColor).style("color", _this.touchscreenFontColor);
                } else {
                    _this.div.style("cursor", _this.cursor);
                }
            }).on("mouseout", function() {
                _this.div.style("cursor", _this.cursor);
                _this.div.style("background-color", _this.backgroundColor).style("color", _this.fontColor);
            });
        }
        Widget.call(this, id, "display");
        return this;
    }
    SingleDisplay.prototype = Object.create(Widget.prototype);
    SingleDisplay.prototype.constructor = SingleDisplay;
    SingleDisplay.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:SingleDisplay
    */
    SingleDisplay.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            displayKey: this.displayKey(),
            cursorName: this.cursorName(),
            auditoryFeedback: this.auditoryFeedback(),
            touchscreenvisibleWhen: this.touchscreenvisibleWhen(),
            touchscreenCommand: this.touchscreenCommand()
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    SingleDisplay.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            SingleDisplay.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        }
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        this.fontsize = this.height * 0.9;
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px").style("font-size", this.fontsize + "px");
        d3.select("div." + this.id()).select("span").attr("width", this.width + "px").attr("height", this.height + "px");
        d3.select("div." + this.id()).select("canvas").attr("width", this.width + "px").attr("height", this.height + "px");
        return this.render(this.example);
    };
    /**
     * Removes the widget's div
     */
    SingleDisplay.prototype.remove = function () {
        SingleDisplay.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };


    /**
     * @function boundFunctions
     * @returns {String} A comma separated string representing the PVS functions modelling actions over this button.
     * @memberof module:SingleDisplay
     */
    SingleDisplay.prototype.boundFunctions = function () {
        if (this.touchscreen) {
            return this.touchscreen.boundFunctions();
        }
        return [];
    };

    SingleDisplay.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.render(this.txt, { blinking: elemIsBlinking });
    };

    SingleDisplay.prototype.invertGlyphiconColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
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
        function renderNumber(data, opt) {
            function drawCircle(context, x, y, r, fillStyle) {
                context.save();
                context.fillStyle = fillStyle;
                context.beginPath();
                context.arc(x, y, r, 0, Math.PI * 2, true);
                context.closePath();
                context.stroke();
                context.fill();
                context.restore();
            }
            function fontheight(font) {
                var r = font.match(/\d+/g)[0];
                return parseFloat(r);
            }
            function decRadius() {
                return _this.smallFont[0] / 8;
            }
            opt = opt || {};
            data.context.clearRect(0, 0, data.width, data.height);
            data.context.fillStyle = opt.fontColor || _this.fontColor;
            if (data.align === "left") {
                data.context.textAlign = "start";
            } else if (data.align === "right") {
                data.context.textAlign = "end";
            } else {
                data.context.textAlign = "center";
            }
            var th = 28,
                x,
                y,
                pad = 2;
            var centerx = data.width / 2,
                centery = data.height / 2,
                txtmeasure;
            var frac = data.numstr.split(".")[1],
                whole = data.numstr.split(".")[0];
            //pad the string if necessary
            var i;
            if (data.cursorpos >= whole.length - 1) {
                for (i = data.cursorpos - (whole.length - 1); i > 0; i--) {
                    whole = "0".concat(whole);
                }
            } else if (data.cursorpos < 0) {
                frac = frac || "";
                for (i = Math.abs(data.cursorpos) - frac.length; i > 0; i--) {
                    frac = frac.concat("0");
                }
            }
            if (frac !== undefined && frac.length > 0) {
                drawCircle(context, centerx, centery, decRadius(), _this.fontColor);
                x = centerx + pad + decRadius();
                context.textAlign = "left";
                context.fillStyle = _this.fontColor;
                context.font = _this.smallFont.join("");
                th = fontheight(context.font);
                y = centery + (th * 0.5);
                //draw the fraction bit
                frac.split("").forEach(function (d, index) {
                    if (data.cursorpos === (index + 1) * -1) {
                        context.save();
                        //draw a cursor and then the number
                        txtmeasure = context.measureText(d);
                        context.fillRect(x, y - th, txtmeasure.width, th);
                        context.fillStyle = (_this.backgroundColor !== "") ? _this.backgroundColor : "#000";
                        context.fillText(d, x, centery);
                        x += txtmeasure.width + pad;
                        context.restore();
                    } else {
                        txtmeasure = context.measureText(d);
                        context.fillText(d, x, centery);
                        x += txtmeasure.width + pad;
                    }
                });
            }
            context.font = _this.font.join("");
            context.textAlign = "right";
            context.fillStyle = _this.fontColor;
            x = centerx - decRadius() - pad;
            th = fontheight(context.font);
            y = centery + (th * 0.5);
            //draw the whole bit in reverse aligning to the right
            whole.split("").reverse().forEach(function (d, index) {
                if (d === "_" && index < whole.length - data.cursorpos) { d = "0"; }
                if (data.cursorpos === index) {
                    context.save();
                    //draw a cursor and then the number
                    txtmeasure = context.measureText(d);
                    context.fillRect(x - txtmeasure.width, y - th, txtmeasure.width, th);
                    context.fillStyle = (_this.backgroundColor !== "") ? _this.backgroundColor : "#000";
                    context.fillText(d, x, centery);
                    x -= (txtmeasure.width + pad);
                    context.restore();
                } else {
                    txtmeasure = context.measureText(d);
                    context.fillText(d, x, centery);
                    x -= (txtmeasure.width + pad);
                }
            });
        }

        opt = opt || {};
        txt = txt || "";
        var _this = this;
        var str = "";
        if (typeof txt === "object") {
            // txt in this case is a PVS state that needs to be parsed
            str = StateParser.resolve(txt, this.displayKey());
            if (str) {
                this.txt = StateParser.evaluate(str);
                if (typeof this.txt === "string") {
                    this.txt = this.txt.replace(new RegExp("\"", "g"), "");
                }
                //read out the display if audio is enabled for this display widget
                if (this.auditoryFeedback() === "enabled") {
                    Speaker.speak(this.txt.toString());
                }
            }
            str = StateParser.resolve(txt, this.cursorName());
            this.cursorpos = StateParser.evaluate(str);
        } else {
            this.txt = txt;
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
        if (this.cursorName() !== "" && !isNaN(parseFloat(this.txt))) {
            renderNumber({
                numstr: parseFloat(this.txt).toString(),
                cursorpos: this.cursorpos,
                context: context,
                align: align,
                height: this.height,
                width: this.width
            }, opt);
        } else {
            renderln({
                txt: this.txt,
                context: context,
                align: align,
                height: this.height,
                width: this.width
            }, opt);
        }
        d3.select("#" + this.id() + "_canvas").style("display", "block");
        d3.select("#" + this.id() + "_span").style("display", "none");

        if (this.touchscreen) {
            this.touchscreenEnabled(false);
            if (this.touchscreenvisibleWhen() !== "") {
                // we need to parse the expression touchscreenvisibleWhen() to understand if the touchscreeen is enabled or not
                var expr = StateParser.simpleExpressionParser(this.touchscreenvisibleWhen());
                if (expr && expr.res) {
                    if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                        this.touchscreenEnabled(true);
                    } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                        // txt in this case is a PVS state that needs to be parsed
                        str = StateParser.resolve(txt, expr.res.attr);
                        if (str) {
                            str = StateParser.evaluate(str);
                            if ((expr.res.binop === "=" && str === expr.res.constant) ||
                                 (expr.res.binop === "!=" && str !== expr.res.constant)) {
                                     this.touchscreenEnabled(true);
                            }
                        }
                    }
                }
            }
        }
        return this.reveal();
    };

    SingleDisplay.prototype.renderGlyphicon = function (icon, opt) {
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
