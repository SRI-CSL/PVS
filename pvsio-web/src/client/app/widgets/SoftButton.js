/**
 * @module TouchscreenButton
 * @version 2.0
 * @description Renders a touchscreen display element.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji
 * @date Sep 15, 2016
 *
 * @example <caption>Typical use of TouchscreenButton APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses TouchscreenButton.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.disp = new TouchscreenButton("disp", { top: 222, left: 96, height: 8, width: 38 });
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
        property = require("util/property");

    /**
     * @function <a name="TouchscreenButton">TouchscreenButton</a>
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
     * @memberof module:TouchscreenButton
     * @instance
     */
    function TouchscreenButton(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;
        this.fontsize = opt.fontsize || (this.height * 0.9);
        this.fontfamily = opt.fontfamily || "sans-serif";
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.8), "px ", this.fontfamily];
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
                        .style("display", "none").attr("id", this.id).attr("class", elemClass);
        this.div.append("span").attr("id", this.id + "_span").attr("class", id + "_span")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top").style("border-radius", "2px");
        this.div.append("canvas").attr("id", this.id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0).style("border-radius", "2px")
                        .style("vertical-align", "top");
        var x2 = this.left + this.width;
        var x3 = this.top + this.height;
        this.div.attr("coords", this.left + "," + this.top + "," + x2 + "," + x3)
                .style("cursor", this.cursor);
        // if (opt.touchscreen) {
        var touchID = this.id;
        this.touchscreenBackgroundColor = opt.touchscreenBackgroundColor || "green";
        this.touchscreenFontColor = opt.touchscreenFontColor || "white";
        this.touchscreen = new Button(touchID, {
            left: this.left, top: this.top, height: this.height, width: this.width
        }, {
            callback: opt.callback || function (err, res) {},
            evts: ['click'],
            area: this.div,
            functionText: opt.functionText,
            visibleWhen: opt.visibleWhen
        });
        this.cursor = opt.cursor || "pointer";
        this.div.style("cursor", this.cursor);
        var _this = this;
        this.div.on("mouseover", function() {
            _this.div.style("background-color", _this.touchscreenBackgroundColor).style("color", _this.touchscreenFontColor);
        }).on("mouseout", function() {
            _this.div.style("background-color", _this.backgroundColor).style("color", _this.fontColor);
        });
        // }
        this.txt = "";

        opt.evts = opt.evts || ["click"];
        opt.functionText = opt.functionText || id;
        opt.softLabel = opt.softLabel || opt.functionText;
        opt.auditoryFeedback = (opt.auditoryFeedback) ? "enabled" : "disabled";
        opt.recallRate = opt.recallRate || 250;
        opt.backgroundColor = opt.backgroundColor || "black";
        opt.touchscreen = {
            callback: opt.callback, functionText: opt.functionText,
            backgroundColor: "green", highlightOnMouseClick: true
        };
        this.evts = property.call(this, opt.evts);
        this.functionText = property.call(this, opt.functionText);
        this.softLabel = property.call(this, opt.softLabel);
        this.buttonReadback = property.call(this, opt.buttonReadback);
        this.recallRate = property.call(this, opt.recallRate);
        this.example = opt.example || ""; // this is used in the prototype builder to demonstrate the font style of the widget

        opt.visibleWhen = (opt.visibleWhen && opt.visibleWhen !== "") ? opt.visibleWhen : "true";
        this.visibleWhen = property.call(this, opt.visibleWhen);

        this.example = opt.example || "btn"; // this is used in the prototype builder to demonstrate the font style of the display
        Widget.call(this, id, "touchscreenbutton");
        return this;
    }
    TouchscreenButton.prototype = Object.create(Widget.prototype);
    TouchscreenButton.prototype.constructor = TouchscreenButton;
    TouchscreenButton.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:TouchscreenButton
    */
    TouchscreenButton.prototype.toJSON = function () {
        return {
            id: this.id(),
            type: this.type(),
            evts: this.evts(),
            recallRate: this.recallRate(),
            functionText: this.functionText(),
            boundFunctions: this.boundFunctions(),
            buttonReadback: this.buttonReadback(),
            softLabel: this.softLabel(),
            visibleWhen: this.visibleWhen(),
            style: {
                backgroundColor: this.backgroundColor,
                fontsize: this.fontsize
            }
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    TouchscreenButton.prototype.updateLocationAndSize = function (pos, opt) {
        TouchscreenButton.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        this.fontsize = this.height * 0.9;
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.8), "px ", this.fontfamily];
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px").style("font-size", this.fontsize + "px");
        d3.select("div." + this.id()).select("span").attr("width", this.width + "px").attr("height", this.height + "px");
        d3.select("div." + this.id()).select("canvas").attr("width", this.width + "px").attr("height", this.height + "px");
        return this.render(this.example, opt);
    };
    /**
     * Removes the widget's div
     */
    TouchscreenButton.prototype.remove = function () {
        TouchscreenButton.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };


    /**
     * @function boundFunctions
     * @returns {String} A comma separated string representing the PVS functions modelling actions over this button.
     * @memberof module:TouchscreenButton
     */
    TouchscreenButton.prototype.boundFunctions = function () {
        if (this.touchscreen) {
            return this.touchscreen.boundFunctions();
        }
        return [];
    };

    TouchscreenButton.prototype.invertColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.render(this.txt, { blinking: elemIsBlinking });
    };

    TouchscreenButton.prototype.invertGlyphiconColors = function () {
        var tmp = this.backgroundColor;
        this.backgroundColor = this.fontColor;
        this.fontColor = tmp;
        var elemIsBlinking = (document.getElementById(this.id()).getAttribute("class").indexOf("blink") >= 0);
        return this.renderGlyphicon(this.txt, { blinking: elemIsBlinking });
    };

    TouchscreenButton.prototype.render = function (txt, opt) {
        opt = opt || {};
        this.txt = this.softLabel() || "";
        var _this = this;
        function doRender() {
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

            // set blinking
            var elemClass = document.getElementById(_this.id()).getAttribute("class");
            if (opt.blinking || _this.blinking) {
                if (elemClass.indexOf("blink") < 0) {
                    elemClass = elemClass + " blink";
                }
            } else {
                elemClass = elemClass.replace(" blink", "");
            }
            document.getElementById(_this.id()).setAttribute("class", elemClass);
            // render content
            var context = document.getElementById(_this.id() + "_canvas").getContext("2d");
            context.textBaseline = _this.textBaseline;
            var align = opt.align || _this.align;
            context.font = _this.font.join("");
            _this.example = _this.txt;

            renderln({ txt: _this.txt, context: context, align: align, height: _this.height, width: _this.width }, opt);

            d3.select("#" + _this.id() + "_canvas").style("display", "block");
            d3.select("#" + _this.id() + "_span").style("display", "none");
            if (_this.touchscreen) {
                if (opt.disableTouch) {
                    _this.div.style("cursor", "default");
                } else {
                    _this.div.style("cursor", _this.cursor);
                }
            }
            return _this.reveal();
        }

        // we need to parse the expression visibleWhen() to understand if the widget is visible or not
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                return doRender();
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                // txt in this case is a PVS state that needs to be parsed
                var str = StateParser.resolve(txt, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str);
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             return doRender();
                    }
                }
            }
        }
        return _this.hide();
    };

    TouchscreenButton.prototype.renderGlyphicon = function (icon, opt) {
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
        span.style.fontSize = 0.8 * this.height + "px";
        d3.select("#" + this.id() + "_canvas").style("display", "none");
        d3.select("#" + this.id() + "_span").style("display", "block");
        this.reveal();
        return this;
    };

    TouchscreenButton.prototype.renderMultiline = function (txt, opt) {
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

    TouchscreenButton.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    TouchscreenButton.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    TouchscreenButton.prototype.move = function (data) {
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

    module.exports = TouchscreenButton;
});
