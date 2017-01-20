/**
 * @module TouchscreenButton
 * @version 2.0
 * @description Renders a touchscreen button using bootstrap styles
 * @author Paolo Masci
 * @date May 24, 2015
 *
 * @example <caption>Typical use of TouchscreenButton APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses SingleDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var touchscreen = {};
 *     touchscreen.ok_btn = new TouchscreenButton("ok");
 *     touchscreen.render(); // the display renders 10
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, dimColor */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3"),
        property = require("util/property"),
        StateParser = require("util/PVSioStateParser"),
        Widget = require("widgets/Widget"),
        Button = require("widgets/Button"),
        BasicDisplay = require("widgets/BasicDisplay");

    /**
     * @function <a name="TouchscreenButton">TouchscreenButton</a>
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
    function TouchscreenButton(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 32;
        this.height = coords.height || 10;
        this.fontsize = opt.fontsize || (this.height * 0.9);
        this.fontfamily = opt.fontfamily || "sans-serif";
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        this.align = opt.align || "center";
        this.backgroundColor = opt.backgroundColor || "black";
        this.fontColor = opt.fontColor || "#fff"; //white
        this.cursor = opt.cursor || "pointer";
        this.blinking = opt.blinking || false;
        this.textBaseline = "middle";
        this.btnClass = opt.btnClass || "primary";
        var elemClass = id + " noselect";
        if (this.blinking) { elemClass += " blink"; }
        opt.position = opt.position || "absolute";
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("border-width", 0)
                        .style("display", "none").attr("id", id).attr("class", elemClass)
                        .style("cursor", this.cursor);

        opt.functionText = opt.functionText || id;
        this.functionText = property.call(this, opt.functionText);
        opt.evts = opt.evts || [ "click" ];
        this.evts = property.call(this, opt.evts);
        opt.buttonReadback = opt.buttonReadback || "";
        this.buttonReadback = property.call(this, opt.buttonReadback);
        this.overlayButton = new Button(id + "_overlayButton", {
            left: this.left, top: this.top, height: this.height, width: this.width
        }, {
            functionText: opt.functionText,
            callback: opt.callback,
            buttonReadback: opt.buttonReadback,
            evts: opt.evts,
            area: this.div,
            parent: id
        });

        opt.displayKey = opt.displayKey || id;
        this.displayKey = property.call(this, opt.displayKey);
        this.overlayDisplay = new BasicDisplay(id + "_overlayDisplay", {
            height: this.height, width: this.width
        }, {
            displayKey: opt.displayKey,
            fontsize: this.fontsize,
            fontColor: this.fontColor,
            backgroundColor: this.backgroundColor,
            cursor: this.cursor,
            position: "relative",
            parent: id
        });
        var _this = this;
        d3.select("#" + id + "_overlayDisplay").on("mouseover", function () {
            if (_this.backgroundColor !== "transparent") {
                _this.overlayDisplay.setColors({ backgroundColor: dimColor(_this.backgroundColor) });
            }
        }).on("mouseout", function () {
            if (_this.backgroundColor !== "transparent") {
                _this.overlayDisplay.setColors({ backgroundColor: _this.backgroundColor });
            }
        }).on("mousedown", function () {
            if (_this.backgroundColor !== "transparent") {
                _this.overlayDisplay.setColors({
                    backgroundColor: "black",
                    fontColor: "white"
                });
            }
        }).on("mouseup", function () {
            if (_this.backgroundColor !== "transparent") {
                _this.overlayDisplay.setColors({
                    backgroundColor: _this.backgroundColor,
                    fontColor: _this.fontColor
                });
            }
            _this.overlayButton.click();
        });

        opt.visibleWhen = opt.visibleWhen || "true"; // default: always enabled/visible
        this.visibleWhen = property.call(this, opt.visibleWhen);
        opt.softLabel = opt.softLabel || "";
        this.softLabel = property.call(this, opt.softLabel);
        this.example = opt.example || "btn";
        Widget.call(this, id, "touchscreenbutton");
        return this;
    }
    TouchscreenButton.prototype = Object.create(Widget.prototype);
    TouchscreenButton.prototype.constructor = TouchscreenButton;
    TouchscreenButton.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:BasicDisplay
    */
    TouchscreenButton.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            evts: this.evts(),
            displayKey: this.displayKey(),
            functionText: this.functionText(),
            boundFunctions: this.overlayButton.boundFunctions(),
            buttonReadback: this.buttonReadback(),
            visibleWhen: this.visibleWhen(),
            softLabel: this.softLabel(),
            fontsize: this.fontsize,
            fontColor: this.fontColor,
            backgroundColor: this.backgroundColor
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    TouchscreenButton.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            TouchscreenButton.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
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
        // only resize is needed (and not translation x y), because we have already moved the div element containing the display and button areas
        this.overlayDisplay.updateLocationAndSize({ width: pos.width, height: pos.height });
        this.overlayButton.updateLocationAndSize({ width: pos.width, height: pos.height });
        return this.render(this.example, opt);
    };
    TouchscreenButton.prototype.updateStyle = function (data) {
        data = data || {};
        this.fontsize = data.fontsize || this.fontsize;
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        this.fontColor = data.fontColor || this.fontColor;
        this.backgroundColor = data.backgroundColor || this.backgroundColor;
        return this;
    };
    TouchscreenButton.prototype.updateWithProperties = function (props) {
        TouchscreenButton.prototype.parentClass.updateWithProperties.apply(this, arguments);
        this.overlayButton.updateWithProperties(props);
        return this;
    };    
    /**
     * Removes the widget's div
     */
    TouchscreenButton.prototype.remove = function () {
        TouchscreenButton.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };


    TouchscreenButton.prototype.render = function (state, opt) {
        // state is used to check whether the button is visible/enabled
        // the expression visibleWhen() is the condition we need to check on the state
        opt = opt || {};
        var isVisible = false;
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                isVisible = true;
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(state, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str);
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             isVisible = true;
                    }
                }
            }
        }
        if (isVisible) {
            this.overlayDisplay.render(this.softLabel(), opt);
            this.overlayButton.reveal(opt);
            return this.reveal();
        }
        return this.hide();
    };
    TouchscreenButton.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.softLabel();
        return this.render(txt, { visibleWhen: "true" });
    };


    TouchscreenButton.prototype.renderGlyphicon = function (icon, opt) {
        opt = opt || {};
        var button = document.getElementById(this.id + "_button");
        this.txt = icon;
        button.setAttribute("class", "glyphicon " + icon + " btn btn-" + this.btnClass + " center");
        button.style.width = this.width;
        button.style.height = this.height;
        button.style.fontSize = 0.8 * this.height + "px";
        button.textContent = opt.txt || this.txt;
        d3.select("#" + this.id + "_button").style("display", "block");
        return this.reveal();
    };


    TouchscreenButton.prototype.hide = function (opt) {
        opt = opt || {};
        this.overlayButton.hide(); //FIXME: the mouse cursor is still a pointer when hovering the button area, because of map#prototypeMap
        this.div.style("display", "none");
        this.cursor = opt.pointer || "default";
        return this;
    };

    TouchscreenButton.prototype.reveal = function (opt) {
        opt = opt || {};
        this.div.style("display", "block");
        this.cursor = opt.pointer || "pointer";
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
