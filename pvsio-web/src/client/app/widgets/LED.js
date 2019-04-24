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
    var Widget = require("widgets/Widget"),
        StateParser = require("util/PVSioStateParser"),
        property = require("util/property");


    /**
     * @function <a name="LED">LED</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 12, height: 12 }.
     * @param opt {Object} Rendering options:
     *        <li>radius (Number) Radius of the LED</li>
     *        <li>color (String) Color of the LED (must be a valid HTML color. Default is bright green "#00FF66")</li>
     *        <li>blinking (Bool) Whether the LED is blinking (default: false, i.e., not blinking)</li>
     *        <li>ledKey (string) The name of the state attribute defining the color of the display. This information will be used by the render method. Default is the ID of the display.</li>
     * @memberof module:LED
     * @instance
     */
    function LED(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 12;
        this.height = coords.height || 12;
        this.radius = opt.radius || (this.height / 4);
        this.color = opt.color || "#00FF66"; // default is bright green
        this.blinking = opt.blinking || false;
        this.cursor = opt.cursor || "default";
        opt.position = opt.position || "absolute";
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("display", "block").attr("id", id).attr("class", id);
        this.div.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top");
        this.div.style("cursor", this.cursor);
        this.isOn = false;
        opt.ledKey = opt.ledKey || id;
        opt.visibleWhen = opt.visibleWhen || "true"; // default: always on
        this.ledKey = property.call(this, opt.ledKey);
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.ledColor = property.call(this, this.color);
        this.example = opt.example || ""; // this is used in the prototype builder to demonstrate the font style of the display
        Widget.call(this, id, "led");
        return this;
    }
    LED.prototype = Object.create(Widget.prototype);
    LED.prototype.constructor = LED;
    LED.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:LED
    */
    LED.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            visibleWhen: this.visibleWhen(),
            ledKey: this.ledKey(),
            ledColor: this.ledColor()
        };
    };
    /**
     * Updates the location and size of the widget according to the given position and size
     */
    LED.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            LED.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        }
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        this.radius = this.height / 4;
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px");
        d3.select("div." + this.id()).select("canvas").attr("width", this.width + "px").attr("height", this.height + "px");
        return this.render(this.example, opt);
    };
    LED.prototype.updateWithProperties = function (props) {
        props = props || {};
        props.ledColor = props.ledColor || "#00FF66"; // default is bright green
        props.visibleWhen = props.visibleWhen || "true";
        LED.prototype.parentClass.updateWithProperties.apply(this, arguments);
        return this.render(this.example);
    };
    /**
     * Removes the widget's div
     */
    LED.prototype.remove = function () {
        LED.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };

    LED.prototype.render = function (txt, opt) {
        opt = opt || {};
        txt = txt || "#00FF66"; // default is light green
        var color = (opt.color) ? opt.color : 
                        (typeof txt === "string") ? txt : this.ledColor();
        var _this = this;
        function doRender() {
            var context = document.getElementById(_this.id() + "_canvas").getContext("2d");
            context.beginPath();
            context.globalAlpha = 0.9;
            context.arc(_this.width / 2, _this.height / 2, _this.radius, 0, 2 * Math.PI, false);
            context.fillStyle = color;
            context.fill();
            if (!opt.noborder) { context.stroke(); }
            if (opt.blinking && !_this.blinking) {
                _this.div.attr("class", _this.div.attr("class") + " blink");
            }
            return _this.reveal();
        }

        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                return doRender();
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(txt, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str).toString();
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             return doRender();
                    }
                }
            }
        }
        return _this.hide();
    };
    LED.prototype.renderSample = function (opt) {
        return this.render("#00FF66");
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
        this.render("", opt);
        return this;
    };

    LED.prototype.off = function () {
        this.isOn = false;
        this.hide();
        return this;
    };

    LED.prototype.blink = function (n) {
        if (n && typeof n === "number") {
            var wasOn = this.isOn;
            var _this = this;
            window.setTimeout(function () {
                _this.div.attr("class", _this.id() + " noselect");
                if (!wasOn) {
                    _this.off();
                }
            }, n * 1000);
        }
        this.div.attr("class", this.id() + " noselect blink");
        this.reveal();
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
