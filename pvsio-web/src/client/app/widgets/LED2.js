/**
 * @module LED22
 * @version 2.0
 * @description Circular LED2 lights.
 * @author Paolo Masci
 * @date June 1, 2017
 *
 * @example <caption>Typical use of LED2 APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses LED2.
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

    var d3 = require("d3/d3");
    var Widget = require("widgets/Widget"),
        StateParser = require("util/PVSioStateParser"),
        property = require("util/property");


    /**
     * @function <a name="LED2">LED2</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (x1,y1,x2,y2) of the display, specifying
     *        the left, top, right, bottom corner of the rectangle (for shape="rect")
     * @param opt {Object}
     * @memberof module:LED2
     * @instance
     */
    function LED2(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 12;
        this.height = coords.height || 12;
        this.radius = opt.radius || (this.height / 4);
        this.color = opt.color || "#00FF66"; // default is bright green
        this.ledColor = property.call(this, this.color);
        this.blinking = opt.blinking || false;
        this.cursor = opt.cursor || "default";
        opt.position = opt.position || "absolute";
        opt.strokeSize = opt.strokeSize || 3;
        var elemClass = id + " noselect";
        if (this.blinking) { elemClass += " blink"; }
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("opacity", 0.6)
                        .style("display", "block").attr("id", id).attr("class", elemClass);
        this.div.append("svg").attr("id", id + "_svg").attr("class", id + "_svg")
                        .attr("width", this.width).attr("height", this.height)
                        .style("margin", 0).style("padding", 0)
                        .style("vertical-align", "top")
                        .append("circle").attr("cx", this.width / 2).attr("cy", this.height / 2)
                        .attr("r", (Math.min(this.width, this.height) - opt.strokeSize) / 2)
                        .attr("stroke", this.ledColor()).attr("stroke-width", opt.strokeSize).attr("fill", "transparent");
        this.div.style("cursor", this.cursor);
        this.isOn = false;
        opt.ledKey = opt.ledKey || id;
        opt.visibleWhen = opt.visibleWhen || "true"; // default: always on
        this.ledKey = property.call(this, opt.ledKey);
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.example = opt.example || ""; // this is used in the prototype builder to demonstrate the font style of the display
        Widget.call(this, id, "led");
        return this;
    }
    LED2.prototype = Object.create(Widget.prototype);
    LED2.prototype.constructor = LED2;
    LED2.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:LED2
    */
    LED2.prototype.toJSON = function () {
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
    LED2.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            LED2.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
        }
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        this.radius = this.height / 4;
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px");
        d3.select("div." + this.id()).select("svg").attr("width", this.width + "px").attr("height", this.height + "px");
        return this.render(this.example, opt);
    };
    LED2.prototype.updateWithProperties = function (props) {
        props = props || {};
        props.ledColor = props.ledColor || "#00FF66"; // default is bright green
        props.visibleWhen = props.visibleWhen || "true";
        LED2.prototype.parentClass.updateWithProperties.apply(this, arguments);
        return this.render(this.example);
    };
    /**
     * Removes the widget's div
     */
    LED2.prototype.remove = function () {
        LED2.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };

    LED2.prototype.render = function (txt, opt) {
        opt = opt || {};
        // var color = (typeof txt === "string") ? txt : 
        //                 (opt.color) ? opt.color : this.ledColor();
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            //-- handles blinking option
            if (opt.blinking && !this.blinking) {
                this.div.attr("class", this.div.attr("class") + " blink");
            }

            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                return this.reveal();
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(txt, expr.res.attr);
                if (str) {
                    str = StateParser.evaluate(str);
                    if ((expr.res.binop === "=" && str === expr.res.constant) ||
                         (expr.res.binop === "!=" && str !== expr.res.constant)) {
                             return this.reveal();
                    }
                }
            }
        }
        return this.hide();
    };
    LED2.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.example;
        return this.render(txt, { visibleWhen: "true" });
    };


    LED2.prototype.toggle = function (opt) {
        if (this.isOn === true) {
            this.hide();
        } else {
            this.reveal();
        }
        return this;
    };

    LED2.prototype.on = function (opt) {
        this.isOn = true;
        var elemClass = this.id() + " noselect";
        if (this.blinking) { elemClass += " blink"; }
        this.div.attr("class", elemClass);
        this.render("", opt);
        return this;
    };

    LED2.prototype.off = function () {
        this.isOn = false;
        this.hide();
        return this;
    };

    LED2.prototype.blink = function (n) {
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

    LED2.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    LED2.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    module.exports = LED2;
});
