/**
 * @module Syringe
 * @version 1.0
 * @description Renders a syringe
 * @author Paolo Masci
 * @date June 1, 2017
 *
 * @example <caption>Typical use of Syringe APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses Syringe.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.syringe = new Syringe("salive", { top: 222, left: 96, height: 8, width: 38 });
 *     device.syringe.render();
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Widget = require("widgets/Widget"),
        syringe_data = require("text!widgets/medical/Syringe/syringe_data.svg"),
        vial_data = require("text!widgets/medical/Syringe/vial_data.svg"),
        infusions_set_data = require("text!widgets/medical/Syringe/springs.svg"),
        lateral_connection_line_data = require("text!widgets/medical/Syringe/connection-line.svg"),
        StateParser = require("util/PVSioStateParser"),
        property = require("util/property");

    // range goes from 0 to 20
    var FULLY_RETRACTED = 22;
    // var FULLY_PUSHED = 0;

    var animation_duration = 250; //ms

    function getLevel(val, opt) {
        opt = opt || {};
        var levels = [
            { level: 0, fluid: "scale(1,0)", plunger: "translate(0,575)", plunger_stick: "translate(0,90)" },
            { level: 1, fluid: "scale(1,0.1) translate(0,33100)", plunger: "translate(0,522)", plunger_stick: "translate(0,90)" },
            { level: 2, fluid: "scale(1,0.2) translate(0,14710)", plunger: "translate(0,470)", plunger_stick: "translate(0,370) scale(1,0.9)" },
            { level: 3, fluid: "scale(1,0.28) translate(0,9450)", plunger: "translate(0,414)", plunger_stick: "translate(0,420) scale(1,0.9)" },
            { level: 4, fluid: "scale(1,0.4) translate(0,5550)", plunger: "translate(0,360)", plunger_stick: "translate(0,630) scale(1,0.8)" },
            { level: 5, fluid: "scale(1,0.49) translate(0,3850)", plunger: "translate(0,300)", plunger_stick: "translate(0,700) scale(1,0.8)" },
            { level: 6, fluid: "scale(1,0.61) translate(0,2350)", plunger: "translate(0,250)", plunger_stick: "translate(0,900) scale(1,0.7)" },
            { level: 7, fluid: "scale(1,0.71) translate(0,1500)", plunger: "translate(0,200)", plunger_stick: "translate(0,900) scale(1,0.7)" },
            { level: 8, fluid: "scale(1,0.81) translate(0,870)", plunger: "translate(0,140)", plunger_stick: "translate(0,1150) scale(1,0.6)" },
            { level: 9, fluid: "scale(1,0.91) translate(0,370)", plunger: "translate(0,90)", plunger_stick: "translate(0,1200) scale(1,0.6)" },
            { level: 10, fluid: "scale(1,1)", plunger: "translate(0,30)", plunger_stick: "translate(0,1250) scale(1,0.6)" },
            { level: 11, fluid: "scale(1,1.1) translate(0,-330)", plunger: "translate(0,-20)", plunger_stick: "translate(0,1320) scale(1,0.6)" },
            { level: 12, fluid: "scale(1,1.2) translate(0,-600)", plunger: "translate(0,-70)", plunger_stick: "translate(0,1420) scale(1,0.56)" },
            { level: 13, fluid: "scale(1,1.3) translate(0,-840)", plunger: "translate(0,-130)", plunger_stick: "translate(0,1420) scale(1,0.56)" },
            { level: 14, fluid: "scale(1,1.39) translate(0,-1010)", plunger: "translate(0,-180)", plunger_stick: "translate(0,1620) scale(1,0.5)" },
            { level: 15, fluid: "scale(1,1.49) translate(0,-1200)", plunger: "translate(0,-230)", plunger_stick: "translate(0,1620) scale(1,0.5)" },
            { level: 16, fluid: "scale(1,1.57) translate(0,-1330)", plunger: "translate(0,-290)", plunger_stick: "translate(0,1900) scale(1,0.4)" },
            { level: 17, fluid: "scale(1,1.64) translate(0,-1430)", plunger: "translate(0,-340)", plunger_stick: "translate(0,1900) scale(1,0.4)" },
            { level: 18, fluid: "scale(1,1.74) translate(0,-1560)", plunger: "translate(0,-390)", plunger_stick: "translate(0,2150) scale(1,0.3)" },
            { level: 19, fluid: "scale(1,1.83) translate(0,-1660)", plunger: "translate(0,-450)", plunger_stick: "translate(0,2350) scale(1,0.2)" },
            { level: 20, fluid: "scale(1,1.91) translate(0,-1750)", plunger: "translate(0,-500)", plunger_stick: "translate(0,2350) scale(1,0.2)" },
            { level: 21, fluid: "scale(1,1.91) translate(0,-1830)", plunger: "translate(0,-650)", plunger_stick: "translate(0,2650) scale(1,0.13)" },
            { level: 22, fluid: "scale(1,1.91) translate(0,-1860)", plunger: "translate(0,-750)", plunger_stick: "translate(0,2750) scale(1,0.1)" }
        ];
        var x = 0;
        if (opt.large_syringe) {
            x = parseInt(val/10);
        }
        x = (x > 22) ? 22
            : (x < 0) ? 0
            : x;
        return levels[x];
    }

    var large_syringe_style = "translate(-82,0) scale(0.2,0.1)";

    var needle_style = {
        blue: "url(#linearGradientBlue)",
        green: "url(#linearGradientGreen)",
        red: "url(#linearGradientRed)",
        yellow: "url(#linearGradientGreen)",
        orange: "url(#linearGradientOrange)"
    };

    /**
     * @function <a name="Syringe">Syringe</a>
     * @description Constructor.
     * @param id {String} The ID of the display.
     * @param coords {Object} The four coordinates (top, left, width, height) of the syringe, specifying
     *        the left, top corner, and the width and height of the div element containing the syringe.
     *        Default is { top: 0, left: 0, width: 80, height: 200 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     *          ...
     * @memberof module:Syringe
     * @instance
     */
    function Syringe(id, coords, opt) {
        opt = opt || {};
        this.id = property.call(this, id);
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 80;
        this.height = coords.height || ((opt.automatic_plunger) ? 383 : 690);
        opt.position = opt.position || "absolute";
        opt.opacity = opt.opacity || 1;
        opt.visibleWhen = opt.visibleWhen || "true";
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.example = opt.example || FULLY_RETRACTED; // example is used in the prototype builder to demonstrate the font style of the display

        var elemClass = id + " syringeWidget" + " noselect ";
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("opacity", opt.opacity)
                        .style("display", "block").attr("id", id).attr("class", elemClass);

        this.syringe = this.div.append("div").attr("id", id + "_syringe").html(syringe_data);
        this.fluid_color = opt.fluid_color || '#0fe95d'; // bright green
        this.fluid = this.div.select("svg g#fluid");
        this.fluid.select("path").style("fill",this.fluid_color);
        this.contains_fluid = opt.contains_fluid || false;
        this.plunger = this.div.select("svg g#plunger");
        this.plunger_stick = this.div.select("svg g#plunger #stick");
        this.plunger_tail = this.div.select("svg g#plunger #tail");
        this.plunger_head = this.div.select("svg g#plunger #head");
        this.automatic_plunger = opt.automatic_plunger || false;
        if (this.automatic_plunger) {
            this.plunger_tail.attr("style", "display:none;");
        }
        this.plungerLevel = opt.plungerLevel || FULLY_RETRACTED;
        this.large_syringe = opt.large_syringe;
        this.syringe = this.div.select("svg g#syringe");
        this.graduation_marks = d3.select(this.syringe.node().parentNode).select("g#graduation_marks");
        if (this.large_syringe) {
            this.syringe.attr("transform", large_syringe_style);
            this.graduation_marks.selectAll(".large_syringe").style("display", "block");
        }

        this.needle_base = this.div.select("svg g#needle_base");
        this.shaft = this.div.select("svg g#shaft");
        this.needle_color = opt.needle_color || "url(#linearGradient550)";
        if (opt.needle_style && needle_style[opt.needle_style]) {
            this.needle_color = needle_style[opt.needle_style];
        }
        this.needle_base.selectAll("path").style("fill", this.needle_color);
        this.has_needle = opt.has_needle || false;

        this.fittings = opt.fittings || null;
        // -- the following fittingss are available in the current implementation;
        // shaft
        this.shaft.style("display", "block");
        // - vial
        this.vial = this.div.append("div").attr("id", id + "_vial").html(vial_data);
        this.vial.attr("style", "transform: rotate(180deg);")
                 .style("margin-top", "-600px")
                 .style("margin-right", "-8px");
        if (this.fittings && this.fittings === "vial") {
            this.vial.select("svg g").style("display", "block");
        } else {
            this.vial.select("svg g").style("display", "none");
        }
        // spring infusion set
        this.infusionSet = this.div.append("div").attr("id", id + "_infusionSet").html(infusions_set_data);
        //this.infusionSet.attr("style", "margin-top:-664px; margin-left:-30px; transform:scale(0.6,0.6)");
        this.infusionSet.attr("style", "margin-top:-204px; margin-left:7px; transform:scale(0.6,0.6)");
        if (this.fittings && this.fittings === "infusionSet") {
            this.infusionSet.style("display", "block");
            this.shaft.style("display", "none");
        } else {
            this.infusionSet.style("display", "none");
        }
        // lateral connection line
        this.lateralConnectionLine = this.div.append("div").attr("id", id + "_lateralConnectionLine").html(lateral_connection_line_data);
        //this.lateralConnectionLine.attr("style", "margin-top:-467px; margin-left:-130px; transform:scale(0.85,0.5)");
        this.lateralConnectionLine.attr("style", "margin-top:-39px; margin-left:-130px; transform:scale(0.85,0.7)");
        if (this.fittings && this.fittings === "lateralConnectionLine") {
            this.lateralConnectionLine.style("display", "block");
            this.shaft.style("display", "none");
        } else {
            this.lateralConnectionLine.style("display", "none");
        }

        this.plunger.attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).plunger);
        this.fluid.attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).fluid);

        Widget.call(this, id, "syringe");
        return this;
    }
    Syringe.prototype = Object.create(Widget.prototype);
    Syringe.prototype.constructor = Syringe;
    Syringe.prototype.parentClass = Widget.prototype;
    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof module:Syringe
    */
    Syringe.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            visibleWhen: this.visibleWhen(),
            plungerLevel: this.plungerLevel,
            fluid_color: this.fluid_color
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    Syringe.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        this.top = pos.y || 0;
        this.left = pos.x || 0;
        this.width = pos.width || 200;
        this.height = pos.height || 80;
        d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
            .style("width", this.width + "px").style("height", this.height + "px");
        return this.render();
    };
    /**
     * Removes the widget's div
     */
    Syringe.prototype.remove = function () {
        Syringe.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };
    Syringe.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.example;
        return this.render(txt, { visibleWhen: "true" });
    };
    Syringe.prototype.render = function (level, opt) {
        opt = opt || {};
        var isEnabled = false;
        var visibleWhen = opt.visibleWhen || this.visibleWhen();
        var expr = StateParser.simpleExpressionParser(visibleWhen);
        if (expr && expr.res) {
            if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                isEnabled = true;
            } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                var str = StateParser.resolve(level, expr.res.attr);
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
            level = (level !== null && level !== undefined) ? level : this.plungerLevel;
            if (typeof level === "object") {
                // txt in this case is a PVS state that needs to be parsed
                var l = StateParser.resolve(level, this.displayKey());
                if (l) {
                    this.plungerLevel = StateParser.evaluate(l);
                }
            } else {
                this.plungerLevel = parseInt(level);
            }
            // render content
            if (opt.has_needle || this.has_needle) {
                this.shaft.style("display", "block");
                this.needle_color = opt.needle_color || this.needle_color || "url(#linearGradient550)";
                if (opt.needle_style && needle_style[opt.needle_style]) {
                    this.needle_color = needle_style[opt.needle_style];
                }
                this.needle_base.selectAll("path").style("fill", this.needle_color);
            } else {
                this.shaft.style("display", "none");
            }
            if (opt.contains_fluid === true ||this.contains_fluid === true) {
                this.fluid.select("svg g").attr("display", "block");
                this.fluid.transition().duration(animation_duration).attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).fluid);
            } else {
                this.fluid.select("svg g").attr("display", "none");
            }
            if (this.automatic_plunger) {
                if (this.div.select("svg g").style("display") === "none") {
                    this.plunger.attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).plunger);
                    this.plunger_stick.attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).plunger_stick);
                } else {
                    this.plunger.transition().duration(animation_duration).attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).plunger);
                    this.plunger_stick.transition().duration(animation_duration).attr("transform", getLevel(this.plungerLevel, { large_syringe: this.large_syringe }).plunger_stick);
                }
                this.plunger_tail.attr("style", "display:none;");
            }
            if (this.fittings === "vial" || opt.fittings === "vial") {
                this.vial.select("svg g").style("display", "block");
                this.infusionSet.style("display", "none");
                this.lateralConnectionLine.style("display", "none");
            } else if (this.fittings === "infusionSet" || opt.fittings === "infusionSet") {
                this.vial.select("svg g").style("display", "none");
                this.shaft.style("display", "none");
                // this.needle_base.selectAll("path").style("fill", "gray");
                this.infusionSet.style("display", "block");
            } else if (this.fittings === "lateralConnectionLine" || opt.fittings === "lateralConnectionLine") {
                this.vial.select("svg g").style("display", "none");
                this.infusionSet.style("display", "none");
                this.lateralConnectionLine.style("display", "block");
                this.shaft.style("display", "none");
                // this.needle_base.selectAll("path").style("fill", "gray");
            } else {
                this.vial.select("svg g").style("display", "none");
                this.infusionSet.style("display", "none");
                this.lateralConnectionLine.style("display", "none");
            }
            return this.reveal();
        }
        return this.hide();
    };
    Syringe.prototype.plugVial = function () {
        this.fittings = "vial";
        return this.render();
    };
    Syringe.prototype.removeVial = function () {
        return this.removeFittings();
    };
    Syringe.prototype.plugInfusionSet = function () {
        this.fittings = "infusionSet";
        return this.render();
    };
    Syringe.prototype.removeInfusionSet = function () {
        return this.removeFittings();
    };
    Syringe.prototype.plugLateralConnectionLine = function () {
        this.fittings = "lateralConnectionLine";
        return this.render();
    };
    Syringe.prototype.removeLateralConnectionLine = function () {
        return this.removeFittings();
    };
    Syringe.prototype.containsFluid = function () {
        this.contains_fluid = true;
        return this.render();
    };
    Syringe.prototype.noFluid = function () {
        this.contains_fluid = false;
        return this.render();
    };
    Syringe.prototype.removeFittings = function () {
        this.fittings = null;
        return this.render();
    };
    Syringe.prototype.hide = function () {
        this.div.select("svg g").style("display", "none");
        this.shaft.style("display", "none");
        this.infusionSet.style("display", "none");
        this.lateralConnectionLine.style("display", "none");
        this.vial.select("svg g").style("display", "none");
        return this;
    };
    Syringe.prototype.reveal = function () {
        this.div.select("svg g").style("display", "block");
        return this;
    };
    Syringe.prototype.move = function (data) {
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
    /**
     * Sets plunger level
     */
    Syringe.prototype.setPlungerLevel = function(level) {
        if (level !== null && level !== undefined) {
            this.plungerLevel = level;
            this.render();
        }
        return this;
    };

    module.exports = Syringe;

});
