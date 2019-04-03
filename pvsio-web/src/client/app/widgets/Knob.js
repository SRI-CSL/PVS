/**
 * @module Knob
 * @desc Knob Widget, based on https://codepen.io/blucube/pen/cudAz
 * @author Paolo Masci
 * @date August 4, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Button = require("widgets/Button");

    /**
     * @function <a name="LED">LED</a>
     * @description Constructor.
     * @param id {String} The ID of the HTML element where the display will be rendered.
     * @param coords {Object} The four coordinates (x1,y1,x2,y2) of the display, specifying
     *        the left, top, right, bottom corner of the rectangle (for shape="rect")
     * @param opt {Object}
     * @memberof module:SingleDisplay
     * @instance
     */
    function Knob(id, coords, opt) {
        opt = opt || {};
        this.id = id;
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 60;
        this.height = coords.height || 60;
        this.font = [this.height, "px ", (opt.font || "sans-serif")];
        this.smallFont = (this.height * 0.8) + "px " + (opt.font || "sans-serif");
        this.align = opt.align || "center";
        this.textBaseline = "middle";
        this.radius = opt.radius || this.width / 2;
        this.color = opt.color || "#00FF66"; // default is light green
        this.blinking = opt.blinking || false;
        this.div = d3.select(this.parent)
                        .append("div").style("position", "absolute")
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0)
                        .style("display", "block").attr("id", id).attr("class", id);
        this.div = this.div.append("div").attr("class", "knob-labels");
        this.div = this.div.append("div").attr("class", "switch")
                        .attr("width", "100%").attr("height", "100%");
        this.label_left = this.div.append("div").append("span").style("font-size", "10px").style("position", "absolute")
                        .style("font-family", "sans-serif").text("inHg")
                        .style("margin-top", "-26px").style("cursor", "pointer")
                        .style("margin-left", "-10px").style("color", "whitesmoke");
        this.label_right = this.div.append("div").append("span").style("font-size", "10px").style("position", "absolute")
                        .style("font-family", "sans-serif").text("hPa")
                        .style("margin-top", "-26px").style("cursor", "pointer")
                        .style("margin-left", "44px").style("color", "whitesmoke");
        var notch = this.div.append("div").attr("class", id + "_notch").style("left", "42%").style("top", "10px")
                        .style("position", "absolute").style("width", "0px").style("height", "12px")
                        .style("border-left", "5px solid transparent").style("border-right", "5px solid transparent")
                        .style("border-bottom", "5px solid whitesmoke")
                        .style("transform", "rotate(-36deg) translate(-10px,-8px)");

        opt.switch_left = opt.switch_left || {};
        this.switch_left = new Button("inHg", {
            left: this.left - 15, top: this.top - 45, height: 26, width: 26
        }, {
            callback: opt.callback,
            animation: function() { notch.style("-webkit-transition", ".6s all").style("-webkit-transform-origin", "0% 100%").style("transform", "rotate(-36deg) translate(-8px,-4px)");},
            evts: opt.switch_left.events || ['click'],
            area: this.label_left,
            keyCode:37,
            keyName:"arrow left"
        });
        opt.switch_right = opt.switch_right || {};
        this.switch_right = new Button("hPa", {
            left: this.left + 45, top: this.top - 45, height: 26, width: 26
        }, {
            callback: opt.callback,
            animation: function () { notch.style("-webkit-transition", ".6s all").style("-webkit-transform-origin", "0% 100%").style("transform", "rotate(36deg) translate(4px,-6px)"); },
            evts: opt.switch_right.events || ['click'],
            area: this.label_right,
            keyCode:39,
            keyName:"arrow right"
        });

        opt.switch_central = opt.switch_central || {};
        this.switch_central = this.div.append("div").attr("id", id).attr("class", id)
                        .style("margin-left", "16px").style("margin-top", "26px")
                        .style("width", "26px").style("height", "26px").style("border-width", 0);
        this.switch_central = new Button(id + "_button", {
            left: this.left + 15, top: this.top - 15, height: 26, width: 26
        }, {
            callback: opt.callback || function (err, res) {},
            evts: opt.switch_central.events || ['click'],
            area: this.switch_central,
            keyCode:17,
            keyName:"ctrl"
        });
        this.div.style("cursor", "pointer");

        return this;
    }

    Knob.prototype.render = function (opt) {
        // opt = opt || {};whitesmoke
        // opt.notchPosition = opt.notchPosition || 5.5;
        // var context = document.getElementById(this.id + "_canvas").getContext("2d");
        // context.beginPath();
        // context.globalAlpha = 0.9;
        // var cX = this.width / 2;
        // var cY = this.height / 2;
        // context.moveTo(cX,cY);
        // context.arc(cX, cY, this.radius, opt.notchPosition, opt.notchPosition + 0.3, false);
        //
        // context.fillStyle = opt.color || this.color;
        // context.fill();
        // //border
        // context.lineWidth = 5;
        // context.strokeStyle = '#003300';
        // context.stroke();
        // var elemClass = this.div.node().getAttribute("class");
        // if (opt.blinking || this.blinking) {
        //     elemClass += " blink";
        // } else {
        //     elemClass = elemClass.replace(" blink", "");
        // }
        // this.div.node().setAttribute("class", elemClass);
        // this.reveal();
        // return this;
    };


    Knob.prototype.toggle = function (opt) {
        if (this.isOn === true) {
            this.hide();
        } else {
            this.reveal();
        }
        return this;
    };

    Knob.prototype.on = function (opt) {
        this.isOn = true;
        this.render(opt);
        return this;
    };

    Knob.prototype.off = function () {
        this.isOn = false;
        this.hide();
        return this;
    };

    Knob.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    Knob.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    module.exports = Knob;
});
