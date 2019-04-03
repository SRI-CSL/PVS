/**
 * @module SliderWidget
 * @version 2.0
 * @description Renders a slider
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 *              Uses http://seiyria.com/bootstrap-slider/
 * @author Paolo Masci
 * @date Sep 15, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses SliderWidget
 // The following require.config assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demos/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib",
         text: "../lib/text",
         stateParser: "./util/PVSioStateParser"
     }
 });
 require(["widgets/SliderWidget"], function (SliderWidget) {
     "use strict";
     var device = {};
     device.sliderS1 = new SliderWidget("sliderS1", {
       top: 350, left: 120, width: 120
     }, {
       max: 340,
       min: 0,
       init: 100, // initial value selected by the slider
       callback: function (err, data) { console.log("adjusting slider"); console.log(data); }
    });
    device.sliderS1.render();  // The slider is rendered at the initial value.
    device.sliderS1.slide(20); // The slide is adjusted at value 20.
                               // Every time the slider is adjusted to a given value, a command
                               // "slide_<slider name>"(<current val>)(<current state>)" is sent to the PVS back-end
});
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Slider */
define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var triangle_template = require("text!widgets/templates/triangle_template.handlebars");
    var Widget = require("widgets/Widget"),
        Button = require("widgets/Button"),
        property = require("util/property");

    /**
     * @function <a name="SliderWidget">SliderWidget</a>
     * @description Constructor.
     * @param id {String} The ID of the display.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 104, height: 250 }.
     * @param opt {Object} Options:
     *          <li>customFunctionText (String): custom PVS function to be associated with slider actions. This function overrides the default association ("slide_" + id).</li>
     *          <li>backgroundColor (String): background display color (default is transparent)</li>
     *          <li>handle (Object): visual aspect of the slider handle. The object attributes are:<br>
     *             - type (String): one of "round", "rect", "triangle" (default: rect)<br>
     *             - height (Number): height of the slider handle<br>
     *             - width (Number): width of the slider handle<br>
     *             - backgroundColor (String): background color for the handle<br>
     *             - borderColor (String): border color for the handle<br>
     *             - borderWidth (String): border width for the handle<br>
     *             - fontColor (String): font color for the text displayed in the handle<br>
     *             - opacity (Number): opacity level of the handle<br>
     *             - zIndex (Number): z-index of the handle<br>
     *           </li>
     *          <li>init (init): initial value selected by the slider
     *          <li>innerImage (Object): inner image to be displayed in the slider.
                    The current implementation supports only the option of visualising a gradient with the shape of a triangle when the slider is horizontal.
                    The object attributes are:<br>
     *             - gradient: valid HTML5 gradient image (default: linear-gradient(90deg, black, black 30%, steelblue))<br>
     *             - shape (String): one of "none", "triangle" (default: none)<br>
     *           </li>
     *          <li>labelFormat (function): label formatter, e.g., function (value) { return value + "%"; } (default function displays the value of the slider)</li>
     *          <li>max (Number): maximum value selectable with the slider
     *          <li>min (Number): minimum value selectable with the slider
     *          <li>orientation (String): either "vertical" or "horizontal" (default is "vertical")</li>
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     *          <li>style (String): predefined set of visual styles: "android", "level-indicator" </li>
     *          <li>tooltip (Object): visual aspect of the tooltip. The object attributes are:<br>
     *             - position (String): either "top", "left", "bottom", "right", or "inner" (default is "left" when orientation is "vertical", and "top" when orientation is "horizontal")<br>
     *             - fontSize (String): font size for the tooltip text (default: 0.2 * handleWidth)<br>
     *             - fontColor (String): text color for the tooltip arrow (default: white)<br>
     *             - backgroundColor (String): backgroundColor color for the tooltip arrow (default: black)<br>
     *             - arrowColor (String): color of the tooltip arrow (default: black)<br>
     *           </li>
     *          <li>track (Object): visual aspect of the slider track. The object attributes are:<br>
     *             - color (String): color for slider track (default: black)<br>
     *             - height (Number): height of the slider track<br>
     *             - width (Number): width of the slider track<br>
     *           </li>
     *          <li>zIndex (Number): z-index of the slider widget<br>
     * @memberof module:SliderWidget
     * @instance
     */
    function SliderWidget(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
        coords.width = coords.width || 104;
        coords.height = coords.height || 250;
        this.id = property.call(this, id);
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.orientation = opt.orientation || "vertical";
        if (this.orientation === "horizontal") {
            var tmp = coords.height;
            coords.height = coords.width;
            coords.width = tmp;

            opt.tooltip = opt.tooltip || {};
            opt.tooltip.position = (opt.tooltip.position === "left") ? "top" : "bottom";
        }
        this.width = coords.width;
        this.height = coords.height;
        this.borderWidth = opt.borderWidth = opt.borderWidth || 1;

        load_style(this, opt);
        console.log(opt);

        this.backgroundColor = opt.backgroundColor || "transparent";
        this.fontColor = opt.fontColor || "white";
        this.borderStyle = opt.borderStyle || "none";
        this.borderColor = opt.borderColor || "inherit";
        this.cursor = opt.cursor || "default";
        if (opt.inverted) {
            var tmp_cl = this.backgroundColor;
            this.backgroundColor = this.fontColor;
            this.fontColor = tmp_cl;
        }
        var elemClass = id + " sliderWidget" + " noselect ";
        opt.position = opt.position || "absolute";
        opt.borderRadius = opt.borderRadius || "2px";
        opt.opacity = opt.opacity || 1;
        this.format = opt.format;
        this.max = opt.max || 100;
        this.min = opt.min || 0;
        this.init = opt.init || this.min;
        this.ticks = opt.ticks || [this.min, this.max];
        opt.zIndex = opt.zIndex || 0;
        this.div = d3.select(this.parent)
                        .append("div").style("position", opt.position)
                        .style("top", this.top + "px").style("left", this.left + "px")
                        .style("width", this.width + "px").style("height", this.height + "px")
                        .style("margin", 0).style("padding", 0).style("border-radius", opt.borderRadius).style("opacity", opt.opacity)
                        .style("background-color", this.backgroundColor)
                        .style("border-width", this.borderWidth + "px")
                        .style("border-style", this.borderStyle)
                        .style("border-color", this.borderColor)
                        .style("padding-left", (opt.zero_padding) ? "0px" : (this.paddingLeft) + "px")
                        .style("padding-top", (opt.zero_padding) ? "0px" : (this.paddingTop) + "px")
                        .style("z-index", opt.zIndex)
                        .style("border", "solid").style("border-color", this.borderColor).style("border-width", "1px")
                        .attr("id", id).attr("class", elemClass);

        opt.innerImage = opt.innerImage || {};
        opt.innerImage.shape = opt.innerImage.type || "none";
        opt.innerImage.gradient = opt.innerImage.gradient || "linear-gradient(90deg, black, black 30%, steelblue)";
        this.div.append("div").attr("id", id + "volumeTriangle")
                        .html(Handlebars.compile(triangle_template, { noEscape: true })({
                                gradient: opt.innerImage.gradient,
                                triangle: opt.innerImage.shape === "triangle" && opt.orientation === "horizontal",
                                transform: "scale(" + this.width / 100 + ", " + this.height / 48 + ")"

                            }));
        this.div.append("input").attr("id", id + "_slider_data")
                        .attr("type", "text");

        this.labelFormat = opt.labelFormat || function(value) {
            return value;
        };
        this.slider = new Slider("#" + id + "_slider_data", {
                        reversed: (this.orientation !== "horizontal"),
                        orientation: this.orientation,
                        tooltip_position: opt.tooltip.position,
                        tooltip: "always",
                        max: this.max,
                        min: this.min,
                        step: 1,
                        ticks_snap_bounds: 10,
                        handle: opt.handle.type || "bar",
                        ticks: this.ticks,
                        ticks_labels: this.ticks,
                        value: this.init,
                        enabled: !opt.readonly,
                        formatter: this.labelFormat
                    });
        if (opt.tooltip.position === "inner") {
            d3.select("#" + id).select(".tooltip").style("margin-top", "-6px");
            opt.tooltip.backgroundColor = opt.tooltip.backgroundColor || "transparent";
            opt.tooltip.arrowColor = opt.tooltip.arrowColor || "transparent";
        }
        if (opt.readonly) {
            d3.select("#" + id).select(".slider-track").style("cursor", "default");
        }
        this.button = new Button(id + "_button", {
            top: 0,
            left: 0,
            width: 0,
            height: 0
        }, {
            callback: opt.callback,
            customFunctionText: opt.customFunctionText || ("slide_" + id)
        });
        if (opt.customFunctionText) {
            this.customFunctionText = property.call(this, opt.customFunctionText);
        }
        opt.functionText = opt.functionText || id;
        this.functionText = property.call(this, opt.functionText);
        var _this = this;
        this.slider.on("slide", function (val) {
            _this.slide(val);
        });

        this.div.selectAll(".slider-track")
                .style("width", opt.track.width + "px")
                .style("height", opt.track.height + "px")
                .style("left", "0%");

        if (this.orientation === "horizontal") {
            this.div.select(".slider-horizontal").style("width", opt.track.width + "px").style("margin-left", opt.handle.width / 6 + "px");
            this.div.selectAll(".slider-track-high").style("opacity", "0");
            this.div.selectAll(".slider-selection").style("background-image", "linear-gradient(0deg, " + opt.track.color + ", " + opt.track.color + ")");
            this.div.selectAll(".slider-track").style("background-image", "linear-gradient(0deg, transparent, transparent)");
        } else {
            this.div.select(".slider-vertical").style("height", opt.track.height + "px");
            this.div.selectAll(".slider-track-high").style("background-color", opt.track.color);
            this.div.selectAll(".slider-selection").style("opacity", "0");
        }

        if (opt.tooltip.position === "inner") {
            this.div.selectAll(".slider-track").style("display", "none");
            this.div.selectAll(".tooltip").style("display", "none");
        }
        if (opt.handle.type === "hidden") {
            this.div.select(".slider-handle").style("display", "none");
        }

        this.div.select(".slider-handle")
                    .style("text-align", "center")
                    .style("font-size", opt.handle.fontSize + "pt")
                    .style("background-color", opt.handle.backgroundColor)
                    .style("background-image", "linear-gradient(0deg, " + opt.handle.backgroundColor + ", " + opt.handle.backgroundColor + ")")
                    .style("border-width", opt.handle.borderWidth + "px")
                    .style("border-color", opt.handle.borderColor)
                    .style("border-style", "solid")
                    .style("opacity", opt.handle.opacity)
                    .style("color", opt.handle.fontColor)
                    .style("z-index", opt.handle.zIndex)
                    .style("padding-top", (opt.handle.height - opt.handle.fontSize) / 3 + "px")
                    .style("width", opt.handle.width + "px")
                    .style("height", opt.handle.height - (this.borderWidth * 2) + "px")
                    .style("margin-left", opt.handle.left + "px")
                    .style("margin-top", opt.handle.top + "px");

        if (opt.tooltip.color) { this.div.select(".tooltip-inner").style("color", opt.tooltip.color); }
        if (opt.tooltip.backgroundColor) { this.div.select(".tooltip-inner").style("background-color", opt.tooltip.backgroundColor); }
        if (opt.tooltip.fontSize) { this.div.select(".tooltip-inner").style("font-size", opt.tooltip.fontSize + "pt"); }
        if (opt.tooltip.arrowColor) {
            if (opt.tooltip.position === "left") {
                this.div.select(".tooltip-arrow")
                            .style("border-left-color", opt.tooltip.arrowColor)
                            .style("border-top-color", "transparent");
            } else if (opt.tooltip.position === "right") {
                this.div.select(".tooltip-arrow")
                            .style("border-right-color", opt.tooltip.arrowColor)
                            .style("border-top-color", "transparent");
            } else if (opt.tooltip.position === "top") {
                this.div.select(".tooltip-arrow")
                            .style("border-left-color", "transparent")
                            .style("border-top-color", opt.tooltip.arrowColor);
            } else {
                this.div.select(".tooltip-arrow")
                            .style("border-right-color", "transparent")
                            .style("border-top-color", opt.tooltip.arrowColor);
            }
        }

        this.div.selectAll(".slider-tick-label-container").style("display", "none");

        this.slider.relayout();
        // initially, the tooltip is not placed correctly by relayout, the following line patched the problem
        this.div.select(".slider .tooltip")
                    .style("margin-top", opt.tooltip.top + "px")
                    .style("margin-left", opt.tooltip.left + "px");
        if (opt.tooltip.transform) {
            this.div.selectAll(".tooltip").style("transform", opt.tooltip.transform);
        }

        this.div.select(".slider").style("opacity", 0);
        this.hide();

        Widget.call(this, id, "sliderwidget");
        return this;
    }
    SliderWidget.prototype = Object.create(Widget.prototype);
    SliderWidget.prototype.constructor = SliderWidget;
    SliderWidget.prototype.parentClass = Widget.prototype;

    function load_style(_this, opt) {
        if (opt) {
            opt.track = opt.track || {};
            opt.track.height = opt.track.height || _this.height * 0.8;
            opt.handle = opt.handle || {};
            opt.tooltip = opt.tooltip || {};
            if (opt.style === "android") {
                opt.handle.width = 20;
                opt.handle.height = 20;
                opt.handle.left = (opt.orientation === "horizontal") ? -(opt.handle.width / 2) : -(opt.handle.width / 4);
                opt.handle.top = (opt.orientation === "horizontal") ? -(_this.height - opt.track.height - opt.handle.height / 2) : -(opt.handle.height / 4);
                opt.handle.type = "round";
                opt.track.height = (opt.orientation === "horizontal") ? 10 : _this.height - opt.handle.height * 2;
                opt.track.width = (opt.orientation === "horizontal") ? (_this.width - opt.handle.width * 2) : 10;
                opt.track.color = opt.track.color || "steelblue";

                _this.paddingLeft = (opt.orientation === "horizontal") ?
                                        (_this.height - opt.track.height) / 2
                                        : (_this.width - opt.track.width) / 2 + opt.borderWidth;
                _this.paddingTop = (opt.orientation === "horizontal") ?
                                    opt.track.height
                                    : (_this.height - opt.track.height) / 2;
                opt.tooltip.top = (opt.orientation === "horizontal") ? -(_this.height - opt.track.height) : -(_this.height - opt.track.height) / 4;
                opt.tooltip.left = (opt.orientation === "horizontal") ? -(_this.width - opt.track.width) / 2 + opt.borderWidth : 0;
                if (_this.orientation !== "horizontal") {
                    opt.tooltip.transform = "translate(-" + opt.handle.height / 4 + "px, " + opt.handle.height / 8 + "px)";
                }
                opt.tooltip.position = opt.tooltip.position || (_this.orientation === "horizontal") ? "top" : "left";
            } else if (opt.style === "level-indicator") {
                opt.handle.type = "hidden";
                opt.handle.width = 20;
                opt.handle.height = 20;
                opt.track.height = (opt.orientation === "horizontal") ? 10 : _this.height - opt.handle.height * 2;
                opt.track.width = (opt.orientation === "horizontal") ? (_this.width - opt.handle.width * 2) : 10;
                opt.zero_padding = true;
                opt.backgroundColor = "transparent";
                opt.borderColor = "transparent";
                opt.tooltip.backgroundColor = "transparent";
                opt.tooltip.arrowColor = opt.tooltip.arrowColor || "white";
                opt.track.color = opt.track.color || "#3ac441"; // bright green
                opt.readonly = true;
            } else {
                // default style
                opt.handle.borderWidth = opt.handle.borderWidth || 2;
                opt.handle.borderColor = opt.handle.borderColor || "#149bdf"; // blue-ish
                opt.handle.width = (opt.orientation === "horizontal") ? _this.height * 1.2 : _this.width - opt.handle.borderWidth;
                opt.handle.height = (opt.orientation === "horizontal") ? _this.height : _this.width * 1.2;
                opt.handle.left = (opt.orientation === "horizontal") ?
                                                -(opt.handle.width / 2)
                                                : -(opt.handle.width / 4) - opt.handle.borderWidth;
                opt.handle.top = (opt.orientation === "horizontal") ? -(opt.handle.height / 4 - opt.borderWidth) : -(opt.handle.height / 4);
                opt.handle.type = "bar";
                opt.handle.backgroundColor = opt.handle.backgroundColor || "#149bdf"; // blue-ish
                opt.handle.fontSize = opt.handle.fontSize || opt.handle.width * 0.2;
                opt.handle.fontColor = opt.handle.fontColor || "white";
                opt.handle.opacity = opt.handle.opacity || 0.9;
                opt.track.height = (opt.orientation === "horizontal") ? 10 : _this.height - opt.handle.height;
                opt.track.width = (opt.orientation === "horizontal") ? (_this.width - opt.handle.width) : 10;
                opt.track.color = "transparent";

                _this.paddingLeft = (opt.orientation === "horizontal") ?
                                        (_this.height - opt.track.height) / 2 - opt.borderWidth * 2
                                        : opt.handle.width / 4 + opt.borderWidth * 2;
                _this.paddingTop = (_this.height - opt.track.height) / 4;
                opt.tooltip.position = "inner";
                _this.labelledHandle = true;
            }
            opt.handle.zIndex = opt.handle.zIndex || 1;
        }
    }

    /**
     * @function <a name="toJSON">toJSON</a>
     * @description Returns a serialised version of the widget in JSON format.
     *              This is useful for saving/loading a specific instance of the widget.
     *              In the current implementation, the following attributes are included in the JSON object:
     *              <li> type (string): widget type, i.e., "sliderwidget" in this case
     *              <li> id (string): the unique identifier of the widget instance
     *              <li> backgroundColor (string): the background color of the button
     *              <li> orientation (string): either horizontal or vertical
     * @returns JSON object
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            backgroundColor: this.backgroundColor,
            orientation: this.orientation
        };
    };
    /**
     * @function <a name="slide">slide</a>
     * @description Programmatically slides the slider's bar to the value passed as parameter.
     * @param val {Number} Value to be rendered with the slider
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.slide = function (val) {
        if (!isNaN(parseFloat(val))) {
            this.slider.setValue(val);
            this.button.click({ functionText: this.functionText() + "(" + val + ")" });
            if (this.labelledHandle) {
                this.div.select(".slider-handle").text(this.labelFormat(val));
            }
        } else if (this.labelledHandle) {
            this.div.select(".slider-handle").text(this.labelFormat(this.getValue()));
        }
        return this.reveal();
    };
    /**
     * @function <a name="render">render</a>
     * @description Renderes the widget.
     * @param val {Number} Value to be rendered with the slider
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.render = function (val) {
        val = (this.labelledHandle && !val) ? this.getValue() : val;
        this.slide(val);
        return this;
    };
    /**
     * @function <a name="hide">hide</a>
     * @description Hides the widget
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.hide = function () {
        return this.div.style("display", "none");
    };
    /**
     * @function <a name="reveal">reveal</a>
     * @description Makes the widget visible
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.reveal = function () {
        this.div.select(".slider").style("opacity", "1");
        this.div.style("display", "block");
        return this;
    };
    /**
     * @function <a name="getValue">getValue</a>
     * @description Returns the current value of the slider (i.e., the current position of the slider's bar)
     * @returns Real number representing the current value of the slider
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.getValue = function () {
        return this.slider._state.value[0];
    };


    /**
     * @function <a name="move">move</a>
     * @description Changes the position of the widget according to the coordinates given as parameter.
     * @param data {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
     * @memberof module:SliderWidget
     * @instance
     */
    SliderWidget.prototype.move = function (data) {
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
     * Removes the widget's div
     */
    SliderWidget.prototype.remove = function () {
        SliderWidget.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    SliderWidget.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            SliderWidget.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
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
    SliderWidget.prototype.updateStyle = function (data) {
        data = data || {};
        this.fontsize = data.fontsize || this.fontsize;
        this.font = [this.fontsize, "px ", this.fontfamily];
        this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
        this.fontColor = data.fontColor || this.fontColor;
        this.backgroundColor = data.backgroundColor || this.backgroundColor;
        return this;
    };

    module.exports = SliderWidget;
});
