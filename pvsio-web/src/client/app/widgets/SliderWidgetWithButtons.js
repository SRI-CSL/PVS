/**
 * @module SliderWidgetWithButtons
 * @version 1.0
 * @description Renders a slider adorned with side buttons + / -
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 *              Uses http://seiyria.com/bootstrap-slider/
 * @author Paolo Masci
 * @date Sep 15, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses SliderWidgetWithButtons
 // The following require.config assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demos/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib"
     }
 });
 require(["widgets/SliderWidgetWithButtons"], function (SliderWidgetWithButtons) {
     "use strict";
     var device = {};
     device.sliderS1 = new SliderWidgetWithButtons("sliderS1", {
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
/*global define */
define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3");
    var Widget = require("widgets/Widget"),
        SliderWidget = require("widgets/SliderWidget"),
        property = require("util/property");

        /**
         * @function <a name="SliderWidgetWithButtons">SliderWidgetWithButtons</a>
         * @description Constructor.
         * @param id {String} The ID of the display.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) display.
         *        Default is { top: 0, left: 0, width: 104, height: 250 }.
         * @param opt {Object} Options:
         *          <li>customFunctionText (String): custom PVS function to be associated with slider actions. This function overrides the default association ("slide_" + id).</li>
         *          <li>backgroundColor (String): background display color (default is transparent)</li>
         *          <li>buttons (Object): visual aspect of the side buttons. The object attributes are:<br>
         *             - backgroundColor (String): background color of the side buttons<br>
         *             - borderColor (String): border color of the side buttons<br>
         *             - borderWidth (Number): border width of the side buttons<br>
         *             - fontColor (String): font color for the text displayed in the side buttons<br>
         *             - opacity (Number): opacity level of the side buttons<br>
         *           </li>
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
         * @memberof module:SliderWidgetWithButtons
         * @instance
         */
    function SliderWidgetWithButtons(id, coords, opt) {
        opt = opt || {};
        coords = coords || {};
        coords.width = coords.width || 104;
        coords.height = coords.height || 250;
        this.id = property.call(this, id);
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = (opt.orientation === "horizontal") ? coords.height : coords.width;
        this.height = (opt.orientation === "horizontal") ? coords.width : coords.height;
        this.max = opt.max || 100;
        this.min = opt.min || 0;
        this.buttonStep = opt.buttonsStep || Math.abs(this.max - this.min) / 10;

        opt.position = opt.position || "absolute";
        opt.borderRadius = opt.borderRadius || "2px";
        opt.borderWidth = opt.borderWidth || 1;
        opt.borderColor = opt.borderColor || "black";
        opt.opacity = opt.opacity || 1;
        opt.fontColor = opt.fontColor || "black";

        this.div = d3.select(this.parent)
                    .append("div").style("position", opt.position)
                    .style("top", this.top + "px").style("left", this.left + "px")
                    .style("width", this.width + "px").style("height", this.height + "px")
                    .style("margin", 0).style("padding", 0).style("border-radius", opt.borderRadius).style("opacity", opt.opacity)
                    .attr("id", id).attr("class", id + " SliderWidgetWithButtons" + " noselect");

        var button_size = (opt.orientation === "horizontal") ? this.height : this.width;
        opt.buttons = opt.buttons || {};
        opt.buttons.backgroundColor = opt.buttons.backgroundColor || "#149bdf"; // blue-ish
        opt.buttons.fontColor = opt.buttons.fontColor || "white";
        opt.buttons.opacity = opt.buttons.opacity || 1;
        opt.buttons.borderWidth = opt.buttons.borderWidth || 2;
        opt.buttons.borderRadius = opt.buttons.borderRadius || 2;
        opt.buttons.borderColor = opt.buttons.borderColor || "#149bdf";

        this.slider_minus = this.div.append("div").style("position", "absolute")
                    .style("top", ((opt.orientation === "horizontal") ? 0 : this.height - button_size) + "px")
                    .style("left", "0px")
                    .style("width", button_size + "px").style("height", button_size + "px")
                    .style("margin", 0).style("padding", 0)
                    .style("border", "solid " + opt.buttons.borderWidth + "px " + opt.buttons.borderColor).style("border-radius", opt.buttons.borderRadius)
                    .style("background-color", opt.buttons.backgroundColor).style("color", opt.buttons.fontColor)
                    .style("opacity", opt.buttons.opacity).style("cursor", "pointer")
                    .attr("id", id + "_minus").attr("class", "btn-default");
        this.slider_minus.append("div").text("-").style("font", (button_size * 0.6) + "px " + opt.fontColor).style("font-weight", "bold")
                .style("margin-top", button_size / 16 + "px")
                .style("margin-left", button_size / 2 + "px")
                .style("transform", "scaleX(1.8)");
        var _this = this;
        this.slider_minus.on("mousedown", function () {
            var dec_val = ((_this.slider_inner.getValue() - _this.buttonStep) > _this.min) ? (_this.slider_inner.getValue() - _this.buttonStep) : _this.min;
            _this.slider_inner.slide(dec_val);
        });

        this.slider_plus = this.div.append("div").style("position", "absolute")
                    .style("top", "0px")
                    .style("left", ((opt.orientation === "horizontal") ? this.width - button_size : 0) + "px")
                    .style("width", button_size + "px").style("height", button_size + "px")
                    .style("margin", 0).style("padding", 0)
                    .style("border", "solid " + opt.buttons.borderWidth + "px " + opt.buttons.borderColor).style("border-radius", opt.buttons.borderRadius)
                    .style("background-color", opt.buttons.backgroundColor).style("color", opt.buttons.fontColor)
                    .style("opacity", opt.buttons.opacity).style("cursor", "pointer")
                    .attr("id", id + "_plus").attr("class", "btn-default");
        this.slider_plus.append("div").text("+").style("font", (button_size * 0.6) + "px " + opt.fontColor).style("font-weight", "bolder")
                .style("margin-top", button_size / 8 + "px")
                .style("margin-left", button_size / 3 + "px")
                .style("transform", "scaleX(1.2)");
        this.slider_plus.on("mousedown", function () {
            var inc_val = ((_this.slider_inner.getValue() + _this.buttonStep) < _this.max) ? (_this.slider_inner.getValue() + _this.buttonStep) : _this.max;
            _this.slider_inner.slide(inc_val);
        });

        opt.parent = id;
        opt.customFunctionText = "slide_" + id;
        this.slider_inner = new SliderWidget(id + "_inner", {
            top: (opt.orientation === "horizontal") ? 0 : button_size,
            left: (opt.orientation === "horizontal") ? button_size : 0,
            width: coords.width,
            height: coords.height - (button_size * 2)
        }, opt);

        this.hide();
        Widget.call(this, id, "SliderWidgetWithButtons");
        return this;
    }
    SliderWidgetWithButtons.prototype = Object.create(Widget.prototype);
    SliderWidgetWithButtons.prototype.constructor = SliderWidgetWithButtons;
    SliderWidgetWithButtons.prototype.parentClass = Widget.prototype;
    /**
     * @function <a name="slide">slide</a>
     * @description Programmatically slides the slider's bar to the value passed as parameter.
     * @param val {Number} Value to be rendered with the slider
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.slide = function (val) {
        return this.slider_inner.slide(val);
    };
    /**
     * @function <a name="render">render</a>
     * @description Renderes the widget.
     * @param val {Number} Value to be rendered with the slider
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.render = function (val) {
        this.slider_inner.render(val);
        return this.reveal();
    };
    /**
     * @function <a name="hide">hide</a>
     * @description Hides the widget
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.hide = function () {
        return this.div.style("display", "none");
    };

    /**
     * @function <a name="reveal">reveal</a>
     * @description Makes the widget visible
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.reveal = function () {
        this.div.select(".slider").style("opacity", "1");
        this.div.style("display", "block");
        return this;
    };
    /**
     * @function <a name="getValue">getValue</a>
     * @description Returns the current value of the slider (i.e., the current position of the slider's bar)
     * @returns Real number representing the current value of the slider
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.getValue = function () {
        return this.slider_inner.getValue();
    };

    /**
     * @function <a name="move">move</a>
     * @description Changes the position of the widget according to the coordinates given as parameter.
     * @param data {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.move = function (data) {
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
     * @function <a name="toJSON">toJSON</a>
     * @description Returns a serialised version of the widget in JSON format.
     *              This is useful for saving/loading a specific instance of the widget.
     *              In the current implementation, the following attributes are included in the JSON object:
     *              <li> type (string): widget type, i.e., "SliderWidgetWithButtons" in this case
     *              <li> id (string): the unique identifier of the widget instance
     *              <li> backgroundColor (string): the background color of the button
     *              <li> orientation (string): either horizontal or vertical
     * @returns JSON object
     * @memberof module:SliderWidgetWithButtons
     * @instance
     */
    SliderWidgetWithButtons.prototype.toJSON = function () {
        return {
            type: this.type(),
            id: this.id(),
            backgroundColor: this.backgroundColor,
            orientation: this.orientation
        };
    };
    /**
    * Updates the location and size of the widget according to the given position and size
     */
    SliderWidgetWithButtons.prototype.updateLocationAndSize = function (pos, opt) {
        opt = opt || {};
        if (opt.imageMap) {
            SliderWidgetWithButtons.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
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
    SliderWidgetWithButtons.prototype.updateStyle = function (data) {
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
    SliderWidgetWithButtons.prototype.remove = function () {
        SliderWidgetWithButtons.prototype.parentClass.remove.apply(this);
        d3.select("div." + this.id()).remove();
    };

    module.exports = SliderWidgetWithButtons;
});
