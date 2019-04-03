/**
 * @module TracingsDisplayEVO
 * @version 1.0
 * @description Renders historical data using tracings-like visualisation.
 *              This module provide APIs for changing the look and feel of the tracings,
 *              including: color, size, and tracings speed.
 * @author Paolo Masci
 * @date May 10, 2015
 *
 * @example <caption>Typical use of TracingsDisplayEVO APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses TracingsDisplayEVO.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.tracings = new TracingsDisplayEVO("tracings",
 *                         { top: 56, left: 164, height: 30, width: 100 },
 *                         { parent: "prototype", align: "left", speed: 0.4 });
 *     device.tracings.render(10); // the display renders 10
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext:true */
/*global define */

define(function (require, exports, module) {
    "use strict";
    var WidgetEVO = require("widgets/core/WidgetEVO");

    function start_animation (_this, opt) {
        function clearContext(context) {
            context.save();
            context.fillStyle = _this.backgroundColor;
            context.fillRect(0, 0, _this.width, _this.height);
            context.restore();
        }
        function render_aux(context) {
            return function () {
                _this.px += _this.speed;
                context.clearRect(_this.opx, 0, _this.scanBar.width, _this.height);
                context.fillStyle = _this.scanBar.color;
                context.fillRect(_this.px, 0, _this.scanBar.width, _this.height);
                context.beginPath();
                context.moveTo(_this.opx, _this.opy);
                context.lineTo(_this.px, _this.py);
                context.stroke();
                _this.opx = _this.px;
                _this.opy = _this.py;
                if (_this.opx > _this.width) {
                    _this.px = _this.opx = -(_this.speed);
                }
            };
        }
        if (!_this.loop) {
            opt = opt || {};
            let context = _this.canvas.node().getContext("2d");
            // let align = opt.align || _this.align;
            context.textBaseline = _this.textBaseline;
            context.strokeStyle = opt.fontColor || _this.style.color;
            context.lineWidth = 1;
            context.font = [ _this.style["font-size"], "px ", _this.style["font-family"] ].join("");
            // render_aux({ val: val, context: context, align: align, height: _this.height, width: _this.width }, opt);
            render_aux(context)();
            _this.loop = setInterval(render_aux(context), _this.refreshRate);
            if (!_this.keep) {
                _this.px = _this.opx = 0;
                clearContext(context);
            }
            _this.reveal();
        }
    }

    class TracingsDisplayEVO extends WidgetEVO {

        /**
         * @function <a name="TracingsDisplayEVO">TracingsDisplayEVO</a>
         * @description Constructor.
         * @param id {String} The ID of the widget.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget area.
         *        Default is { top: 0, left: 0, width: 200, height: 80 }.
         * @param opt {Object} Style options defining the visual appearance of the widget.
         *                     Options can be given either as standard html style attributes or using the following widget attributes:
         *          <li>blinking (bool): whether the widget is blinking (default is false, i.e., does not blink)</li>
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the widget. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>range (Object): visualisation range. The object has two attributes, max and min, indicating the maximum and minimum value of the range (default: { max: 100, min: 0 })</li>
         *          <li>scanBar (Object): characteristics (width, color) of the scan bar (default: { width: 4, color: "black" })</li>
         *          <li>speed (Number): trace visualisation speed (default: 1 pixes/second)</li>
         *          <li>visibleWhen (String): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         *                  The following additional attribute links the display widget to a specific state attribute of a model:
         *          <li>displayKey (String): name of the state attribute defining the display content. Default is the ID of the widget.</li>
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};

            // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
            opt.backgroundColor = opt.backgroundColor || "black";
            opt.cursor = opt.cursor || "default";
            opt.overflow = "hidden";

            // invoke WidgetEVO constructor to create the widget
            super(id, coords, opt);

            // set widget type & display key
            this.type = this.type || "TracingsDisplayEVOEVO";
            this.displayKey = (typeof opt.displayKey === "string") ? opt.displayKey : id;

            // add widget-specific style attributes
            this.range = opt.range || { max: 100, min: 0 };
            this.speed = opt.speed || 1;
            opt.scanBar = opt.scanBar || {};
            this.scanBar = {};
            this.scanBar.color = opt.scanBar.color || "black";
            this.scanBar.width = opt.scanBar.width || 4;

            // additional state variables necessary for rendering the trace
            this.keep = opt.keepLastState || false;
            this.py = this.height * 0.5;
            this.px = 0;
            this.opx = this.px;
            this.opy = this.py;
            this.loop = null;
            this.refreshRate = opt.refreshRate || 250;

            this.canvas = this.base.append("canvas").attr("id", id + "_canvas").attr("class", id + "_canvas")
                                    .attr("width", this.width).attr("height", this.height)
                                    .style("margin", 0).style("padding", 0)
                                    .style("vertical-align", "top");

            return this;
        }

        /**
         * @function <a name="render">render</a>
         * @description Rendering function.
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @param opt {Object} Style options overriding the style attributes used when the widget was created.
         *                     The override style options are temporary, i.e., they are applied only for the present invocation of the render method.
         *                     Available options are either html style attributes or the following widget attributes:
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the widget. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
        render (val, opt) {
            // set style
            opt = this.normaliseOptions(opt);
            opt["background-color"] = opt.backgroundColor || this.style["background-color"];
            opt["font-size"] = (opt.fontSize || this.style["font-size"]) + "pt";
            opt["font-family"] = opt.fontFamily || this.style["font-family"];
            opt.color = opt.fontColor || this.style.color;
            opt["text-align"] = opt.align || this.style["text-align"];
            opt["border-width"] = (opt.borderWidth) ? opt.borderWidth + "px" : this.style["border-width"];
            opt["border-style"] = opt.borderStyle || this.style["border-style"];
            opt["border-radius"] = (isNaN(parseFloat(opt.borderRadius))) ? this.style["border-radius"] : opt.borderRadius;
            opt["border-color"] = opt.borderColor || this.style["border-color"];
            this.setStyle(opt);

            // render content
            var _this = this;
            val = (val !== null && val !== undefined && !isNaN(parseFloat(val)))? parseFloat(val) : 0;
            opt.range = opt.range || { };
            opt.range.max = opt.range.max || _this.range.max;
            opt.range.min = opt.range.min || _this.range.min;
            this.py = (val > 0) ? this.height - (val / (opt.range.max - opt.range.min)) * this.height : this.height;
            start_animation(this, opt);
            return this;
        }

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
        hide () {
            super.hide();
            if (this.loop) {
                clearInterval(this.loop);
                this.loop = null;
            }
            return this;
        }

        /**
         * @function <a name="pauseTrace">pauseTrace</a>
         * @description Pauses the running scan bar of the trace display.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
        pauseTrace () {
            if (this.loop) {
                clearInterval(this.loop);
                this.loop = null;
            }
            return this;
        }

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
        reveal () {
            super.reveal();
            start_animation(this);
            return this;
        }

        // the following methods are inherited from WidgetEVO

        /**
         * @function <a name="move">move</a>
         * @description Changes the position of the widget according to the coordinates given as parameter.
         * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
         * @param opt {Object}
         *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
         *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
        * @function <a name="rotate">rotate</a>
        * @description Rotates the widget of the degree given as parameter.
        * @param deg {Number | String} Degrees by which the widget will be rotated. Positive degrees are for clock-wise rotations, negative degrees are for counter-clock-wise rotations.
        * @param opt {Object}
        *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
        *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-in") </li>
        *         <li> transformOrigin (String): rotation pivot, e.g., "top", "bottom", "center" (default is "center") </li>
        * @memberof module:TracingsDisplayEVO
        * @instance
        */

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
         * @function <a name="evaluate">evaluate</a>
         * @description Returns the state of the widget.
         * @param attr {String} Name of the state attribute associated with the widget.
         * @param state {Object} Current system state, represented as a JSON object.
         * @return {String} String representation of the state of the widget.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
        * @function <a name="getVizExpression">getVizExpression</a>
        * @description Returns the expression defining the visibility of the widget.
        * @memberof module:TracingsDisplayEVO
        * @instance
        */

        /**
         * @function <a name="setStyle">setStyle</a>
         * @description Sets the font color and background color.
         * @param style {Object} Style attributes characterising the visual appearance of the widget.
         *                      Attributes can be either standard HTML5 attributes, or the following widgets attributes:
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is 0.9, i.e., semi-opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
         * @function <a name="select">select</a>
         * @description Selects the widget -- useful to highlight the widget programmaticaly.
         * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
        * @function <a name="deselect">deselect</a>
        * @description Deselects the widget.
        * @memberof module:TracingsDisplayEVO
        * @instance
        */

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:TracingsDisplayEVO
         * @instance
         */

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:TracingsDisplayEVO
         * @instance
         */
    }

    module.exports = TracingsDisplayEVO;
});
