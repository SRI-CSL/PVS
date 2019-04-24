/**
 * @module WidgetEVO
 * @version 1.0
 * @description Base class for EVO widgets.
 * @author Paolo Masci
 * @date Dec 11, 2017
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    const d3 = require("d3/d3");
    const MAX_COORDINATES_ACCURACY = 0; // max 0 decimal digits for coordinates, i.e., accuracy is 1px
    const StateParser = require("util/PVSioStateParser");
    const widget_template = require("text!widgets/templates/generic_widget_template.handlebars");

    const normalised = {
        backgroundcolor: "backgroundColor",
        fontsize: "fontSize",
        fontfamily: "fontFamily",
        fontcolor: "fontColor",
        borderwidth: "borderWidth",
        borderstyle: "borderStyle",
        borderradius: "borderRadius",
        bordercolor: "borderColor",
        zindex: "zIndex"
    };
    function normalise_options(data) {
        var opt = {};
        if (data) {
            let norm_key = null;
            for (let key in data) {
                norm_key = normalised[key] || key;
                opt[norm_key] = data[key];
            }
        }
        return opt;
    }
    const html_attributes = {
        backgroundColor: "background-color",
        backgroundcolor: "background-color",
        fontSize: "font-size",
        fontsize: "font-size",
        fontFamily: "font-family",
        fontfamily: "font-family",
        fontColor: "color",
        fontcolor: "color",
        align: "text-align",
        borderWidth: "border-width",
        borderwidth: "border-width",
        borderStyle: "border-style",
        borderstyle: "border-style",
        borderRadius: "border-radius",
        borderradius: "border-radius",
        borderColor: "border-color",
        bordercolor: "border-color",
        zIndex: "z-index"
    };
    function normalise_style(data) {
        var style = {};
        if (data) {
            data = normalise_options(data);
            let html_key = null;
            for (let key in data) {
                html_key = html_attributes[key] || key;
                style[html_key] = data[key];
            }
        }
        return style;
    }

    class WidgetEVO {
        /**
         * @function <a name="ButtonEVO">ButtonEVO</a>
         * @description Constructor.
         * @param id {String} The ID of the touchscreen button.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget.
         * @param opt {Object} Style options defining the visual appearance of the widget.
         *                     Options can be given either as standard html style attributes or using the following widget attributes:
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 ))</li>
         *          <li>marginLeft (Number): left margin (default is 0)</li>
         *          <li>marginTop (Number): top margin (default is 0)</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is 0.9, i.e., semi-opaque)</li>
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>visibleWhen (String): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:WidgetEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = normalise_options(opt);
            coords = coords || {};
            this.id = id;
            this.type = opt.type || "WidgetEVO";
            this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
            this.top = coords.top || 0;
            this.left = coords.left || 0;
            this.width = (isNaN(parseFloat(coords.width))) ? 32 : coords.width;
            this.height = (isNaN(parseFloat(coords.height))) ? 32 : coords.height;
            this.visibleWhen = opt.visibleWhen || "true"; // default: always enabled/visible
            this.widget = true; // this flag can be used to identify whether an object is a widget
            this.position = opt.position || "absolute";
            this.zIndex = opt.zIndex || 0;

            // visual style
            opt.borderRadius = (opt.borderRadius) ?
                                    ((typeof opt.borderRadius === "string" && opt.borderRadius.indexOf("px") >= 0) ? opt.borderRadius : opt.borderRadius.toString() + "px")
                                    : 0;
            opt.borderStyle = (opt.borderStyle) ? opt.borderStyle : (opt.borderRadius || opt.borderWidth) ? "solid" : "none";
            opt.borderWidth = (!isNaN(parseFloat(opt.borderWidth))) ? opt.borderWidth : (opt.borderColor) ? 2 : 0;
            this.style = {};
            this.style["background-color"] = opt.backgroundColor || "transparent";
            this.style["font-size"] = (opt.fontSize || (this.height - opt.borderWidth) / 2) + "pt";
            this.style["font-family"] = opt.fontFamily || "sans-serif";
            this.style.color = opt.fontColor || "white";
            this.style["text-align"] = opt.align || "center";
            this.style["border-width"] = opt.borderWidth + "px";
            this.style["border-style"] = opt.borderStyle;
            this.style["border-radius"] = opt.borderRadius;
            this.style["border-color"] = opt.borderColor || "steelblue";
            this.style.overflow = opt.overflow || "hidden";
            this.style["margin-left"] = (isNaN(parseFloat(opt.marginLeft))) ? "0px" : parseFloat(opt.marginLeft) + "px";
            this.style["margin-top"] = (isNaN(parseFloat(opt.marginTop))) ? "0px" : parseFloat(opt.marginTop) + "px";
            this.style["white-space"] = "nowrap";
            this.style.opacity = (isNaN(parseFloat(opt.opacity))) ? 0.9 : opt.opacity;
            this.style.blinking = opt.blinking || false;
            this.style.cursor = opt.cursor || "default";
            this.style["overlay-color"] = opt.overlayColor;

            this.widget_template = opt.widget_template || widget_template;
            var res = Handlebars.compile(this.widget_template, { noEscape: true })(this);
            if (d3.select(this.parent).empty()) {
                console.error("Error: " + this.parent + " does not exist. Widget '" + id + "' cannot be attached to DOM :((");
            }

            d3.select(this.parent).append("div").style("height", "0px").style("width", "0px").html(res);
            this.div = d3.select("#" + this.id);
            this.base = d3.select("#" + this.id + "_base");
            this.overlay = d3.select("#" + this.id + "_overlay");
            this.setStyle(this.style);

            this.hide();
            return this;
        }

        /**
         * @function <a name="render">render</a>
         * @description Basic rendering function (reveals the widget). Widgets need to override this function when rendering involves additional/different logic.
         * @memberof module:WidgetEVO
         * @instance
         */
        render (res) {
            return this.reveal();
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         *              The predefined behaviour is rendering the widget type.
         * @memberof module:WidgetEVO
         * @instance
         */
        renderSample () {
            return this.render(this.type);
        }

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        reveal () {
            if (this.div && !this.div.empty()) {
                // console.log("revealing widget " + this.id);
                this.div.style("display", "block");
            }
            return this;
        }

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        hide () {
            if (this.div && !this.div.empty()) {
                // console.log("hiding widget " + this.id);
                this.div.style("display", "none");
            }
            return this;
        }

        /**
         * @function <a name="move">move</a>
         * @description Changes the position of the widget according to the coordinates given as parameter.
         * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
         * @param opt {Object}
         *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
         *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
         * @memberof module:WidgetEVO
         * @instance
         */
        move (coords, opt) {
            // console.log(coords);
            if (this.div && !this.div.empty()) {
                coords = coords || {};
                opt = normalise_options(opt);
                opt.duration = opt.duration || 0;
                opt.transitionTimingFunction = opt.transitionTimingFunction || "ease-out";
                this.top = (isNaN(parseFloat(coords.top))) ? this.top : parseFloat(parseFloat(coords.top).toFixed(MAX_COORDINATES_ACCURACY));
                this.left = (isNaN(parseFloat(coords.left))) ? this.left : parseFloat(parseFloat(coords.left).toFixed(MAX_COORDINATES_ACCURACY));
                this.div.transition().duration(opt.duration).style("top", this.top + "px").style("left", this.left + "px")
                            .style("transition-timing-function", opt.transitionTimingFunction);
            }
            return this.reveal();
        }

        /**
         * @function <a name="resize">resize</a>
         * @description Changes the size of the widget according to the width and height given as parameter.
         * @param coords {Object} Width and height indicating the new size of the widget. The coordinates are given in the form { width: (number), height: (number) }
         * @param opt {Object}
         *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
         *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
         * @memberof module:WidgetEVO
         * @instance
         */
        resize (size, opt) {
            // console.log(coords);
            if (this.div && !this.div.empty()) {
                size = size || {};
                opt = normalise_options(opt);
                opt.duration = opt.duration || 0;
                opt.transitionTimingFunction = opt.transitionTimingFunction || "ease-out";
                this.height = (isNaN(parseFloat(size.height))) ? this.height : parseFloat(parseFloat(size.height).toFixed(MAX_COORDINATES_ACCURACY));
                this.width = (isNaN(parseFloat(size.width))) ? this.width : parseFloat(parseFloat(size.width).toFixed(MAX_COORDINATES_ACCURACY));
                if (opt.duration) {
                    this.div.transition().duration(opt.duration).style("height", this.height + "px").style("width", this.width + "px")
                                .style("transition-timing-function", opt.transitionTimingFunction);
                    this.base.transition().duration(opt.duration).style("line-height", this.height + "px").style("height", this.height + "px").style("width", this.width + "px")
                                .style("transition-timing-function", opt.transitionTimingFunction);
                    this.overlay.transition().duration(opt.duration).style("height", this.height + "px").style("width", this.width + "px")
                                .style("transition-timing-function", opt.transitionTimingFunction);
                } else {
                    this.div.style("height", this.height + "px").style("width", this.width + "px");
                    this.base.style("line-height", this.height + "px").style("height", this.height + "px").style("width", this.width + "px");
                    this.overlay.style("height", this.height + "px").style("width", this.width + "px");
                }
            }
            return this.reveal();
        }

        /**
         * @function <a name="rotate">rotate</a>
         * @description Rotates the widget of the degree given as parameter.
         * @param deg {Number | String} Degrees by which the widget will be rotated. Positive degrees are for clock-wise rotations, negative degrees are for counter-clock-wise rotations.
         * @param opt {Object}
         *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
         *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-in") </li>
         *         <li> transformOrigin (String): rotation pivot, e.g., "top", "bottom", "center" (default is "center") </li>
         * @memberof module:WidgetEVO
         * @instance
         */
        rotate (deg, opt) {
            if (this.div && !this.div.empty()) {
                deg = (isNaN(parseFloat(deg))) ? 0 : parseFloat(deg);
                opt = normalise_options(opt);
                opt.duration = opt.duration || 0;
                opt.transitionTimingFunction = opt.transitionTimingFunction || "ease-in";
                opt.transformOrigin = opt.transformOrigin || "center";
                this.div.transition().duration(opt.duration)
                            .style("transform", "rotate(" + deg + "deg)")
                            .style("transform-origin", opt.transformOrigin)
                            .style("transition-timing-function", opt.transitionTimingFunction);
            }
            return this.reveal();
        }

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:WidgetEVO
         * @instance
         */
        remove () {
            if (this.div && !this.div.empty()) {
                this.div.remove();
            }
            return this;
        }

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:WidgetEVO
         * @instance
         */
        evalViz (state) {
            var vizAttribute = true;
            if (state && typeof state === "object") {
                vizAttribute = false;
                var expr = StateParser.simpleExpressionParser(this.visibleWhen);
                if (expr && expr.res) {
                    if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                        vizAttribute = true;
                    } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                        var str = StateParser.resolve(state, expr.res.attr);
                        if (str) {
                            str = StateParser.evaluate(str);
                            if ((expr.res.binop === "=" && str === expr.res.constant) ||
                                (expr.res.binop === "!=" && str !== expr.res.constant)) {
                                    vizAttribute = true;
                            }
                        }
                    }
                }
            }
            return vizAttribute;
        }

        /**
         * @function <a name="evaluate">evaluate</a>
         * @description Returns the state of the widget.
         * @param attr {String} Name of the state attribute associated with the widget.
         * @param state {Object} Current system state, represented as a JSON object.
         * @return {String} String representation of the state of the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        evaluate (attr, state) {
            if (attr && state && typeof state === "object") {
                var disp = StateParser.resolve(state, attr);
                if (disp !== null && disp !== undefined) {
                    return StateParser.evaluate(disp).replace(new RegExp("\"", "g"), "");
                } else {
                    console.log("Warning: WidgetEVO.evaluate could not find state attribute " + attr + " requested by " + this.id);
                }
            }
            return "";
        }


        /**
         * @function <a name="getVizExpression">getVizExpression</a>
         * @description Returns the expression defining the visibility of the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        getVizExpression () {
            return this.visibleWhen;
        }


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
         *          <li>fontSize (String): font size (default is "VALpx", where VAL = (coords.height - opt.borderWidth) / 2)</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is 0.9, i.e., semi-opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:WidgetEVO
         * @instance
         */
        setStyle (style) {
            style = normalise_style(style);
            for(var key in style) {
                this.base.style(key, style[key]);
            }
            if (style.blinking) { this.base.classed("blinking", true); }
            return this;
        }

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:WidgetEVO
         * @instance
         */
        invertColors () {
            this.base.style("background-color", this.style["font-color"]);
            this.base.style("color", this.style["background-color"]);
            return this;
        }

        /**
         * @function <a name="select">select</a>
         * @description Selects the widget -- useful to highlight the widget programmaticaly.
         * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        select (opt) {
            opt = normalise_options(opt);
            opt.opacity = (isNaN(parseFloat(opt.opacity))) ? 0.5 : opt.opacity;
            this.setStyle(opt);
            if (opt.overlayColor) { this.overlay.style("background-color", opt.overlayColor); }
            if (opt.classed) { this.base.classed(opt.classed, true); }
            return this;
        }

        /**
         * @function <a name="deselect">deselect</a>
         * @description Deselects the widget.
         * @memberof module:WidgetEVO
         * @instance
         */
        deselect () {
            this.setStyle(this.style);
            return this;
        }

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:WidgetEVO
         * @instance
         */
        getPosition () {
            return { left: this.left, top: this.top };
        }

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:WidgetEVO
         * @instance
         */
        getSize () {
            return { width: this.width, height: this.height };
        }

        /**
         * @function <a name="setPosition">setPosition</a>
         * @description Sets the position of the widget, equivalent to function move(...)
         * @param coords {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:WidgetEVO
         * @instance
         */
        setPosition (coords) {
            return this.move(coords);
        }

        /**
         * @function <a name="setSize">setSize</a>
         * @description Set the size of the widget, equivalent to function resize(...)
         * @param size {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:WidgetEVO
         * @instance
         */
        setSize (size) {
            return this.resize(size);
        }

        /**
         * @function <a name="setPositionAndSize">setPositionAndSize</a>
         * @description Sets the position & size of the widget
         * @param data {Object} Coordinates and size of the widget, in the form { left: x, top: y, width: w, height: h }, where x, y, w, h are real numbers
         * @memberof module:WidgetEVO
         * @instance
         */
        setPositionAndSize (data) {
            if (data) {
                this.move(data);
                this.resize(data);
            }
            return this;
        }


        /**
         * @function <a name="normaliseOptions">normaliseOptions</a>
         * @description Utility function for normalising options names (e.g., fontsize -> fontSize)
         * @param opt {Object} Widget options
         * @return {Object} Normalised options or {} if function argument is null
         * @memberof module:WidgetEVO
         * @instance
         */
        normaliseOptions (opt) {
            return normalise_options(opt);
        }

        /**
         * @function <a name="toHtmlAttributes">toHtmlAttributes</a>
         * @description Utility function for translating widget style attributes into standard html5 style attributes (e.g., fontSize -> font-size)
         * @param opt {Object} Style attributes (e.g., { fontSize: 10 })
         * @return {Object} HTML5 style attributes or {} if funciton argument is null
         * @memberof module:WidgetEVO
         * @instance
         */
        toHtmlAttributes (opt) {
            return normalise_style(opt);
        }

        getType () {
            return this.type;
        }

        getStyle () {
            let ans = {};
            let _this = this;
            // remove units of numeric values, e.g., font-size is returned as 13 (rather than "13pt")
            Object.keys(this.style).forEach(function (key) {
                ans[key] = isNaN(parseFloat(_this.style[key])) ? _this.style[key] : parseFloat(_this.style[key]);
            });
            return ans;
        }

        getStyle2 () {
            let ans = {};
            let _this = this;
            // remove units of numeric values, e.g., font-size is returned as 13 (rather than "13pt")
            Object.keys(this.style).forEach(function (key) {
                let isNumeric = !isNaN(parseFloat(_this.style[key]));
                ans[key] = {
                    val: (isNumeric) ? parseFloat(_this.style[key]) : _this.style[key],
                    isNumeric: isNumeric
                };
            });
            return ans;
        }

        getCoordinates () {
            return {
                top: parseFloat(this.top).toFixed(MAX_COORDINATES_ACCURACY),
                left: parseFloat(this.left).toFixed(MAX_COORDINATES_ACCURACY),
                width: parseFloat(this.width).toFixed(MAX_COORDINATES_ACCURACY),
                height: parseFloat(this.height).toFixed(MAX_COORDINATES_ACCURACY)
            };
        }

        getKeys () {
            let _this = this;
            let ans = {};
            this.constructor.widgetKeys.forEach(function (key) {
                ans[key] = _this[key];
            });
            return ans;
        }
        
        getPrimaryKey () {
            return this.id;
        }

        getEvents () {
            return this.evts;
        }

        toJSON (opt) {
            return {
                id: this.id,
                type: this.constructor.alias || this.getType(),
                key: this.getPrimaryKey(),
                coords: this.getCoordinates(),
                style: this.getStyle()
            };
        }
    }

    WidgetEVO.widgetKeys = [];
    module.exports = WidgetEVO;
});
