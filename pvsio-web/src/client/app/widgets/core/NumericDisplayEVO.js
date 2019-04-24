/**
 * @module NumericDisplayEVO
 * @version 1.0
 * @description Renders a Numeric Display with a cursor that can be used to highlight digits.
                Digits can be either numeric or alphanumeric characters.
                The visibility of the decimal point is enhanced by making it bigger and aligned towards the middle of the line height.
                The font of integer digits is slightly bigger than that of fractional digits.
                All digits are evenly spaced, and the exact spacing can be set and controlled programmatically.
 * @author Paolo Masci
 * @date Dec 11, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses NumericDisplayEVO
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: { d3: "../lib/d3", text: "../lib/text" }
 });
 require(["widgets/core/NumericDisplayEVO"], function (NumericDisplayEVO) {
      "use strict";
      var disp = new NumericDisplayEVO("disp", {
        top: 200, left: 120, height: 24, width: 120
      }, {
        fontColor: "black",
        fontsize: 16,
        backgroundColor: "blue"
      });
     disp.render("BN-32.5"); // The display shows BN-32.5
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext:true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    let BasicDisplayEVO = require("widgets/core/BasicDisplayEVO"),
        digits_template = require("text!widgets/templates/digits_template.handlebars");

    const selectedFontSize = 1.076; // ratio selectedFont/normalFont for integer digits

    class NumericDisplayEVO extends BasicDisplayEVO {
        /**
         * @function <a name="NumericDisplayEVO">NumericDisplayEVO</a>
         * @description Constructor.
         * @param id {String} The ID of the touchscreen button.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget area.
         *        Default is { top: 0, left: 0, width: 32, height: 20 }.
         * @param opt {Object} Style options defining the visual appearance of the widget.
         *                     Options can be given either as standard html style attributes or using the following widget attributes:
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>cursorName (String): name of the state attribute defining the cursor position. Default is empty string, i.e., cursor is disabled.</li>
         *          <li>decimalPointOffset (Number): offset for the decimal point position (default is 0, i.e., the decimal point is placed at the center of the display)</li>
         *          <li>decimalFontSize (Number): decimal font size (default is opt.fontSize * 0.8)</li>
         *          <li>decimalLetterSpacing (Number): fixed letter spacing for decimal digits (default: opt.decimalFontSize * 0.8).</li>
         *          <li>displayKey (String): name of the state attribute defining the display content. Default is the ID of the widget.</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>letterSpacing (Number): fixed letter spacing (default: opt.fontSize * 0.8).</li>
         *          <li>maxIntegerDigits (Number): max digits of the whole part of the display (default is Math.floor(0.75 * coords.width / opt.letterSpacing)).</li>
         *          <li>maxDecimalDigits (Number): max digits of the fractional part of the display (default is Math.floor(0.25 * coords.width / opt.decimalLetterSpacing)).</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>visibleWhen (String): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         *                  The following additional attribute links the display widget to a specific state attribute of a model:
         * @memberof module:NumericDisplayEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};

            // override options
            opt.type = opt.type || "NumericDisplayEVO";
            opt.backgroundColor = opt.backgroundColor || "black";

            // invoke BasicDisplayEVO constructor to create the widget
            super(id, coords, opt);
            
            // set cursor name
            this.cursorName = opt.cursorName || "";

            // add widget-specific style attributes
            this.style["letter-spacing"] = opt.letterSpacing || parseFloat(this.style["font-size"]);
            this.style["decimal-font-size"] = opt.decimalFontSize || parseFloat(this.style["font-size"]) * 0.8;
            this.style["decimal-letter-spacing"] = opt.decimalLetterSpacing || parseFloat(this.style["decimal-font-size"]) * 0.8;
            this.maxDecimalDigits = (isNaN(parseInt(opt.maxDecimalDigits))) ? 2 : parseInt(opt.maxDecimalDigits);

            //  this.maxDecimalDigits = (isNaN(parseInt(opt.maxDecimalDigits))) ? Math.floor(0.25 * this.width / parseFloat(this.style["decimal-letter-spacing"])) : parseInt(opt.maxDecimalDigits);
            this.maxIntegerDigits = (isNaN(parseInt(opt.maxIntegerDigits))) ? Math.floor((this.width - this.maxDecimalDigits * parseFloat(this.style["decimal-letter-spacing"])) / parseFloat(this.style["letter-spacing"])) : parseInt(opt.maxIntegerDigits);
            this.decimalPointOffset = opt.decimalPointOffset || 0;
            return this;
        }

        /**
         * @function <a name="render">render</a>
         * @description Rendering function for button widgets.
         * @param state {Object|String} Information to be rendered
         * @param opt {Object} Style options overriding the style attributes used when the widget was created.
         *                     The override style options are temporary, i.e., they are applied only for the present invocation of the render method.
         *                     Available options are either html style attributes or the following widget attributes:
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>cursorName (String): name of the state attribute defining the cursor position. Default is empty string, i.e., cursor is disabled.</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:NumericDisplayEVO
         * @instance
         */
        render (state, opt) {
            if (state !== null && state !== undefined) {
                // set style
                opt = this.normaliseOptions(opt);
                opt["background-color"] = opt.backgroundColor || this.style["background-color"];
                opt["font-size"] = (opt.fontSize || this.style["font-size"]) + "pt";
                opt["font-family"] = opt.fontFamily || this.style["font-family"];
                opt.color = opt.fontColor || this.style.color;
                opt["text-align"] = opt.align || this.style["text-align"];
                let borderWidth = parseFloat(opt.borderWidth);
                opt["border-width"] = (!isNaN(borderWidth)) ? opt.borderWidth + "px" : this.style["border-width"];
                opt["border-style"] = (opt.borderStyle) ? opt.borderStyle : (!isNaN(borderWidth)) ? "solid" : this.style["border-style"];
                opt["border-radius"] = (!isNaN(parseFloat(opt.borderRadius))) ? opt.borderRadius : this.style["border-radius"];
                opt["border-color"] = (opt.borderColor) ? opt.borderColor : (!isNaN(borderWidth)) ? "transparent" : this.style["border-color"];
                this.setStyle(opt);

                // set content
                if (typeof state === "string" || typeof state === "number") {
                    var val = state;
                    state = {};
                    state[this.displayKey] = val;
                }
                if (typeof state === "object" && this.evalViz(state)) {
                    var disp = this.evaluate(this.displayKey, state);
                    var parts = disp.split(".");
                    var _this = this;

                    var desc = {
                        whole: [], frac: [],
                        point: (disp.indexOf(".") >= 0),
                        whole_zeropadding: [], frac_zeropadding: [],
                        max_integer_digits: this.maxIntegerDigits,
                        max_decimal_digits: this.maxDecimalDigits
                    };
                    desc.whole = parts[0].split("").map(function (d) {
                        return { val: d, selected: false, "font-size": parseFloat(_this.style["font-size"]) };
                    });
                    if (parts.length > 1) {
                        desc.frac = parts[1].split("").map(function (d) {
                            return { val: d, selected: false, "font-size": parseFloat(_this.style["decimal-font-size"]) };
                        });
                    }
                    let cursorName = opt.cursorName || this.cursorName;
                    desc.cursorPos = parseInt(this.evaluate(cursorName, state));
                    if (!isNaN(desc.cursorPos)) {
                        if (desc.cursorPos >= 0) {
                            if (desc.cursorPos < desc.whole.length) {
                                desc.whole[desc.whole.length - 1 - desc.cursorPos].selected = true;
                                desc.whole[desc.whole.length - 1 - desc.cursorPos].fontSize *= selectedFontSize;
                            } else { // introduce leading zeros
                                desc.whole_zeropadding = new Array(desc.cursorPos - (desc.whole.length - 1)).fill({
                                    val: 0, selected: false, "font-size": parseFloat(_this.style["font-size"])
                                });
                                desc.whole_zeropadding[0] = {
                                    val: 0, selected: true, "font-size": parseFloat(_this.style["font-size"]) * selectedFontSize
                                };
                            }
                        } else if (desc.cursorPos < 0) {
                            if (-(desc.cursorPos + 1) < desc.frac.length) {
                                desc.frac[-(desc.cursorPos + 1)].selected = true;
                            } else { // introduce trailing zeros and the decimal point
                                desc.frac_zeropadding = new Array(-desc.cursorPos - desc.frac.length).fill({
                                    val: 0, selected: false, "font-size": parseFloat(_this.style["decimal-font-size"])
                                });
                                desc.frac_zeropadding[desc.frac_zeropadding.length - 1] = {
                                    val: 0, selected: true, "font-size": parseFloat(_this.style["decimal-font-size"])
                                };
                                desc.point = true;
                            }
                        }
                    }
                    //  console.log(desc);
                    var point_style = {
                        left: (parseFloat(desc.max_integer_digits) * parseFloat(this.style["letter-spacing"]) + parseFloat(this.decimalPointOffset)).toFixed(2),
                        width: (parseFloat(this.style["letter-spacing"]) / 2).toFixed(2),
                        height: this.height - 2 * borderWidth,
                        "margin-left": (-parseFloat(this.style["letter-spacing"]) / 32).toFixed(2),
                        "font-size": parseFloat(this.style["decimal-font-size"]).toFixed(2),
                        viz: desc.point
                    };
                    var whole_style = {
                        digits: desc.whole_zeropadding.concat(desc.whole),
                        width: parseFloat(desc.max_integer_digits) * parseFloat(this.style["letter-spacing"]),
                        height: this.height - 2 * borderWidth,
                        left: parseFloat(point_style.left) - parseFloat(point_style.width),
                        "letter-spacing": parseFloat(this.style["letter-spacing"]).toFixed(2),
                        color: this.style.color,
                        "background-color": this.style["background-color"],
                        "padding-left": ((parseFloat(desc.max_integer_digits) - parseFloat(desc.whole.length) - parseFloat(desc.whole_zeropadding.length)) * parseFloat(this.style["letter-spacing"])).toFixed(2)
                    };
                    var frac_style = {
                        digits: desc.frac.concat(desc.frac_zeropadding),
                        width: (parseFloat(desc.max_decimal_digits) * parseFloat(this.style["decimal-letter-spacing"])).toFixed(2),
                        height: this.height - 2 * borderWidth,
                        left: (parseFloat(point_style.left) + parseFloat(point_style.width)).toFixed(2),
                        "letter-spacing": parseFloat(this.style["decimal-letter-spacing"]).toFixed(2),
                        color: this.style.color,
                        "background-color": this.style["background-color"]
                    };
                    //  console.log(frac_style);
                    frac_style.viz = (frac_style.digits.length > 0);
                    var dom = Handlebars.compile(digits_template, { noEscape: true })({
                        type: this.type,
                        whole: whole_style,
                        frac: frac_style,
                        point: point_style
                    });
                    this.base.html(dom);
                }
            }
            this.reveal();
            return this;
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         * @memberof module:NumericDisplayEVO
         * @instance
         */
        renderSample () {
            let st = {};
            st[this.displayKey] = "_12.3";
            st.demoCursor = 2;
            return this.render(st, { cursorName: "demoCursor", borderWidth: 2, fontColor: "white", backgroundColor: "black" });
        }

        // getKeys() {
        //     return {
        //         displayKey: this.displayKey,
        //         cursorName: this.cursorName
        //     };
        // }


        // the following methods are inherited from WidgetEVO

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
        * @function <a name="move">move</a>
        * @description Changes the position of the widget according to the coordinates given as parameter.
        * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
        * @param opt {Object}
        *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
        *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
        * @memberof module:NumericDisplayEVO
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
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
        * @function <a name="evaluate">evaluate</a>
        * @description Returns the state of the widget.
        * @param attr {String} Name of the state attribute associated with the widget.
        * @param state {Object} Current system state, represented as a JSON object.
        * @return {String} String representation of the state of the widget.
        * @memberof module:NumericDisplayEVO
        * @instance
        */

        /**
         * @function <a name="getVizExpression">getVizExpression</a>
         * @description Returns the expression defining the visibility of the widget.
         * @memberof module:NumericDisplayEVO
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
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
        * @function <a name="select">select</a>
        * @description Selects the widget -- useful to highlight the widget programmaticaly.
        * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
        * @memberof module:NumericDisplayEVO
        * @instance
        */

        /**
         * @function <a name="deselect">deselect</a>
         * @description Deselects the widget.
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:NumericDisplayEVO
         * @instance
         */

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:NumericDisplayEVO
         * @instance
         */
    }

    NumericDisplayEVO.widgetKeys = BasicDisplayEVO.widgetKeys.concat("cursorName");
    module.exports = NumericDisplayEVO;
});
