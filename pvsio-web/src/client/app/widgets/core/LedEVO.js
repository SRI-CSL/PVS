/**
 * @module LedEVO
 * @version 1.0
 * @description Renders an LED light
 * @author Paolo Masci
 * @date Jul 16, 2018
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses LedEVO
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: { d3: "../lib/d3", text: "../lib/text" }
 });
 require(["widgets/core/LedEVO"], function (LedEVO) {
      "use strict";
      var led = new LedEVO("disp", {
        top: 200, left: 120, height: 24, width: 120
      });
      led.render(); // The LED is on
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    let WidgetEVO = require("widgets/core/WidgetEVO");

    const COLOR = {
        brightGreen: "#00FF66"
    };

    class LedEVO extends WidgetEVO {
        /**
         * @function <a name="LedEVO">LedEVO</a>
         * @description Constructor.
         * @augments WidgetEVO
         * @param id {String} The ID of the touchscreen button.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget area.
         *        Default is { top: 0, left: 0, width: 32, height: 20 }.
         * @param opt {Object} Style options defining the visual appearance of the widget.
         *                     Options can be given either as standard html style attributes or using the following widget attributes:
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>ledKey (String): The name of the state attribute defining the color of the display. This information will be used by the render method. Default is the ID of the display.</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>visibleWhen (String): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         *                  The following additional attribute links the display widget to a specific state attribute of a model:
         * @memberof module:LedEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};

            let size = Math.min(coords.width, coords.height) || 1;
            opt.marginLeft = (coords.width - size / 2) / 2;
            opt.marginTop = (coords.height - size / 2) / 2;
            coords.height = coords.width = size / 2;

            // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
            opt.type = opt.type || "LedEVO";
            opt.backgroundColor = opt.color || opt.backgroundColor || COLOR.brightGreen;
            opt.cursor = opt.cursor || "default";
            opt.borderRadius = opt.borderRadius || Math.max(coords.width, coords.height); // this will produce a round LED
            opt.overflow = "visible";
            
            // invoke WidgetEVO constructor to create the widget
            super(id, coords, opt);

            // delete unnecessary style options
            delete this.style["font-size"];
            delete this.style["font-family"];
            delete this.style["text-align"];
            delete this.style["white-space"];

            // set display key
            this.ledKey = (typeof opt.ledKey === "string") ? opt.ledKey : id;

            return this;
        }

        /**
         * @function <a name="render">render</a>
         * @description Rendering function for button widgets.
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
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:LedEVO
         * @instance
         */
        render (state, opt) {
            // set style
            opt = this.normaliseOptions(opt);
            opt["background-color"] = opt.color || opt.backgroundColor || this.style["background-color"];
            opt["border-width"] = (opt.borderWidth) ? opt.borderWidth + "px" : this.style["border-width"];
            opt["border-style"] = opt.borderStyle || this.style["border-style"];
            opt["border-radius"] = (isNaN(parseFloat(opt.borderRadius))) ? this.style["border-radius"] : opt.borderRadius;
            opt["border-color"] = opt.borderColor || this.style["border-color"];
            this.setStyle(opt);

            // update style, if necessary
            state = (state === undefined || state === null)? "" : state;
            if (typeof state === "string") {
                this.setStyle({ "background-color": state });
            } else if (typeof state === "object" && this.ledKey !== "" && this.evalViz(state)) {
                this.setStyle({ "background-color": this.evaluate(this.ledKey, state) });
            }

            this.reveal();
            return this;
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         * @memberof module:LedEVO
         * @instance
         */
        renderSample () {
            return this.render(COLOR.brightGreen);
        }

        getPrimaryKey () {
            return this.ledKey;
        }

        // getKeys () {
        //     return {
        //         ledKey: this.ledKey,
        //         ledColor: this.ledColor
        //     }
        // }

        // the following methods are inherited from WidgetEVO

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:LedEVO
         * @instance
         */

        /**
        * @function <a name="move">move</a>
        * @description Changes the position of the widget according to the coordinates given as parameter.
        * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
        * @param opt {Object}
        *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
        *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
        * @memberof module:LedEVO
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
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:LedEVO
         * @instance
         */

        /**
        * @function <a name="evaluate">evaluate</a>
        * @description Returns the state of the widget.
        * @param attr {String} Name of the state attribute associated with the widget.
        * @param state {Object} Current system state, represented as a JSON object.
        * @return {String} String representation of the state of the widget.
        * @memberof module:LedEVO
        * @instance
        */

        /**
         * @function <a name="getVizExpression">getVizExpression</a>
         * @description Returns the expression defining the visibility of the widget.
         * @memberof module:LedEVO
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
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:LedEVO
         * @instance
         */

        /**
        * @function <a name="select">select</a>
        * @description Selects the widget -- useful to highlight the widget programmaticaly.
        * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
        * @memberof module:LedEVO
        * @instance
        */

        /**
         * @function <a name="deselect">deselect</a>
         * @description Deselects the widget.
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:LedEVO
         * @instance
         */

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:LedEVO
         * @instance
         */
    }

    LedEVO.widgetKeys = [ "ledKey" ];
    module.exports = LedEVO;
});
