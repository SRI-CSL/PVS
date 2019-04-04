/**
 * @module BasicDisplayEVO
 * @version 1.0
 * @description Renders a digital display for rendering text.
 *              This module provide APIs for setting up the visual appearance of the widget, e.g., font size and color.
 * @author Paolo Masci
 * @date Dec 11, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses BasicDisplayEVO
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: { d3: "../lib/d3", text: "../lib/text" }
 });
 require(["widgets/core/BasicDisplayEVO"], function (BasicDisplayEVO) {
      "use strict";
      var disp = new BasicDisplayEVO("disp", {
        top: 200, left: 120, height: 24, width: 120
      }, {
        fontColor: "black",
        fontsize: 16,
        backgroundColor: "blue"
      });
      disp.render("Hello World!"); // The display shows Hello World!
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    const WidgetEVO = require("widgets/core/WidgetEVO");

    class BasicDisplayEVO extends WidgetEVO {
        /**
         * @function <a name="BasicDisplayEVO">BasicDisplayEVO</a>
         * @description Constructor.
         * @augments WidgetEVO
         * @param id {String} The ID of the touchscreen button.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget area.
         *        Default is { top: 0, left: 0, width: 32, height: 20 }.
         * @param opt {Object} Style options defining the visual appearance of the widget.
         *                     Options can be given either as standard html style attributes or using the following widget attributes:
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
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
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>visibleWhen (String): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         *                  The following additional attribute links the display widget to a specific state attribute of a model:
         *          <li>displayKey (String): name of the state attribute defining the display content. Default is the ID of the widget.</li>
         * @memberof module:BasicDisplayEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};

            // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
            opt.type = opt.type || "BasicDisplayEVO";
            opt.backgroundColor = opt.backgroundColor || "transparent";
            opt.cursor = opt.cursor || "default";
            opt.overflow = "hidden";
            
            // invoke WidgetEVO constructor to create the widget
            super(id, coords, opt);

            // set display key
            this.displayKey = (typeof opt.displayKey === "string") ? opt.displayKey : id;

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
         * @memberof module:BasicDisplayEVO
         * @instance
         */
        render (state, opt) {
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
            state = (state === undefined || state === null)? "" : state;
            if (typeof state === "string" || typeof state === "number") {
                this.base.text(state);
            } else if (typeof state === "object" && this.displayKey !== "" && this.evalViz(state)) {
                this.base.text(this.evaluate(this.displayKey, state));
            }
            this.reveal();
            return this;
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         * @memberof module:BasicDisplayEVO
         * @instance
         */
        renderSample () {
            return this.render("ABC12", { fontColor: "white", backgroundColor: "black" });
        }


        /**
         * @function <a name="renderGlyphicon">renderGlyphicon</a>
         * @description Renders a glyphicon.
         * @param icon (String) Name of the glyphicon to be rendered, e.g., glyphicon-time. Glyphicon names are those of the bootstrap library (https://getbootstrap.com/docs/3.3/components/)
         * @param opt {Object} Attributes characterising the visual appearance of the widget:
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is black, "transparent")</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:BasicDisplayEVO
         * @instance
         */
        renderGlyphicon (icon, opt) {
            this.setStyle(opt);
            if (icon) {
                this.base.classed("blink glyphicon " + icon, true).style("font-family", "");
            }
            this.reveal();
            return this;
        }

        getPrimaryKey () {
            return this.displayKey;
        }

        // getKeys () {
        //     return {
        //         displayKey: this.displayKey
        //     };
        // }


        // the following methods are inherited from WidgetEVO

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
        * @function <a name="move">move</a>
        * @description Changes the position of the widget according to the coordinates given as parameter.
        * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
        * @param opt {Object}
        *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
        *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
        * @memberof module:BasicDisplayEVO
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
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
        * @function <a name="evaluate">evaluate</a>
        * @description Returns the state of the widget.
        * @param attr {String} Name of the state attribute associated with the widget.
        * @param state {Object} Current system state, represented as a JSON object.
        * @return {String} String representation of the state of the widget.
        * @memberof module:BasicDisplayEVO
        * @instance
        */

        /**
         * @function <a name="getVizExpression">getVizExpression</a>
         * @description Returns the expression defining the visibility of the widget.
         * @memberof module:BasicDisplayEVO
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
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
        * @function <a name="select">select</a>
        * @description Selects the widget -- useful to highlight the widget programmaticaly.
        * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
        * @memberof module:BasicDisplayEVO
        * @instance
        */

        /**
         * @function <a name="deselect">deselect</a>
         * @description Deselects the widget.
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:BasicDisplayEVO
         * @instance
         */

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:BasicDisplayEVO
         * @instance
         */
    }

    BasicDisplayEVO.widgetKeys = [ "displayKey" ];
    module.exports = BasicDisplayEVO;
});
