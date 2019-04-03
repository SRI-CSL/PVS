/**
 * @module TouchScreenEVO
 * @version 1.0
 * @description Renders a touchscreen display.
 * @author Paolo Masci
 * @date Jul 25, 2018
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses TouchScreenEVO
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: { d3: "../lib/d3", text: "../lib/text" }
 });
 require(["widgets/core/TouchScreenEVO"], function (TouchScreenEVO) {
      "use strict";
      var disp = new TouchScreenEVO("touch", {
        top: 200, left: 120, height: 24, width: 120
      }, {
        callback: function (err, data) { console.log("Ok button clicked"); console.log(data); }
      });
     disp.render("touch me!"); // The display shows touch me!
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext:true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    const ButtonEVO = require("widgets/core/ButtonEVO");

    class TouchScreenEVO extends ButtonEVO {
        /**
         * @function <a name="TouchScreenEVO">TouchScreenEVO</a>
         * @description Constructor.
         * @param id {String} The ID of the touchscreen button.
         * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
         *        the left, top corner, and the width and height of the (rectangular) widget area.
         *        Default is { top: 0, left: 0, width: 32, height: 20 }.
         * @param opt {Object} Options:
         *          <li>blinking (bool): whether the button is blinking (default is false, i.e., does not blink)</li>
         *          <li>align (String): text align: "center", "right", "left", "justify" (default is "center")</li>
         *          <li>backgroundColor (String): background display color (default is transparent)</li>
         *          <li>borderColor (String): border color, must be a valid HTML5 color (default is "steelblue")</li>
         *          <li>borderRadius (Number|String): border radius, must be a number or a valid HTML5 border radius, e.g., 2, "2px", etc. (default is 0, i.e., square border)</li>
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>buttonReadback (String): playback text reproduced with synthesised voice wheneven an action is performed with the button.</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>overlayColor (String): color of the semi-transparent overlay layer used indicating mouse over button, button pressed, etc (default is steelblue)</li>
         *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
         *          <li>position (String): standard HTML position attribute indicating the position of the widget with respect to the parent, e.g., "relative", "absolute" (default is "absolute")</li>
         *          <li>pushButton (Bool): if true, the visual aspect of the button resembles a push button, i.e., the button remains selected after clicking the button</li>
         *          <li>softLabel (String): the button label (default is blank).</li>
         *          <li>dblclick_timeout (Number): timeout, in milliseconds, for detecting double clicks (default is 350ms)</li>
         *          <li>toggleButton (Bool): if true, the visual aspect of the button resembles a toggle button, i.e., the button remains selected after clicking the button</li>
         *          <li>visibleWhen (string): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         *                  The following additional attributes define which events are triggered when the button is activated:
         *          <li>evts (String|Array[String]): actions associated to the widget. Can be either "click", or "press/release" (default is "click").
         *                             Actions can be specified either as a string and using an array of strings (this is useful for backwards compatibility with old prototypes)
         *                             The function associated with the widget is given by the widget name prefixed with the action name.
         *                             In the case of "press/release", the widget is associated to two functions: press_<id> and release_<id>.</li>
         *          <li>customFunctionText (String): overrides the standard action name associated with click events.</li>
         *          <li>functionText (String): defines the action names associated with the widget.
         *                                     The indicated name is prefixed with the string indicated in opt.evts.</li>
         *          <li>keyCode (Number): binds the widget to keyboard keyCodes. Use e.g., http://keycode.info/, to see keyCodes</li>
         *          <li>rate (Number): interval, in milliseconds, for repeating button clicks when the button is pressed and held down (default is 250ms)</li>
         * @memberof module:TouchScreenEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};

            // override default style options
            opt.fontSize = opt.fontSize || 12;
            opt.backgroundColor = opt.backgroundColor || "steelblue";
            opt.fontColor = opt.fontColor || "white";
            opt.touchscreenMode = true;
            opt.type = opt.type || "TouchScreenEVO";

            // invoke ButtonEVO constructor to create the widget
            super(id, coords, opt);

            return this;
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         * @memberof module:TouchScreenEVO
         * @instance
         */
        renderSample () {
            return this.render("touch");
        }

        // getKeys () {
        //     return {
        //         displayKey: this.displayKey,
        //         functionText: this.functionKey,
        //         customFunctionText: this.customFunctionText,
        //         evts: this.evts
        //     };
        // }

        // all other functions are inherited from ButtonEVO
    }

    TouchScreenEVO.widgetKeys = ButtonEVO.widgetKeys.concat("displayKey");
    module.exports = TouchScreenEVO;
});
