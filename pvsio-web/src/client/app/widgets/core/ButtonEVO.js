/**
 * @module ButtonEVO
 * @version 1.0
 * @description Renders a customisable button.
 *              The button has two layers: one layer renders the visual appearance, the other layer captures user interactions with the widget.
 *              This module provide APIs for setting up the visual appearance of the button and the user interactions captured by the button.
 *              Note that the button can also be transparent and without label: this is useful for creating
 *              interactive areas over pictures of a user interface.
 * @author Paolo Masci
 * @date Dec 11, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses ButtonEVO
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib",
         text: "../lib/text",
         stateParser: "./util/PVSioStateParser"
     }
 });
 require(["widgets/core/ButtonEVO"], function (ButtonEVO) {
      "use strict";
      let device = {};
      device.btnOk = new ButtonEVO("btnOk", {
        top: 200, left: 120, height: 24, width: 120
      }, {
        softLabel: "Ok",
        fontColor: "black",
        backgroundColor: "blue",
        fontsize: 16,
        callback: function (err, data) { console.log("Ok button clicked"); console.log(data); }
      });
     device.btnOk.render(); // The button is rendered.
     device.btnOk.click();  // Actions can be triggered programmatically. Clicking the button has the effect of sending a command "click_btnOk(<current state>)" to the PVSio-web back-end
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    const Timer = require("util/Timer"),
          WidgetEVO = require("widgets/core/WidgetEVO"),
          ButtonHalo2 = require("widgets/ButtonHalo2"),
          ButtonActionsQueue = require("widgets/ButtonActionsQueue").getInstance();

    const CLICK_RATE = 250; // 250 milliseconds, default interval for repeating button clicks when the button is pressed and held down
                             // going below 250ms may cause multiple unintended activations when pressing the button
    const DBLCLICK_TIMEOUT = 350; // 350 milliseconds, default (and minimum) timeout for detecting double clicks

    //-- Utility functions -----------------------
    function installHandlers(_this, opt) {
        opt = opt || {};
        function onButtonPress(_this) {
            if (_this.evts["press/release"]) {
                _this.pressAndHold();
            } else if (_this.evts.click && !_this.touchscreenMode) {
                _this.click();
            }
        }
        function onButtonRelease(_this) {
            if (_this.evts["press/release"]) {
                _this.release();
            } else if (_this.evts.click && _this.touchscreenMode) {
                _this.click();
            }
        }
        function onButtonDoubleClick(_this) {
            if (_this.evts.dblclick) {
                _this.dblclick();
            }
        }
        _this.overlay.on("mouseover", function () {
            if (!(_this.toggleButton || _this.pushButton) || _this.isSelected) {
                _this.select({ opacity: 0.8, "background-color": "steelblue" });
            } else { _this.select({ opacity: 0.4, "background-color": "transparent" }); }
            _this.hover = true;
        }).on("mouseout", function () {
            if (!_this.isSelected) { _this.deselect(); }
            if (_this._tick) { onButtonRelease(_this); }
            _this.hover = false;
        }).on("mousedown", function () {
            _this.isSelected = (_this.pushButton)? true : (_this.toggleButton) ? !_this.isSelected : _this.isSelected;
            if (_this.isSelected) {
                _this.select({ opacity: 0.4 });
            } else { _this.deselect(); }
            onButtonPress(_this);
        }).on("mouseup", function () {
            onButtonRelease(_this);
            if (_this.isSelected || (_this.hover && !_this.toggleButton)) {
                _this.select({ opacity: 0.8, "background-color": "steelblue" });
            } else { _this.deselect(); }
            // the following code is the dblclick handler
            if (_this.dblclick_timer) {
                onButtonDoubleClick(_this);
                clearInterval(_this.dblclick_timer);
                _this.dblclick_timer = null;
            } else {
                _this.dblclick_timer = setTimeout(function () {
                    _this.dblclick_timer = null;
                }, _this.dblclick_timeout);
            }
        }).on("blur", function () {
            if (_this.isSelected || (_this.hover && !_this.toggleButton)) {
                _this.select({ opacity: 0.4 });
            } else { _this.deselect(); }
            if (_this._tick) { onButtonRelease(_this); }
            _this.hover = false;
        });
        // bind key events
        if (_this.keyCode) {
            ButtonHalo2.getInstance().installKeypressHandler(_this, {
                keyCode: _this.keyCode,
                coords: { left: _this.left, top: _this.top, height: _this.height, width: _this.width },
                evts: _this.evts,
                noHalo: opt.noHalo
            });
        }
    }
    function btn_action(evt, _this, opt) {
        opt = opt || {};
        opt.callback = opt.callback || _this.callback;
        opt.functionText = opt.customFunctionText || _this.customFunctionText || (evt + "_" + _this.functionText);
        ButtonActionsQueue.queueGUIAction(opt.functionText, opt.callback);
        _this.deselect();
        // console.log(opt.functionText);
    }

    class ButtonEVO extends WidgetEVO {
        /**
         * @function <a name="ButtonEVO">ButtonEVO</a>
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
         * @memberof module:ButtonEVO
         * @instance
         */
        constructor (id, coords, opt) {
            opt = opt || {};
            // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
            opt.type = opt.type || "ButtonEVO";
            opt.backgroundColor = opt.backgroundColor || "transparent";
            opt.fontColor = opt.fontColor || "black";
            opt.cursor = opt.cursor || "pointer";
            opt.borderWidth = (!isNaN(parseFloat(opt.borderWidth))) ? parseFloat(opt.borderWidth) : (opt.borderColor) ? 1 : 0;
            opt.borderRadius = (!isNaN(parseFloat(opt.borderRadius))) ? parseFloat(opt.borderRadius) : 4;
            opt.zIndex = opt.zIndex || 1; // z-index for buttons should be at least 1, so they are placed over display widgets
            opt.overlayColor = opt.overlayColor || "steelblue";

            // invoke WidgetEVO constructor to create the basic widget
            super(id, coords, opt);

            // add button-specific functionalities
            this.buttonReadback = opt.buttonReadback || "";
            this.toggleButton = opt.toggleButton;
            this.pushButton = opt.pushButton;
            this.softLabel = opt.softLabel || "";
            this.touchscreenMode = opt.touchscreenMode || false;
            this.keyCode = opt.keyCode;
            this.functionText = opt.functionText || id;
            this.customFunctionText = opt.customFunctionText;
            this.rate = (isNaN(parseFloat(opt.rate))) ? CLICK_RATE : Math.max(CLICK_RATE, parseFloat(opt.rate));
            this.dblclick_timeout = (isNaN(parseFloat(opt.dblclick_timeout))) ? DBLCLICK_TIMEOUT : Math.max(DBLCLICK_TIMEOUT, parseFloat(opt.rate));

            // associate relevant actions to the button
            opt.evts = opt.evts || "click";
            if (typeof opt.evts === "object" && opt.evts.length > 0) {
                this.evts = {};
                this.evts.click = (opt.evts.filter(function (evt) { return evt === "click"; }).length > 0);
                this.evts.dblclick = (opt.evts.filter(function (evt) { return evt === "dblclick"; }).length > 0);
                this.evts["press/release"] = (opt.evts.filter(function (evt) { return evt === "press/release"; }).length > 0);
            } else {
                this.evts = {
                    "press/release": (opt.evts === "press/release"),
                    "click": (opt.evts === "click"),
                    "dblclick": (opt.evts === "dblclick")
                };
            }

            // prepare timers necessary for executing press & hold actions
            let _this = this;
            this._timer = new Timer(this.rate);
            this.callback = opt.callback || function (err, res) { console.log("Warning: " + _this.id + " does not have a callback :/"); };
            if (!opt.callback) { this.callback(); }

            // install action handlers
            installHandlers(this);
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
         *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid", "dotted", "dashed", etc. (default is "none")</li>
         *          <li>borderWidth (Number): border width (if option borderColor !== null then the default border is 2px, otherwise 0px, i.e., no border)</li>
         *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
         *          <li>fontFamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
         *          <li>fontSize (Number): font size (default is (coords.height - opt.borderWidth) / 2 )</li>
         *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
         *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
         * @memberof module:ButtonEVO
         * @instance
         */
        render (state, opt) {
            // set style
            opt = this.normaliseOptions(opt);
            // handle options that need units
            opt["font-size"] = (opt.fontSize || this.style["font-size"]) + "pt";
            opt["border-width"] = (opt.borderWidth) ? opt.borderWidth + "px" : this.style["border-width"];
            this.setStyle(opt);

            // render content
            state = (state === undefined || state === null)? "" : state;
            if (this.evalViz(state)) {
                if (typeof state === "string") {
                    this.base.text(state);
                } else {
                    this.base.text(this.softLabel);
                }
                this.reveal();
            }
            return this;
        }

        /**
         * @function <a name="renderSample">renderSample</a>
         * @description Version of the render function that demonstrates the functionalities of the widget.
         * @memberof module:ButtonEVO
         * @instance
         */
        renderSample () {
            return this.render("Esc", { borderColor: "steelblue", borderWidth: 1 });
        }



        /**
         * @function <a name="click">click</a>
         * @description API to simulate a click action on the button
         * @memberof module:ButtonEVO
         * @instance
         */
        click (opt) {
            btn_action("click", this, opt);
            return this;
        }

        /**
         * @function <a name="dblclick">dblclick</a>
         * @description API to simulate a double click action on the button
         * @memberof module:ButtonEVO
         * @instance
         */
        dblclick (opt) {
            btn_action("dblclick", this, opt);
            return this;
        }

        /**
         * @function <a name="press">press</a>
         * @description API to simulate a single press action on the button
         * @memberof module:ButtonEVO
         * @instance
         */
        press (opt) {
            btn_action("press", this, opt);
            return this;
        }

        /**
         * @function <a name="pressAndHold">pressAndHold</a>
         * @description API to simulate a press & hold action on the button
         * @memberof module:ButtonEVO
         * @instance
         */
        pressAndHold (opt) {
            let _this = this;
            this._tick = function () {
                _this.press(opt);
            };
            this._timer.addListener("TimerTicked", function () {
                if (_this._tick) {
                    _this._tick();
                } else { console.err("Error in ButtonEVO: undefined handler for tick function."); }
            });
            _this.press(opt);
            this._timer.interval(_this.rate).start();
            return this;
        }

        /**
         * @function <a name="release">release</a>
         * @description API to simulate a release action on the button
         * @memberof module:ButtonEVO
         * @instance
         */
        release (opt) {
            btn_action("release", this, opt);
            this._tick = null;
            this._timer.reset();
            return this;
        }

        getPrimaryKey () {
            return this.functionText;
        }

        // getKeys () {
        //     return {
        //         functionText: this.functionText,
        //         customFunctionText: this.customFunctionText,
        //         evts: this.getEvents()
        //     };
        // }

        // getEvents () {
        //     let ans = [];
        //     let _this = this;
        //     Object.keys(this.evts).forEach(function (key) {
        //         if (_this.evts[key]) { ans.push(key); }
        //     });
        //     return ans.join(", ");
        // }


        // the following methods are inherited from WidgetEVO

        /**
         * @function <a name="reveal">reveal</a>
         * @description Reveals the widget.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="hide">hide</a>
         * @description Hides the widget.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="move">move</a>
         * @description Changes the position of the widget according to the coordinates given as parameter.
         * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
         * @param opt {Object}
         *         <li> duration (Number): duration in milliseconds of the move transition (default is 0, i.e., instantaneous) </li>
         *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-out") </li>
         * @memberof module:ButtonEVO
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
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="remove">remove</a>
         * @description Removes the div elements of the widget from the html page -- useful to programmaticaly remove widgets from a page.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="evalViz">evalViz</a>
         * @description Evaluates the visibility of the widget based on the state attrbutes (passed as function parameter) and the expression stored in this.visibleWhen
         * @param state {Object} JSON object with the current value of the state attributes of the modelled system
         * @return {bool} true if the state attributes indicate widget visible, otherwise false.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="evaluate">evaluate</a>
         * @description Returns the state of the widget.
         * @param attr {String} Name of the state attribute associated with the widget.
         * @param state {Object} Current system state, represented as a JSON object.
         * @return {String} String representation of the state of the widget.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="getVizExpression">getVizExpression</a>
         * @description Returns the expression defining the visibility of the widget.
         * @memberof module:ButtonEVO
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
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="invertColors">invertColors</a>
         * @description Inverts the colors of the display (as in a negative film).
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="select">select</a>
         * @description Selects the widget -- useful to highlight the widget programmaticaly.
         * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="deselect">deselect</a>
         * @description Deselects the widget.
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="getPosition">getPosition</a>
         * @description Returns the position of the widget
         * @return {Object} Coordinates of the widget, in the form { left: x, top: y }, where x and y are real numbers
         * @memberof module:ButtonEVO
         * @instance
         */

        /**
         * @function <a name="getSize">getSize</a>
         * @description Returns the size of the widget
         * @return {Object} Size of the widget, in the form { width: x, height: y }, where x and y are real numbers
         * @memberof module:ButtonEVO
         * @instance
         */
    }

    ButtonEVO.widgetKeys = [ "functionText" ];
    module.exports = ButtonEVO;
});
