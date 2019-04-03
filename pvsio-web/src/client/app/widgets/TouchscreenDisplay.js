/**
 * @module TouchscreenDisplay
 * @version 2.0
 * @description Renders a touchscreen display.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji
 * @date Apr 1, 2015
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses TouchscreenDisplay
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib"
     }
 });
 require(["widgets/TouchscreenDisplay"], function (TouchscreenDisplay) {
      "use strict";
      var device = {};
      device.touchdisp = new TouchscreenDisplay("touchdisp", {
        top: 250, left: 120, height: 24, width: 120
      }, {
        displayKey: "touchdisp",
        fontColor: "yellow",
        backgroundColor: "black",
        fontsize: 12,
        callback: function (err, data) { console.log("Touchscreen display touched"); console.log(data); }
      });
     device.touchdisp.render({ touchdisp: "touch display" }); // The touchscreen display is rendered. Clicking the button has the effect of sending a command "click_touchdisp(<current state>)" to the pvs back-end
 });
 *
 */
 /*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
 /*global define, dimColor */

 define(function (require, exports, module) {
     "use strict";

     var d3 = require("d3/d3"),
         StateParser = require("util/PVSioStateParser"),
         Widget = require("widgets/Widget"),
         Button = require("widgets/Button"),
         BasicDisplay = require("widgets/BasicDisplay"),
         NumericDisplay= require("widgets/NumericDisplay"),
         property = require("util/property");

     /**
      * @function <a name="TouchscreenDisplay">TouchscreenDisplay</a>
      * @description Constructor.
      * @param id {String} The ID of the touchscreen button.
      * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
      *        the left, top corner, and the width and height of the (rectangular) display.
      *        Default is { top: 0, left: 0, width: 32, height: 20 }.
      * @param opt {Object} Options:
      *          <li>displayKey (string): the name of the state attribute defining the display content. This information will be used by the render method. Default is the ID of the display.
      *          <li>visibleWhen (string): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).
      *          <li>fontfamily (String): font type (default is "sans-serif")</li>
      *          <li>fontsize (Number): font size (default is 0.8 * height)</li>
      *          <li>fontfamily (String): font family, must be a valid HTML5 font name (default is "sans-serif")</li>
      *          <li>fontColor (String): font color, must be a valid HTML5 color (default is "white", i.e., "#fff")</li>
      *          <li>backgroundColor (String): background display color (default is black, "#000")</li>
      *          <li>borderWidth (Number): border width (default is 0, i.e., no border, unless option borderColor has been specified -- in this case, the border is 2px)</li>
      *          <li>borderStyle (String): border style, must be a valid HTML5 border style, e.g., "solid" (default is "none")</li>
      *          <li>borderColor (String): border color, must be a valid HTML5 color (default color used in the widget is "black")</li>
      *          <li>align (String): text alignment (available options are "left", "right", anc "center". Default is "center")</li>
      *          <li>cursor (String): cursor style, must be a valid HTML5 cursor style, e.g., "pointer", "crosshair", etc. (default is "default")</li>
      *          <li>blinking (Bool): true means the text is blinking (default is false, i.e., not blinking)</li>
      *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     function TouchscreenDisplay(id, coords, opt) {
         opt = opt || {};
         this.id = id;
         this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
         this.top = coords.top || 0;
         this.left = coords.left || 0;
         this.width = coords.width || 32;
         this.height = coords.height || 10;
         this.fontsize = opt.fontsize || (this.height * 0.9);
         this.fontfamily = opt.fontfamily || "sans-serif";
         this.font = [this.fontsize, "px ", this.fontfamily];
         this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
         this.align = opt.align || "center";
         this.backgroundColor = opt.backgroundColor || "black";
         this.fontColor = opt.fontColor || "#fff"; //white
         this.cursor = opt.cursor || "pointer";
         this.blinking = opt.blinking || false;
         this.textBaseline = "middle";
         this.btnClass = opt.btnClass || "primary";
         var elemClass = id + " noselect";
         if (this.blinking) { elemClass += " blink"; }
         opt.position = opt.position || "absolute";
         opt.displayMode = opt.displayMode || "standard"; // supported modes are "standard" and "numeric"
         this.displayMode = property.call(this, opt.displayMode);

         opt.displayKey = opt.displayKey || id;
         opt.cursorName = opt.cursorName || "";
         opt.auditoryFeedback = (opt.auditoryFeedback) ?
             ((opt.auditoryFeedback.toString() === "enabled" || opt.auditoryFeedback.toString() === "true") ? "enabled" : "disabled") : "disabled";
         opt.visibleWhen = opt.visibleWhen || "true";
         opt.functionText = opt.functionText || "";
         opt.evts = opt.evts || [ "click" ];
         this.displayKey = property.call(this, opt.displayKey);
         this.cursorName = property.call(this, opt.cursorName);
         this.auditoryFeedback = property.call(this, opt.auditoryFeedback);
         this.visibleWhen = property.call(this, opt.visibleWhen);
         this.functionText = property.call(this, opt.functionText);
         this.evts = property.call(this, opt.evts);

         var _this = this;
         this.div = d3.select(this.parent)
                         .append("div").style("position", opt.position)
                         .style("top", this.top + "px").style("left", this.left + "px")
                         .style("width", this.width + "px").style("height", this.height + "px")
                         .style("margin", 0).style("padding", 0).style("border-width", 0)
                         .style("display", "none").attr("id", id).attr("class", elemClass)
                         .style("cursor", this.cursor);
         this.overlayDisplay = new BasicDisplay(id + "_overlayDisplay", {
             height: this.height, width: this.width
         }, {
             fontsize: this.fontsize,
             fontColor: this.fontColor,
             backgroundColor: this.backgroundColor,
             borderWidth: opt.borderWidth,
             borderStyle: opt.borderStyle,
             borderColor: opt.borderColor,
             cursor: this.cursor,
             displayKey: this.displayKey(),
             auditoryFeedback: this.auditoryFeedback(),
             position: "relative",
             parent: id
         });
         this.overlayNumericDisplay = new NumericDisplay(id + "_overlayNumericDisplay", {
             height: this.height, width: this.width
         }, {
             fontsize: this.fontsize,
             fontColor: this.fontColor,
             backgroundColor: this.backgroundColor,
             borderWidth: opt.borderWidth,
             borderStyle: opt.borderStyle,
             borderColor: opt.borderColor,
             cursor: this.cursor,
             cursorName: this.cursorName(),
             displayKey: this.displayKey(),
             auditoryFeedback: this.auditoryFeedback(),
             position: "relative",
             parent: id
         });
         this.overlayButton = new Button(id + "_overlayButton", {
             left: this.left, top: this.top, height: this.height, width: this.width
         }, {
             functionText: this.functionText(),
             callback: opt.callback || function () {},
             evts: opt.events || ['click'],
             area: this.div,
             parent: id
         });
         d3.select("#" + id + "_overlayDisplay").on("mouseover", function () {
             if (_this.displayMode() === "standard") {
                 _this.overlayDisplay.setColors({ backgroundColor: dimColor(_this.backgroundColor) });
             }
         }).on("mouseout", function () {
             if (_this.displayMode() === "standard") {
                 _this.overlayDisplay.setColors({ backgroundColor: _this.backgroundColor });
             }
         }).on("mousedown", function () {
             if (_this.displayMode() === "standard") {
                 _this.overlayDisplay.setColors({
                     backgroundColor: "black",
                     fontColor: "white"
                 });
             }
         }).on("mouseup", function () {
             if (_this.displayMode() === "standard") {
                 _this.overlayDisplay.setColors({
                     backgroundColor: _this.backgroundColor,
                     fontColor: _this.fontColor
                 }, {
                     auditoryFeedback: _this.auditoryFeedback()
                 });
                 _this.overlayButton.click();
             }
         });
         d3.select("#" + id + "_overlayNumericDisplay").on("mouseover", function () {
             if (_this.displayMode() === "numeric") {
                 _this.overlayNumericDisplay.setColors({ backgroundColor: dimColor(_this.backgroundColor) });
             }
         }).on("mouseout", function () {
             if (_this.displayMode() === "numeric") {
                 _this.overlayNumericDisplay.setColors({ backgroundColor: _this.backgroundColor });
             }
         }).on("mousedown", function () {
             if (_this.displayMode() === "numeric") {
                 _this.overlayNumericDisplay.setColors({
                     backgroundColor: "black",
                     fontColor: "white"
                 });
             }
         }).on("mouseup", function () {
             if (_this.displayMode() === "numeric") {
                 _this.overlayNumericDisplay.setColors({
                     backgroundColor: _this.backgroundColor,
                     fontColor: _this.fontColor
                 }, {
                     auditoryFeedback: _this.auditoryFeedback()
                 });
                 _this.overlayButton.click();
             }
         });
         this.txt = opt.softLabel || id;
         this.example = opt.example || "touch";
         Widget.call(this, id, "touchscreendisplay");
         return this;
     }
     TouchscreenDisplay.prototype = Object.create(Widget.prototype);
     TouchscreenDisplay.prototype.constructor = TouchscreenDisplay;
     TouchscreenDisplay.prototype.parentClass = Widget.prototype;

     /**
      * @function <a name="toJSON">toJSON</a>
      * @description Returns a serialised version of the widget in JSON format.
      *              This is useful for saving/loading a specific instance of the widget.
      *              In the current implementation, the following attributes are included in the JSON object:
      *              <li> type (string): widget type, i.e., "touchscreendisplay" in this case
      *              <li> id (string): the unique identifier of the widget instance
      *              <li> displayKey (string): the name of the state attribute defining the display content.
      *              <li> fontsize (string): the font size of the button
      *              <li> fontColor (string): the font color of the button
      *              <li> backgroundColor (string): the background color of the button
      *              <li> boundFunctions (string): the name of the command that will be sent to the pvs back-end when the display is touched.
      *              <li> visibleWhen (string): a booloan expression defining when the condition under which the widget is visible
      *              <li> auditoryFeedback (string): whether display readback is enabled
      * @returns JSON object
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     TouchscreenDisplay.prototype.toJSON = function () {
         return {
             type: this.type(),
             id: this.id(),
             displayKey: this.displayKey(),
             evts: this.evts(),
             functionText: this.functionText(),
             boundFunctions: this.overlayButton.boundFunctions(),
             auditoryFeedback: this.auditoryFeedback(),
             visibleWhen: this.visibleWhen(),
             fontsize: this.fontsize,
             fontColor: this.fontColor,
             backgroundColor: this.backgroundColor
         };
     };
     /**
     * Updates the location and size of the widget according to the given position and size
      */
     TouchscreenDisplay.prototype.updateLocationAndSize = function (pos, opt) {
         opt = opt || {};
         if (opt.imageMap) {
             TouchscreenDisplay.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
         }
         this.top = pos.y || 0;
         this.left = pos.x || 0;
         this.width = pos.width || 200;
         this.height = pos.height || 80;
        //  this.fontsize = this.height * 0.9;
        //  this.font = [this.fontsize, "px ", this.fontfamily];
        //  this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
         d3.select("div." + this.id()).style("left", this.left + "px").style("top", this.top + "px")
             .style("width", this.width + "px").style("height", this.height + "px").style("font-size", this.fontsize + "px");
         // only resize is needed, because we have already moved the div element containing the display and button areas
         this.overlayDisplay.updateLocationAndSize({ width: pos.width, height: pos.height });
         this.overlayNumericDisplay.updateLocationAndSize({ width: pos.width, height: pos.height });
         this.overlayButton.updateLocationAndSize({ width: pos.width, height: pos.height });
         return this.render(this.example, opt);
     };
     TouchscreenDisplay.prototype.updateStyle = function (data) {
         data = data || {};
         this.fontsize = data.fontsize || this.fontsize;
         this.font = [this.fontsize, "px ", this.fontfamily];
         this.smallFont = [(this.fontsize * 0.7), "px ", this.fontfamily];
         this.fontColor = data.fontColor || this.fontColor;
         this.backgroundColor = data.backgroundColor || this.backgroundColor;
         return this;
     };
     TouchscreenDisplay.prototype.updateWithProperties = function (props) {
         TouchscreenDisplay.prototype.parentClass.updateWithProperties.apply(this, arguments);
         this.overlayButton.updateWithProperties(props);
         return this;
     };
     /**
      * Removes the widget's div
      */
     TouchscreenDisplay.prototype.remove = function () {
         TouchscreenDisplay.prototype.parentClass.remove.apply(this);
         d3.select("div." + this.id()).remove();
     };


     /**
      * @function <a name="render">render</a>
      * @param data {Object} JSON object representing the touchscreen display to be rendered.
      *                      The display value is specified in the attribute <displayKey>
      *                      (the actual value of <displayKey> is instantiated when creating the widget, see constructor's options)
      * @param opt {Object} Override options for the display style, useful to dynamically change the display style during simulations. Available options include:
      *              <li> fontsize (string): the font size of the display
      *              <li> fontColor (string): the font color of the display
      *              <li> backgroundColor (string): the background color of the display
      *              <li> blinking (Bool): true means the text is blinking
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     TouchscreenDisplay.prototype.render = function (state, opt) {
         opt = opt || {};

         // state is used to check whether the button is visible/enabled
         // the expression visibleWhen() is the condition we need to check on the state
         var isEnabled = false;
         var visibleWhen = opt.visibleWhen || this.visibleWhen();
         var expr = StateParser.simpleExpressionParser(visibleWhen);
         if (expr && expr.res) {
             if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                 isEnabled = true;
             } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                 var str = StateParser.resolve(state, expr.res.attr);
                 if (str) {
                     str = StateParser.evaluate(str);
                     if ((expr.res.binop === "=" && str === expr.res.constant) ||
                          (expr.res.binop === "!=" && str !== expr.res.constant)) {
                              isEnabled = true;
                     }
                 }
             }
         }
         if (isEnabled) {
             var txt = state;
             opt.displayMode = opt.displayMode || this.displayMode();
             if (opt.displayMode === "numeric") {
                 this.overlayNumericDisplay.render(txt, opt);
                 this.overlayDisplay.hide();
             } else {
                 if (typeof txt === "object") {
                    var dispVal = StateParser.resolve(txt, this.displayKey());
                    if (dispVal) {
                        if (typeof dispVal === "object") {
                            dispVal = JSON.stringify(dispVal);
                        }
                        dispVal = StateParser.evaluate(dispVal).toString().replace(new RegExp("\"", "g"), "");
                    }
                    this.overlayDisplay.render(dispVal, opt);
                } else {
                    this.overlayDisplay.render(txt);
                }
                this.overlayNumericDisplay.hide();
             }
             this.txt = txt || this.txt;
             this.example = txt || this.example;
             return this.reveal();
         }
         return this.hide();
     };
     TouchscreenDisplay.prototype.renderSample = function (opt) {
         opt = opt || {};
         var txt = opt.txt || this.example;
         return this.render(txt, { visibleWhen: "true" });
     };


     /**
      * @function <a name="hide">hide</a>
      * @description Hides the widget
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     TouchscreenDisplay.prototype.hide = function () {
         this.div.style("display", "none");
         return this;
     };

     /**
      * @function <a name="reveal">reveal</a>
      * @description Makes the widget visible
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     TouchscreenDisplay.prototype.reveal = function () {
         this.div.style("display", "block");
         return this;
     };

     /**
      * @function <a name="move">move</a>
      * @description Changes the position of the widget according to the coordinates given as parameter.
      * @param data {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
      * @memberof module:TouchscreenDisplay
      * @instance
      */
     TouchscreenDisplay.prototype.move = function (data) {
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

     module.exports = TouchscreenDisplay;
 });
