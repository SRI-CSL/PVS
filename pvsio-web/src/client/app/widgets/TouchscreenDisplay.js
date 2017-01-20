/**
 * @module TouchscreenDisplay
 * @version 2.0
 * @description Renders a touchscreen display, combining three elements: button, basic display, and numeric display.
 *              This module provide APIs for changing the look and feel of
 *              the rendered text, including: cursors, background color, font, size, alignment.
 * @author Paolo Masci, Patrick Oladimeji
 * @date Apr 1, 2015
 *
 * @example <caption>Typical use of TouchscreenDisplay APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses TouchscreenDisplay.
 * define(function (require, exports, module) {
 *     "use strict";
 *     var device = {};
 *     device.disp = new TouchscreenDisplay("disp", { top: 222, left: 96, height: 8, width: 38 });
 *     device.disp.render(10); // the display renders 10
 * });
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
      *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
      * @memberof module:SingleDisplay
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
      * Returns a JSON object representation of this Widget.
      * @returns {object}
      * @memberof module:BasicDisplay
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


     TouchscreenDisplay.prototype.hide = function () {
         this.div.style("display", "none");
         return this;
     };

     TouchscreenDisplay.prototype.reveal = function () {
         this.div.style("display", "block");
         return this;
     };

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
