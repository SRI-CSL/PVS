/**
 * @module Stylus
 * @version 1.0
 * @description Renders a mouse cursor.
 *              The widget is implemented using a png image embedded in a div.
 * @author Paolo Masci
 * @date Dec 23, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses Stylus
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
 require(["widgets/core/Stylus"], function (Stylus) {
      "use strict";
      var device = {};
      device.cursor = new Stylus("cursor", {
        top: 200, left: 120, height: 24, width: 120
      });
     device.cursor.render(); // The cursor is rendered.
     device.cursor.move( { top: 100, left: 300 }, { duration: 1000 }); // The cursor is moved to position (100,300), and the animation duration is 1 second.
 });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */
define(function (require, exports, module) {
    "use strict";
    var WidgetEVO = require("widgets/core/WidgetEVO"),
        stylus = require("text!widgets/media/imgs/stylus_white.svg"),
        img_template = require("text!widgets/media/templates/img_template.handlebars");

    /**
     * @function <a name="Stylus">Stylus</a>
     * @description Constructor.
     * @param id {String} The ID of the touchscreen button.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) widget area.
     *        Default is { top: 0, left: 0, width: 32, height: 20 }.
     * @param opt {Object} Options:
     *          <li>opacity (Number): opacity of the button. Valid range is [0..1], where 0 is transparent, 1 is opaque (default is opaque)</li>
     *          <li>parent (String): the HTML element where the display will be appended (default is "body")</li>
     *          <li>visibleWhen (string): boolean expression indicating when the display is visible. The expression can use only simple comparison operators (=, !=) and boolean constants (true, false). Default is true (i.e., always visible).</li>
     *          <li>zIndex (String): z-index property of the widget (default is 1)</li>
     * @memberof module:Stylus
     * @instance
     */
     function Stylus(id, coords, opt) {
         coords = coords || {};
         opt = opt || {};

         // set widget type
         this.type = this.type || "Stylus";

         // adjust position, based on the stylus size: the coordinates are indicating the position of the stylus pointer
         this.style = this.style || {};
         this.style["margin-top"] = "-680px";
         this.style["margin-left"] = "-40px";

         // store initial position
         this.initial_position = {
             top: coords.top,
             left: coords.left
         };

         // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
         opt.backgroundColor = "transparent";
         opt.zIndex = opt.zIndex || "inherit";
         opt.opacity = opt.opacity || 1;

         // invoke WidgetEVO constructor to create the widget
         WidgetEVO.apply(this, [ id, coords, opt ]);

         // append the pointer image to the base layer
         var dom = Handlebars.compile(img_template, { noEscape: true })({
             svg: stylus
         });
         this.base.html(dom);
         return this;
     }
     Stylus.prototype = Object.create(WidgetEVO.prototype);
     Stylus.prototype.parentClass = WidgetEVO.prototype;
     Stylus.prototype.constructor = Stylus;

     /**
      * @function <a name="render">render</a>
      * @description Rendering function for button widgets.
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.render = function () {
         return this.reveal();
     };

     /**
      * @function <a name="click">click</a>
      * @description Simulates a small forward movement of the stylus. The overall duration of the movement is 1000ms.
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.click = function (opt) {
         opt = opt || {};
         opt.fw_move = opt.fw_move || 20;
         this.move({ top: this.top + opt.fw_move, left: this.left - opt.fw_move }, {
             duration: 200
         });
         var _this = this;
         setTimeout(function () {
             _this.move({ top: _this.top - opt.fw_move, left: _this.left + opt.fw_move }, {
                 duration: 500
             });
         }, 500);
         return this.reveal();
     };

     /**
      * @function <a name="longPress">longPress</a>
      * @description Simulates a small forward movement of the stylus. The overall duration of the movement is 3000ms.
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.longPress = function (opt) {
         opt = opt || {};
         opt.fw_move = opt.fw_move || 20;
         this.move({ top: this.top + opt.fw_move, left: this.left - opt.fw_move }, {
             duration: 200
         });
         var _this = this;
         setTimeout(function () {
             _this.move({ top: _this.top - opt.fw_move, left: _this.left + opt.fw_move }, {
                 duration: 500
             });
         }, 2500);
         return this.reveal();
     };

     /**
      * @override
      * @function <a name="move">move</a>
      * @description Changes the position of the stylus according to the coordinates given as parameter. The stylus is automatically rotated by 35 degree when moving the stylus.
      * @param coords {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
      * @param opt {Object}
      *         <li> duration (Number): duration in milliseconds of the move transition (default is 1000) </li>
      *         <li> transitionTimingFunction (String): HTML5 timing function (default is "ease-in") </li>
      *         <li> rotation (bool): whether the stylus should be rotated while moving it</li>
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.move = function (coords, opt) {
         if (this.div && this.div.node()) {
             coords = coords || {};
             opt = opt || {};
             opt.duration = opt.duration || 1000;
             opt.transitionTimingFunction = opt.transitionTimingFunction || "ease-out";
             opt.rotation = (opt.rotation !== undefined) ? opt.rotation : true;
             this.top = (isNaN(parseFloat(coords.top))) ? this.top : parseFloat(parseFloat(coords.top).toFixed(WidgetEVO.MAX_COORDINATES_ACCURACY));
             this.left = (isNaN(parseFloat(coords.left))) ? this.left : parseFloat(parseFloat(coords.left).toFixed(WidgetEVO.MAX_COORDINATES_ACCURACY));
             // the translation needs to be normalised by the top / left attributes of the div
             var rotation_offset = (opt.rotation) ? { top: 45, left: 360 } : { top: 0, left: 0 };
             var rotation_val = (opt.rotation) ? 35 : 0;
             this.base.style("transform", "translateX(" + (this.left + rotation_offset.left + parseFloat(this.style["margin-left"])) + "px)"
                                            + "translateY(" + (this.top + rotation_offset.top + parseFloat(this.style["margin-top"])) + "px)"
                                             + "rotate(" + rotation_val + "deg)")
                    .style("transform-origin", "bottom")
                    .style("transition-duration", opt.duration + "ms")
                    .style("transition-timing-function", opt.transitionTimingFunction);
         }
         return this;
     };

     /**
      * @override
      * @function <a name="select">select</a>
      * @description Selects the widget -- useful to highlight the widget programmaticaly.
      * @param style {Object} Set of valid HTML5 attributes characterising the visual appearance of the widget.
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.select = function (opt) {
         opt = opt || {};
         opt.opacity = (isNaN(parseFloat(opt.opacity))) ? 1 : opt.opacity;
         return this.setStyle(opt);
     };

     /**
      * @override
      * @function <a name="reveal">reveal</a>
      * @description Reveals the stylus using fade-in effect
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.reveal = function () {
         if (this.div && this.div.node() && this.div.style("display") !== "block") {
             this.div.style("display", "block").style("opacity", 0).transition().duration(250).style("opacity", this.opacity);
         }
         return this;
     };

     /**
      * @override
      * @function <a name="hide">reveal</a>
      * @description Hides the stylus with a fade-out effect. The position is reset to the initial position, and rotation is reset to 0.
      * @memberof module:Stylus
      * @instance
      */
     Stylus.prototype.hide = function () {
         if (this.div && this.div.node() && this.div.style("display") === "block") {
             this.div.transition().duration(250).style("opacity", 0);
             var _this = this;
             setTimeout(function () {
                 _this.move(_this.initial_position, { rotation: false });
                 _this.div.style("display", "none");
             }, 260);
         }
         return this;
     };

     module.exports = Stylus;
});
