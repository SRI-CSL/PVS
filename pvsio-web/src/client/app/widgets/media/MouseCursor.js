/**
 * @module MouseCursor
 * @version 1.0
 * @description Renders a mouse cursor.
 *              The widget is implemented using a png image embedded in a div.
 * @author Paolo Masci
 * @date Dec 23, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses MouseCursor
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
 require(["widgets/core/MouseCursor"], function (MouseCursor) {
      "use strict";
      var device = {};
      device.cursor = new MouseCursor("cursor", {
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
        mouse = require("text!widgets/media/imgs/mouse_cursor.svg"),
        img_template = require("text!widgets/media/templates/img_template.handlebars");

    /**
     * @function <a name="MouseCursor">MouseCursor</a>
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
     * @memberof module:MouseCursor
     * @instance
     */
     function MouseCursor(id, coords, opt) {
         coords = coords || {};
         opt = opt || {};

         // set widget type
         this.type = this.type || "MouseCursor";

         // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
         opt.backgroundColor = "transparent";
         opt.zIndex = opt.zIndex || "inherit";
         opt.opacity = opt.opacity || 0.9;

         // invoke WidgetEVO constructor to create the widget
         WidgetEVO.apply(this, [ id, coords, opt ]);

         // append the pointer image to the base layer
         var dom = Handlebars.compile(img_template, { noEscape: true })({
             svg: mouse
         });
         this.base.html(dom);
         return this;
     }
     MouseCursor.prototype = Object.create(WidgetEVO.prototype);
     MouseCursor.prototype.parentClass = WidgetEVO.prototype;
     MouseCursor.prototype.constructor = MouseCursor;

     /**
      * @function <a name="render">render</a>
      * @description Rendering function for button widgets.
      * @memberof module:MouseCursor
      * @instance
      */
     MouseCursor.prototype.render = function () {
         return this.reveal();
     };

     /**
      * @function <a name="click">click</a>
      * @description Simulates a click -- this function does nothing for now.
      * @memberof module:MouseCursor
      * @instance
      */
     MouseCursor.prototype.click = function () {
         return this.reveal();
     };

     module.exports = MouseCursor;
});
