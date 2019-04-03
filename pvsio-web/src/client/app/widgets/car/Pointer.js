/**
 * @module Pointer
 * @version 1.0.0
 * @author Henrique Pacheco
 * @desc This module is responsible for building Pointer elements that can
 * be used by the GaugeSport and Clock widgets.
 * @date July 12, 2017
 *
 * @example <caption>Usage of Pointer within a PVSio-web project.</caption>
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // Require the Pointer module
 *     require("widgets/car/Pointer");
 *
 *     function main() {
 *          // After Pointer module was loaded, initialize it
 *          var pointer = new Pointer(
 *               'example', // id of the element that will be created
 *               { top: 100, left: 100, width: 300, height: 300 }, // coordinates object
 *               { style: "pointer" }
 *               // description of the possible options in the constructor documentation.
 *           );
 *
 *          // Rotates the pointer 180º
 *          pointer.render(180);
 *     }
 * });
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var SVGWidget = require("widgets/car/SVGWidget");

    /**
     * @function constructor
     * @description Constructor for the Pointer widget.
     * @param id {String} The id of the widget instance.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 125, left: 125, width: 250, height: 250 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body").</li>
     *          <li>position (String): value for the CSS property position (default is "absolute").</li>
     *          <li>style (String): a valid style identifier (default is 1).</li>
     *          <li>transition (Float): number of milliseconds to be applied in the CSS property transition (default is 0).</li>
     *          <li>z-index (String): value for the CSS property z-index (if not provided, no z-index is applied).</li>
     *          <li>initial (Float): The initial absolute value for the movement of the pointer (default is min value).</li>
     *          <li>transform_origin (String): Value for the CSS property transform origin -should be provided as
     * percentages and not absolute values. Examples are "center top" or "50% 20%".</li>
     *          <li>filename (String): The path to the pointer file, in the cr/svg/gauge-pointers directory.</li>
     * @returns {Pointer} The created instance of the widget Pointer.
     * @memberof module:Pointer
     * @instance
     */
    function Pointer(id, coords, opt) {
        SVGWidget.call(this, id, coords, opt);
        this.setId(id);

        // Handle coords object
        coords = coords || {};
        this.top = coords.top || 125;
        this.left = coords.left || 125;
        this.width = coords.width || 250;
        this.height = coords.height || 250;

        // Handle options and default values
        opt = opt || {};
        opt.position = opt.position || "absolute";
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.transition = opt.transition || 0;
        this.style_id = opt.style || 'gauge-pointer-3';
        this.filename = opt.filename || this.style_id;
        this.initial = opt.initial || 0;

        // Merge the provided opt with the default style configs
        this.opt = this.mergeConfigs(this.getDefaultStyleConfigs(opt.style || 'gauge-pointer-3'), this.opt);

        // Find pointer file to load from style configs
        var file_to_require = "text!widgets/car/svg/gauge-pointers/" + this.filename + ".svg";
        var self = this;
        require([file_to_require], function(file_required) {
            // Add pointer div
            self.div = d3.select(self.parent).append("div")
                .attr('id', id + '_pointer')
                .style("position", opt.position)
                .style("top", self.top + 'px')
                .style("left", self.left + 'px')
                .style("height", self.height + 'px')
                .style("width", self.width + 'px')
                .style('display', 'block')
                .style('margin', 'auto')
                .html(file_required);

            // Set transform origin attribute on the SVG element
            self.div.select('svg')
                .style("transform-origin", self.opt.transform_origin)
                .style("-webkit-transition", "all "+self.transition+"s ease")
                .style("-moz-transition", "all "+self.transition+"s ease")
                .style("-ms-transition", "all "+self.transition+"s ease")
                .style("-o-transition", "all "+self.transition+"s ease")
                .style("transition", "all "+self.transition+"s ease");

            if(opt['z-index'] !== undefined) {
                self.div.style('z-index', opt['z-index']);
            }

            // Set initial position
            self.ready();
            self.render(self.initial);
            return self;
        });

        return this;
    }

    Pointer.prototype = Object.create(SVGWidget.prototype);
    Pointer.prototype.constructor = Pointer;
    Pointer.prototype.parentClass = SVGWidget.prototype;

    /**
     * @function render
     * @description Render method of the Pointer widget. Re-renders the pointer
     * with the provided new value and configurations.
     * @param value {Float} Value in degrees for the pointer rotation.
     * @param opt {Object} Override options when re-rendering. See constructor
     * docs for detailed docs on the available options.
     * @memberof module:Pointer
     * @instance
     */
    Pointer.prototype.render = function(value, opt) {

        if(this.isReady()) {
            this.div.select('svg').style('transform', 'rotate('+value+'deg)');
            this.setValue(value);
        }

        return this;
    };

    /**
     * @function getDefaultStyleConfigs
     * @description Returns the style configurations for the provided style
     * identifier. The possible styles for the Pointer widget are the numbers
     * from 1-5, 7-10 and 15-23.
     * @param style_id {string} The style identifier - should match the file name
     * which will be loaded for the pointer.
     * @returns {Object} An object of configurations for the provided style
     * identifier. The possible configurations can be consulted in the
     * documentation of the constructor method.
     * @memberof module:Pointer
     * @instance
     */
    Pointer.prototype.getDefaultStyleConfigs = function (style_id)
    {
        switch (style_id) {
            case 'gauge-pointer-3':
                return {
                    transform_origin: "50% 20%",
                };

            case 'gauge-pointer-4':
                return {
                    transform_origin: "50% 12%",
                };

            case 'gauge-pointer-5':
                return {
                    transform_origin: "50% 4.5%",
                };

            case 'gauge-pointer-7':
                return {
                    transform_origin: "50% 0%",
                };

            case 'gauge-pointer-8':
                return {
                    transform_origin: "50% 10%",
                };

            case 'gauge-pointer-9':
                return {
                    transform_origin: "50% 20%",
                };

            case 'gauge-pointer-10':
                return {
                    transform_origin: "50% 17.5%",
                };

            case 'gauge-pointer-15':
                return {
                    transform_origin: "50% 50%",
                };

            case 'gauge-pointer-17':
                return {
                    transform_origin: "50% 25%",
                };

            case 'gauge-pointer-18':
                return {
                    transform_origin: "50% 16%",
                };

            case 'gauge-pointer-19':
                return {
                    transform_origin: "50% 35%",
                };

            case 'gauge-pointer-20':
                return {
                    transform_origin: "50% 22%",
                    };

            case 'gauge-pointer-21':
                return {
                    transform_origin: "50% 35%",
                };

            case 'gauge-pointer-22':
                return {
                    transform_origin: "50% 14%",
                };

            case 'gauge-pointer-23':
                return {
                    transform_origin: "50% 9.5%",
                };

            case 'pointer':
                return {
                    transform_origin: "50% 20%",
                };

            default:
                return {
                    transform_origin: "center top",
                };
        }
    };

    module.exports = Pointer;
});
