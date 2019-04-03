/**
 * @module GaugeSport
 * @version 1.0.0
 * @author Paolo Masci
 * @desc This module helps you building gauge widgets using SVG files. Uses the Pointer module to
 * draw pointers for the gauges.
 *
 * @date June 25, 2017
 *
 * @example <caption>Usage of GaugeSport within a PVSio-web project.</caption>
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // Require the GaugeSport module
 *     require("widgets/car/GaugeSport");
 *
 *     function main() {
 *          // After GaugeSport module was loaded, initialize it
 *          var tachometer = new GaugeSport(
 *              // Id of the generated HTML element
 *              'id-tachometer',
 *              // Coordinates object
 *              { top: 100, left: 100, width: 300, height: 300 },
 *              // Configuration object
 *              {
 *                  panel_file: 'gauge-tachometer-panel-1.svg',
 *                  pointers: [
 *                      {
 *                          id: 'tachometer-pointer',
 *                          min: 0,
 *                          max: 10,
 *                          min_degree: 58,
 *                          max_degree: 306,
 *                          style: 'gauge-pointer-3',
 *                          width: 38,
 *                          top: 133,
 *                          left: 133
 *                      }
 *                  ]
 *              }
 *          );
 *
 *          // Render the GaugeSport instance
 *          tachometer.render(2);
 *     }
 * });
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var Pointer = require("widgets/car/Pointer");
    var SVGWidget = require("widgets/car/SVGWidget");

    /**
     * @function constructor
     * @description Constructor for the GaugeSport widget.
     * @param id {String} The id of the widget instance.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 250, height: 250 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body").</li>
     *          <li>position (String): value for the CSS property position (default is "absolute").</li>
     *          <li>style (String): a valid style identifier (default is "tachometer").</li>
     *          <li>z-index (String): value for the CSS property z-index (if not provided, no z-index is applied).</li>
     *          <li>panel_file (String) - the path to the SVG panel file (inside the widgets/car/svg/gauge-panels)
     * directory</li>
    *           <li>pointers (Array) - an array of objects with the configurations that should be provided to the Pointer
     * that the style should compose. This object can contain the following properties:</li>
     *          <li>id (String): An unique identifier for the pointer.</li>
     *          <li>top (Float): Top coord of the Pointer widget.</li>
     *          <li>left (Float): Left coord of the Pointer widget.</li>
     *          <li>width (Float): Width coord of the Pointer widget.</li>
     *          <li>height (Float): Height coord of the Pointer widget.</li>
     *          <li>style (String): Style identifier for the Pointer widget.</li>
     *          <li>filename (String): The path to the pointer file, in the cr/svg/gauge-pointers directory.</li>
     *          <li>transform_origin (String): Value for the CSS property transform origin -should be provided as
     * percentages and not absolute values. Examples are "center top" or "50% 20%".</li>
     *          <li>initial (Float): The initial absolute value for the movement of the pointer (default is min value).</li>
     *          <li>transition (Float): number of milliseconds to be applied in the CSS property transition (default is 0).</li>
     *          <li>min_degree (Float): The minimum degree of range for the pointer movement (default is 90).</li>
     *          <li>min (Float): The minimum absolute value for the movement of the pointer (default is 0).</li>
     *          <li>max_degree (Float): The maximum degree of range for the pointer movement (default is 270).</li>
     *          <li>max (Float): The maximum absolute value for the movement of the pointer (default is 10).</li>
     * @returns {GaugeSport} The created instance of the widget GaugeSport.
     * @memberof module:GaugeSport
     * @instance
     */
    function GaugeSport(id, coords, opt) {
        SVGWidget.call(this, id, coords, opt);
        this.setId(id);

        // Ready to render control flag
        this.readyToRender = false;

        // Handle coords
        coords = coords || {};
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 250;
        this.height = coords.height || 250;

        // Handle options
        opt = opt || {};
        this.opt = opt;

        this.pointerOverrideOpts = opt.pointer || {};

        // Merge the provided opt with the default style configs
        this.opt = this.mergeConfigs(this.getDefaultStyleConfigs(opt.style || 'tachometer'), this.opt);

        // Find panel file to load from style configs
        var file_to_require = "text!widgets/car/svg/gauge-panels/" + this.opt.panel_file;
        var self = this;
        require([file_to_require], function(file_required) {

            // Aux configurations and variables
            opt.position = opt.position || "absolute";
            self.parent = (opt.parent) ? ("#" + opt.parent) : "body";

            // Find configs for Pointer widget(s)
            self.pointers = [];
            self.pointers_opt = [];
            var pointer_opts = self.opt.pointers;

            if(pointer_opts !== undefined && pointer_opts.constructor === Array) {

                // Add all Pointer widgets to the screen
                pointer_opts.map(function(opt) {

                    // Pointer options' default values
                    opt.parent = self.id;
                    opt.id = opt.id || self.id + '-pointer';
                    opt.min_degree = opt.min_degree || 90; // deg
                    opt.max_degree = opt.max_degree || 270; // deg
                    opt.max = opt.max || 10;
                    opt.min = opt.min || 0;
                    opt.initial = opt.inital || opt.min_degree;

                    // Override default configs with the ones provided
                    opt = self.mergeConfigs(opt, self.pointerOverrideOpts);

                    // Create Pointer widget instance and save it for later usage in rendering
                    self.pointers[opt.id] = new Pointer(
                        opt.id,
                        { top: opt.top, left: opt.left, height: self.height, width: self.width },
                        opt
                    );

                    // Save pointer opts for later usage
                    self.pointers_opt[opt.id] = opt;
                });
            }

            // Create the gauge element
            self.div = d3.select(self.parent)
                .append('div').attr('id', id)
                .style("position", opt.position)
                .style("top", self.top + "px").style("left", self.left + "px")
                .style("width", self.width + "px").style("height", self.height + "px")
                .html(file_required);

            if(opt['z-index'] !== undefined) {
                self.div.style('z-index', self.opt['z-index']);
            }

            // Get SVG's width and height as integer
            var svgHeight = parseFloat(self.div.select('svg').style('height').replace('px', ''));
            var svgWidth = parseFloat(self.div.select('svg').style('width').replace('px', ''));

            // Calc max deficit between width and height for the original div
            var widthDeficit = svgWidth - self.width;
            var heightDeficit = svgHeight - self.height;

            var ratio = (widthDeficit === heightDeficit || widthDeficit > heightDeficit) ?
                self.width / svgWidth : self.height / svgHeight;

            // Set transform origin attributes and scale the SVG elements
            self.div.style("transform-origin", "0 0").style('transform', 'scale('+ (ratio * 1.2) +')');

            self.ready();
            return self;
        });

        return this;
    }

    GaugeSport.prototype = Object.create(SVGWidget.prototype);
    GaugeSport.prototype.constructor = GaugeSport;
    GaugeSport.prototype.parentClass = SVGWidget.prototype;

    /**
     * @function render
     * @description Render method of the GaugeSport widget. Calls the render method of the associated pointers of this widget,
     * with the provided value(s) - check the value param documentation for more info on the rendering process.
     * @param value {Float|Object} The value provided can either be a float or an object. If it is a float, the all of the
     * widget pointers' render method will be called with the provided value. If it is an object, then the properties of the objects
     * should match identifiers of pointers, and the value for those properties is the value that will be provided to the render
     * methods. An example of this behavior is the object { pt1: 10, pt2: 20 }, where the render method of the pointer pt1 will
     * be called with 10, and the render method of pointer pt2 will be called with 20.
     * @param opt {Object} Override options when re-rendering. See constructor docs for detailed docs on the available options.
     * @memberof module:GaugeSport
     * @instance
     */
    GaugeSport.prototype.render = function(value, opt) {
        if (this.isReady() === false) {
            // retry render after one second
            let _this = this;
            setTimeout(function () {
                _this.render(value, opt);
            }, 1000);
        } else {
            if (typeof value !== 'undefined') {
                if(value.constructor === Object) {
                    for (var prop in value) {
                        if (value.hasOwnProperty(prop) && this.pointers.hasOwnProperty(prop)) {
                            this.renderPointer(this.pointers[prop], value[prop], this.pointers_opt[prop]);
                        }
                    }
                } else {
                    var self = this;
                    Object.keys(this.pointers).map(function(key, index) {
                        self.renderPointer(self.pointers[key], value, self.pointers_opt[key]);
                    });
                }
                this.setValue(value);
            }
        }
        return this;
    };

    GaugeSport.prototype.renderSample = function () {
        return this.render();
    };

    /**
     * @function renderPointer
     * @description Calls the render method of the provided Pointer instance
     * with the provided value.
     * @param pointer {Pointer} Instance of the Pointer widget that should be rendered.
     * @param value {Float} Value (between min and max configurations) to be rendered.
     * @param pointer_opt {Object} Object with pointer rotation configurations.
     * @memberof module:GaugeSport
     * @instance
     * @private
     */
    GaugeSport.prototype.renderPointer = function (pointer, value, pointer_opt) {

        value = Math.max(value, pointer_opt.min);
        value = Math.min(value, pointer_opt.max);

        pointer.render(this.value2deg(value, pointer_opt));
    };

    /**
     * @function value2deg
     * @description Converts the provided value to degress of rotation, taking
     * into account the minimum and maximum rotation degrees.
     * @param value {Float} The value to convert to degrees.
     * @param pointer_opt {Object} Object with pointer rotation configurations.
     * @returns {Float} The converted value in degrees.
     * @memberof module:GaugeSport
     * @instance
     * @private
     */
    GaugeSport.prototype.value2deg = function (value, pointer_opt) {
        var rangePerc = (value - pointer_opt.min) / (pointer_opt.max - pointer_opt.min);
        var interpolatedOffset = rangePerc * (pointer_opt.max_degree - pointer_opt.min_degree);
        return pointer_opt.min_degree + interpolatedOffset;
    };

    /**
     * @function getDefaultStyleConfigs
     * @description Returns the default style configurations for the provided style identifier.
     * The possible styles for the GaugeSport widget are:
     * <li>tachomter, tachomter2, tachomter3, tachomter4</li>
     * <li>speedomter, speedomter2, speedomter3, speedomter4, speedomter5, speedomter6, speedomter7, speedomter8</li>
     * <li>thermometer</li>
     * <li>fuel, fuel2, fuel3, fuel4</li>
     * <li>pressure</li>
     * <li>compass, compass2</li>
     * @param style_id {string} The style identifier.
     * @returns {Object} An object of default configurations for the provided style identifier.
     * Check the documentation of the constructor method for the possible configurations.
     * @memberof module:GaugeSport
     * @instance
     */
    GaugeSport.prototype.getDefaultStyleConfigs = function (style_id) {
        switch (style_id) {
            // Tachomter styles
            case 'tachometer':
                return {
                    panel_file: 'gauge-tachometer-panel-1.svg',
                    pointers: [
                        {
                            id: 'tachometer-pointer',
                            max: 10,
                            min_degree: 56,
                            max_degree: 304,
                            style: 'gauge-pointer-3',
                            width: 38,
                            top: 120,
                            left: 120,
                        }
                    ]
                };
            case 'tachometer2':
                return {
                    panel_file: 'gauge-tachometer-panel-2.svg',
                    pointers: [
                        {
                            id: 'tachometer2-pointer',
                            max: 8,
                            min_degree: 65,
                            max_degree: 298,
                            style: 'gauge-pointer-20',
                            top: 105,
                            left: 112,
                            width: 26,
                        }
                    ]
                };
            case 'tachometer3':
                return {
                    panel_file: 'gauge-tachometer-panel-3.svg',
                    pointers: [
                        {
                            id: 'tachometer3-pointer',
                            max: 7,
                            min_degree: 73,
                            max_degree: 181,
                            style: 'gauge-pointer-9',
                            width: 8,
                            top: 105,
                            left: 112.5,
                        }
                    ]
                };
            case 'tachometer4':
                return {
                    panel_file: 'gauge-tachometer-panel-4.svg',
                    pointers: [
                        {
                            id: 'tachometer4-pointer',
                            max: 8,
                            min_degree: 68,
                            max_degree: 298,
                            style: 'gauge-pointer-10',
                            top: 110,
                            left: 106,
                            width: 38,
                        }
                    ]
                };


            // Speedometer Styles
            case 'speedometer':
                return {
                    panel_file: 'gauge-speedometer-panel-1.svg',
                    pointers: [
                        {
                            id: 'speedometer-pointer',
                            min_degree: 64,
                            min: 10,
                            max_degree: 309,
                            max: 140,
                            style: 'gauge-pointer-4',
                            top: 111,
                            left: 111,
                            width: 34,
                        }
                    ]
                };
            case 'speedometer2':
                return {
                    panel_file: 'gauge-speedometer-panel-2.svg',
                    pointers: [
                        {
                            id: 'speedometer2-pointer',
                            min_degree: 90,
                            max_degree: 330,
                            max: 120,
                            style: 'gauge-pointer-23',
                            top: 110,
                            left: 113.5,
                            width: 23,
                        }
                    ]
                };
            case 'speedometer3':
                return {
                    panel_file: 'gauge-speedometer-panel-3.svg',
                    pointers: [
                        {
                            id: 'speedometer3-pointer',
                            min_degree: 79,
                            max_degree: 279,
                            max: 190,
                            top: 122,
                            left: 114,
                            width: 20,
                            style: 'gauge-pointer-5',
                        }
                    ]
                };
            case 'speedometer4':
                return {
                    panel_file: 'gauge-speedometer-panel-4.svg',
                    pointers: [
                        {
                            id: 'speedometer4-pointer',
                            min_degree: 78,
                            max_degree: 284,
                            max: 140,
                            style: 'gauge-pointer-2',
                            width: 8,
                        }
                    ]
                };
            case 'speedometer5':
                return {
                    panel_file: 'gauge-speedometer-panel-5.svg',
                    pointers: [
                        {
                            id: 'speedometer5-pointer',
                            min_degree: 60,
                            max_degree: 304,
                            max: 240,
                            style: 'gauge-pointer-2',
                            width: 8,
                        }
                    ]
                };
            case 'speedometer6':
                return {
                    panel_file: 'gauge-speedometer-panel-6.svg',
                    pointers: [
                        {
                            id: 'speedometer6-pointer',
                            min_degree: 49,
                            max_degree: 315,
                            max: 120,
                            style: 'gauge-pointer-9',
                            width: 8,
                            top: 105,
                            left: 112.5,
                        }
                    ]
                };
            case 'speedometer7':
                return {
                    panel_file: 'gauge-speedometer-panel-7.svg',
                    pointers: [
                        {
                            id: 'speedometer7-pointer',
                            min_degree: 55,
                            max_degree: 308,
                            max: 140,
                            style: 'gauge-pointer-10',
                        }
                    ]
                };
            case 'speedometer8':
                return {
                    panel_file: 'gauge-speedometer-panel-8.svg',
                    pointers: [
                        {
                            id: 'speedometer8-pointer',
                            min_degree: 48,
                            max_degree: 312,
                            max: 220,
                            top: 108,
                            left: 108,
                            style: 'gauge-pointer-22',
                            width: 34,
                        }
                    ]
                };


            // Thermometer styles
            case 'thermometer':
                return {
                    panel_file: 'gauge-temperature-panel-1.svg',
                    pointers: [
                        {
                            id: 'thermometer-pointer',
                            min_degree: 45,
                            max_degree: 135,
                            max: 130,
                            min: 50,
                            top: 113,
                            left: 195,
                            style: 'gauge-pointer-8',
                            transition: 0.3,
                        }
                    ]
                };


            // Fuel styles
            case 'fuel':
                return {
                    panel_file: 'gauge-fuel-panel-1.svg',
                    pointers: [
                        {
                            id: 'fuel-pointer',
                            min_degree: 115,
                            max_degree: 242,
                            max: 100,
                            top: 115,
                            left: 119,
                            height: 80,
                            style: 'gauge-pointer-16',
                            transition: 0.3,
                            width: 18,
                        }
                    ]
                };
            case 'fuel2':
                return {
                    panel_file: 'gauge-combo-fuel-pressure-panel-1.svg',
                    pointers: [
                        {
                            id: 'temperature',
                            min_degree: 50,
                            max_degree: 130,
                            min: 50,
                            max: 130,
                            style: 'gauge-pointer-20',
                            top: 104,
                            left: 89,
                            width: 22,
                            transition: 0.3,
                        },
                        {
                            id: 'fuel',
                            min_degree: -50,
                            max_degree: -130,
                            max: 100,
                            style: 'gauge-pointer-20',
                            top: 104,
                            left: 134,
                            width: 22,
                            transition: 0.3,
                        }
                    ]
                };
            case 'fuel3':
                return {
                    panel_file: 'gauge-fuel-panel-2.svg',
                    pointers: [
                        {
                            id: 'fuel3-pointer',
                            min_degree: 36,
                            max_degree: 144,
                            max: 100,
                            style: 'gauge-pointer-10',
                            top: 100,
                            left: 168,
                            width: 34,
                            transition: 0.3,
                        }
                    ]
                };
            case 'fuel4':
                return {
                    panel_file: 'gauge-fuel-panel-3.svg',
                    pointers: [
                        {
                            id: 'fuel4-pointer',
                            min_degree: 90,
                            max_degree: 270,
                            max: 100,
                            top: 106,
                            left: 106,
                            style: 'gauge-pointer-3',
                            width: 38,
                            transition: 0.3,
                        }
                    ]
                };


            // Pressure styles
            case 'pressure':
                return {
                    panel_file: 'gauge-pressure-panel-1.svg',
                    pointers: [
                        {
                            id: 'pressure-pointer',
                            min_degree: 136,
                            max_degree: 220,
                            max: 100,
                            top: 155,
                            left: 110,
                            style: 'gauge-pointer-5',
                            width: 20,
                            transition: 0.3,
                        }
                    ]
                };


            // Compass styles
            case 'compass':
                return {
                    panel_file: 'gauge-compass-panel-1.svg',
                    pointers: [
                        {
                            id: 'compass-pointer',
                            min_degree: 180,
                            max_degree: 540,
                            min: 0,
                            max: 100,
                            top: 25,
                            left: 112,
                            style: 'gauge-pointer-15',
                            height: 220,
                            transition: 0.5,
                        }
                    ]
                };
            case 'compass2':
                return {
                    panel_file: 'gauge-compass-panel-2.svg',
                    pointers: [
                        {
                            id: 'compass-pointer',
                            min_degree: 180,
                            max_degree: 540,
                            min: 0,
                            max: 100,
                            top: 25,
                            left: 112,
                            style: 'gauge-pointer-15',
                            height: 220,
                            transition: 0.5,
                        }
                    ]
                };

            case 'example':
                return {
                    panel_file: 'example.svg',
                    pointers: [
                        {
                            id: 'example-pointer-id',
                            min_degree: 49,
                            max_degree: 315,
                            max: 120,
                            style: 'gauge-pointer-3',
                            width: 40,
                            top: 124,
                            left: 130,
                        }
                    ]
                };

            default:
                return {
                    panel_file: style_id + '.svg'
                };
        }
    };

    module.exports = GaugeSport;
});
