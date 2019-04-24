/**
 * @module Clock
 * @version 1.0.0
 * @author Henrique Pacheco
 * @desc This module helps you building clock widgets using SVG files. Uses the Pointer module to
 * draw the clock pointers.
 *
 * @date August 19, 2017
 *
 * @example <caption>Usage of Clock within a PVSio-web project.</caption>
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // Require the Clock module
 *     require("widgets/car/Clock");
 *
 *     function main() {
 *          // After Clock module was loaded, initialize it
 *          var clock = new Clock(
 *               'example', // id of the element that will be created
 *               { top: 100, left: 100 }, // coordinates object
 *               {
 *                   panel_file: 'gauge-clock-panel-2.svg',
 *                   pointers: [
 *                       {
 *                           id: 'seconds',
 *                           top: 68,
 *                           left: 119,
 *                           width: 12,
 *                           style: 'gauge-pointer-19',
 *                           min_degree: 180,
 *                           min: 0,
 *                           max_degree: 540,
 *                           max: 60,
 *                       },
 *                       {
 *                           id: 'minutes',
 *                           top: 109,
 *                           left: 120,
 *                           width: 11,
 *                           style: 'gauge-pointer-18',
 *                           min_degree: 180,
 *                           min: 0,
 *                           max_degree: 540,
 *                           max: 60,
 *                       },
 *                       {
 *                           id: 'hours',
 *                           top: 106,
 *                           left: 118,
 *                           height: 60,
 *                           width: 14,
 *                           style: 'gauge-pointer-17',
 *                           min_degree: 180,
 *                           min: 0,
 *                           max_degree: 900,
 *                           max: 24,
 *                       }
 *                   ]
 *                }
 *           );
 *
 *          // Render the Clock widget - based on the current date and time.
 *          clock.render();
 *     }
 * });
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define, _*/
define(function (require, exports, module) {
    "use strict";

    var Pointer = require("widgets/car/Pointer");
    var SVGWidget = require("widgets/car/SVGWidget");

    /**
     * @function constructor
     * @description Constructor for the Clock widget.
     * @param id {String} The id of the widget instance.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 250, height: 250 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body").</li>
     *          <li>position (String): value for the CSS property position (default is "absolute").</li>
     *          <li>style (String): a valid style identifier (default is "clock").</li>
     *          <li>panel_file (String) Path to the SVG panel file (inside the widgets/car/svg/gauge-panels) directory.</li>
     *          <li>pointers (Array) - an array of objects with the configurations of the Pointer widgets
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
     *          <li>transition (Float): number of milliseconds to be applied in the CSS property transition (default is 0).</li>
     *          <li>initial (Float): The initial absolute value for the movement of the pointer (default is min value).</li>
     *          <li>min_degree (Float): The minimum degree of range for the pointer movement (default is 90).</li>
     *          <li>min (Float): The minimum absolute value for the movement of the pointer (default is 0).</li>
     *          <li>max_degree (Float): The maximum degree of range for the pointer movement (default is 270).</li>
     *          <li>max (Float): The maximum absolute value for the movement of the pointer (default is 10).</li>
     * @returns {Clock} The created instance of the widget Clock.
     * @memberof module:Clock
     * @instance
     */
    function Clock(id, coords, opt) {
        SVGWidget.call(this, id, coords, opt);
        this.setId(id);


        // Handle coords
        coords = coords || {};
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 250;
        this.height = coords.height || 250;

        // Handle options
        opt = opt || {};
        this.opt = opt;
        // Merge the provided opt with the default style configs
        this.defaultStyles = this.getDefaultStyleConfigs(opt.style || 'clock');
        this.opt = this.mergeConfigs(_.clone(this.defaultStyles), this.opt);

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

                    // Override default configs with the ones provided
                    var overrideOpt = _.find(
                        self.opt.pointers,
                        function(v){
                            return (!_.isUndefined(v.id) && v.id === opt.id);
                        }
                    ) || {};

                    var styleOpt = _.find(
                        self.defaultStyles.pointers,
                        function(v){
                            return (!_.isUndefined(v.id) && v.id === opt.id);
                        }
                    ) || {};
                    opt = self.mergeConfigs(styleOpt, overrideOpt);

                    // Pointer options' default values
                    opt.parent = self.id;
                    opt.id = opt.id || self.id + '-pointer';
                    opt.min_degree = opt.min_degree || 90; // deg
                    opt.max_degree = opt.max_degree || 270; // deg
                    opt.max = opt.max || 10;
                    opt.min = opt.min || 0;
                    opt.initial = opt.inital || opt.min_degree;

                    // Create Pointer widget
                    self.pointers[opt.id] = new Pointer(
                        self.id + '-' + id,
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

            if(self.opt['z-index'] !== undefined) {
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
            self.div.style("transform-origin", "0 0").style('transform', 'scale('+ratio+')');

            self.ready();
            return self;
        });

        return this;
    }

    Clock.prototype = Object.create(SVGWidget.prototype);
    Clock.prototype.constructor = Clock;
    Clock.prototype.parentClass = SVGWidget.prototype;

    /**
     * @function render
     * @description Render method of the Clock widget. Calls the render method of the associated pointers of this widget,
     * using the current date and time.
     * @memberof module:Clock
     * @instance
     */
    Clock.prototype.render = function() {
        if(this.isReady() === false) {
            let _this = this;
            setTimeout(function () {
                _this.render();
            }, 1000);
        } else {
            // Clock is rendered based on the current date and time
            var current = new Date();
            this.renderPointer(this.pointers.seconds, current.getSeconds(), this.pointers_opt.seconds);
            this.renderPointer(this.pointers.minutes, current.getMinutes(), this.pointers_opt.minutes);
            this.renderPointer(this.pointers.hours, current.getHours(), this.pointers_opt.hours);
            this.setValue(current);
        }

        return this;
    };

    /**
     * @function renderPointer
     * @description Calls the render method of the provided Pointer instance
     * with the provided value.
     * @param pointer {Pointer} Instance of the Pointer widget that should be rendered.
     * @param value {Float} Value (between min and max configurations) to be rendered.
     * @param pointer_opt {Object} Object with pointer rotation configurations.
     * @memberof module:Clock
     * @instance
     * @private
     */
    Clock.prototype.renderPointer = function (pointer, value, pointer_opt) {

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
     * @memberof module:Clock
     * @instance
     * @private
     */
    Clock.prototype.value2deg = function (value, pointer_opt) {
        var rangePerc = (value - pointer_opt.min) / (pointer_opt.max - pointer_opt.min);
        var interpolatedOffset = rangePerc * (pointer_opt.max_degree - pointer_opt.min_degree);
        return pointer_opt.min_degree + interpolatedOffset;
    };

    /**
     * @function getDefaultStyleConfigs
     * @description Returns the default style configurations for the provided style identifier.
     * The possible styles for the Clock widget are clock, clock2, clock3 and clock4.
     * @param style_id {string} The style identifier.
     * @returns {Object} An object of configurations for the provided style identifier. The possible
     * configurations can be consulted in the documentation of the constructor method.
     * @memberof module:Clock
     * @instance
     */
    Clock.prototype.getDefaultStyleConfigs = function (style_id)
    {
        switch (style_id) {

            case 'clock':
                return {
                    panel_file: 'gauge-clock-panel-2.svg',
                    pointers: [
                        {
                            id: 'seconds',
                            top: 68,
                            left: 119,
                            width: 12,
                            style: 'gauge-pointer-19',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'minutes',
                            top: 109,
                            left: 120,
                            width: 11,
                            style: 'gauge-pointer-18',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'hours',
                            top: 106,
                            left: 118,
                            height: 60,
                            width: 14,
                            style: 'gauge-pointer-17',
                            min_degree: 180,
                            min: 0,
                            max_degree: 900,
                            max: 24,
                        }
                    ]
                };
            case 'clock2':
                return {
                    panel_file: 'gauge-clock-panel-1.svg',
                    pointers: [
                        {
                            id: 'seconds',
                            top: 68,
                            left: 119,
                            width: 12,
                            style: 'gauge-pointer-19',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'minutes',
                            top: 109,
                            left: 120,
                            width: 11,
                            style: 'gauge-pointer-18',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'hours',
                            top: 106,
                            left: 118,
                            height: 60,
                            width: 14,
                            style: 'gauge-pointer-17',
                            min_degree: 180,
                            min: 0,
                            max_degree: 900,
                            max: 24,
                        }
                    ]
                };
            case 'clock3':
                return {
                    panel_file: 'gauge-clock-panel-3.svg',
                    pointers: [
                        {
                            id: 'seconds',
                            top: 73,
                            left: 123,
                            width: 3,
                            style: 'gauge-pointer-19',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                            'z-index': 1,
                        },
                        {
                            id: 'minutes',
                            top: 109,
                            left: 120,
                            width: 11,
                            style: 'gauge-pointer-18',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                            'z-index': 2,
                        },
                        {
                            id: 'hours',
                            top: 106,
                            left: 118,
                            height: 60,
                            width: 14,
                            style: 'gauge-pointer-17',
                            min_degree: 180,
                            min: 0,
                            max_degree: 900,
                            max: 24,
                            'z-index': 2,
                        }
                    ]
                };
            case 'clock4':
                return {
                    panel_file: 'gauge-clock-panel-4.svg',
                    pointers: [
                        {
                            id: 'seconds',
                            top: 68,
                            left: 119,
                            width: 12,
                            style: 'gauge-pointer-19',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'minutes',
                            top: 109,
                            left: 120,
                            width: 11,
                            style: 'gauge-pointer-18',
                            min_degree: 180,
                            min: 0,
                            max_degree: 540,
                            max: 60,
                        },
                        {
                            id: 'hours',
                            top: 106,
                            left: 118,
                            height: 60,
                            width: 14,
                            style: 'gauge-pointer-17',
                            min_degree: 180,
                            min: 0,
                            max_degree: 900,
                            max: 24,
                        }
                    ]
                };

            default:
                return {
                    panel_file: style_id + '.svg',
                    pointers: []
                };
        }
    };

    module.exports = Clock;
});
