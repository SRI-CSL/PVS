/**
 * @module Gearbox
 * @version 1.0.0
 * @author Henrique Pacheco
 * @desc This module is responsible for building Gearbox widget instances.
 *
 * @date July 23, 2017
 *
 * @example <caption>Usage of Gearbox within a PVSio-web project.</caption>
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // Require the Gearbox module
 *     require("widgets/car/Gearbox");
 *
 *     function main() {
 *          // After Gearbox module was loaded, initialize it
 *          var gearbox = new Gearbox(
 *               'example', // id of the element that will be created
 *               { top: 100, left: 100 }, // coordinates object
 *               { panel: 'gear-box-auto.svg',
 *                       stick: 'gear-stick.svg',
 *                       leftOffsets: {
 *                           'D': 0.345,
 *                           'N': 0.345,
 *                           'R': 0.345,
 *                           'P': 0.345,
 *                       },
 *                       topOffsets: {
 *                           'D': 0.55,
 *                           'N': 0.4,
 *                           'R': 0.25,
 *                           'P': 0,
 *                       }
 *               } // style can be 'auto', 'manual', 'manual2' or 'manual3'
 *           );
 *
 *          // Render the Gearbox widget - providing a new shift value.
 *          gearbox.render('D'); // for automatic gearboxes, 'P' 'N' 'R' or 'D'
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
     * @description Constructor for the Gearbox widget.
     * @param id {String} The id of the widget instance.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 256, height: 256 }.
     * @param opt {Object} Options:
     *          <li>parent (String): the HTML element where the display will be appended (default is "body").</li>
     *          <li>position (String): value for the CSS property position (default is "absolute").</li>
     *          <li>style (String): a valid style identifier (default is "auto").</li>
     *          <li>panel (String) - the path to the SVG panel file (in widgets/car/svg/gearbox directory).</li>
     *          <li>stick (String) - the path to the SVG stick file (in widgets/car/svg/gearbox directory).</li>
     *          <li>left_offsets (Object) - an object with the left coordinate offsets as percentage (float between 0 and 1) per each gear.</li>
     *          <li>top_offsets (Object) - an object with the top coordinate offsets as percentage (float between 0 and 1) per each gear.</li>
     * @returns {Gearbox} The created instance of the widget Gearbox.
     * @memberof module:Gearbox
     * @instance
     */
    function Gearbox(id, coords, opt) {
        SVGWidget.call(this, id, coords, opt);
        this.setId(id);

        // Ready to render control flag
        this.stickReady = false;
        this.panelReady = false;

        // Handle coords object
        coords = coords || {};
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 256;
        this.height = coords.height || 256;

        // Handle options and default values
        opt = opt || {};
        opt.position = opt.position || "absolute";
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";

        // Merge the provided opt with the default style configs
        this.opt = this.mergeConfigs(this.getDefaultStyleConfigs(opt.style || 'auto'), this.opt);

        // Create wrapper div
        this.buildWrapper();

        // Find panel file to load from style configs -- if any
        if(this.opt.panel !== undefined) {
            this.loadPanel();
        }

        // Find stick file to load from style configs -- if any
        if(this.opt.stick !== undefined) {
            this.loadStick();
        }

        return this;
    }

    Gearbox.prototype = Object.create(SVGWidget.prototype);
    Gearbox.prototype.constructor = Gearbox;
    Gearbox.prototype.parentClass = SVGWidget.prototype;


    /**
     * @function buildWrapper
     * @description Builds the wrapper for the gear box that will be displayed.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.buildWrapper = function() {
        // Create wrapper div
        this.wrapper = d3.select(this.parent)
            .append('div').attr('id', this.id)
            .style("position", 'absolute')
            .style("top", this.top + "px").style("left", this.left + "px")
            .style("width", this.width + "px").style("height", this.height + "px");
    };


    /**
     * @function loadPanel
     * @description Loads the SVG that will be used as a gearbox panel.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.loadPanel = function()
    {
        // Find the name of the file to load
        var file_to_require = "text!widgets/car/svg/gear-box/" + this.opt.panel;
        var self = this;
        require([file_to_require], function(file_required) {
            // Create the panel HTML element
            self.panel = self.wrapper
                .append('div').attr('id', self.id + '_panel')
                .style("position", 'absolute')
                .style("width", self.width + "px").style("height", self.height + "px")
                .html(file_required);

            // Set width and height and scale SVG accordingly
            var scale = Math.min(self.height, self.width) / 133;
            self.panel.select('svg')
                .style("transform", "scale(" + scale + "," + scale +")")
                .style("transform-origin", "0 0");

            // Report panel as ready
            self.reportPanelReady();
        });
    };


    /**
     * @function loadStick
     * @description Loads the SVG file that will be used as a gearbox stick.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.loadStick = function()
    {
        // Find the name of the file to load
        var file_to_require = "text!widgets/car/svg/gear-box/" + this.opt.stick;
        var self = this;
        require([file_to_require], function(file_required) {

            var widthRatio = self.opt.widthRatio || 0.33;
            var heightRatio = self.opt.heightRatio || 0.33;

            var stickWidth = (widthRatio * self.width);
            var stickHeight = (heightRatio * self.height);

            // Create the stick HTML element
            self.stick = self.wrapper
                .append('div').attr('id', self.id + '_stick')
                .style("width", stickWidth + "px").style("height", stickHeight + "px")
                .style("position", 'absolute')
                .style("-webkit-transition", "all 0.3s ease")
                .style("-moz-transition", "all 0.3s ease")
                .style("-ms-transition", "all 0.3s ease")
                .style("-o-transition", "all 0.3s ease")
                .style("transition", "all 0.3s ease")
                .style("z-index", "1")
                .html(file_required);

            // Get SVG's width and height as integer
            var svgHeight = parseFloat(self.stick.select('svg').style('height').replace('px', ''));
            var svgWidth = parseFloat(self.stick.select('svg').style('width').replace('px', ''));

            // Calc max deficit between width and height for the original div
            var widthDeficit = svgWidth - stickWidth;
            var heightDeficit = svgHeight - stickHeight;

            var ratio = (widthDeficit === heightDeficit || widthDeficit > heightDeficit) ?
                stickWidth / svgWidth : stickHeight / svgHeight;

            // Set transform origin attributes and scale the SVG elements
            self.stick.select('svg').style("transform-origin", "0 0").style('transform', 'scale('+ratio+')');

            // Set sticker initial positios
            self.setStickPosition('P');

            // Report stick as ready
            self.reportStickReady();
        });
    };


    /**
     * @function setSickPosition
     * @description Sets the gearbox stick in the position associated with the provided gear.
     * @param gear {String} The gear to set the position.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.setStickPosition = function(gear)
    {
        var leftPerc = this.getLeftOffset(gear);
        var topPerc = this.getTopOffset(gear);
        this.stick.style("top", topPerc * this.height + "px").style("left", leftPerc * this.width + "px");
    };

    /**
     * @function getLeftOffset
     * @description Returns the left offset as defined in the style configurations for the provided gear.
     * @param gear {String} The gear to set the position.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.getLeftOffset = function(gear)
    {
        return this.opt.leftOffsets[gear];
    };

    /**
     * @function getTopOffset
     * @description Returns the top offset as defined in the style configurations for the provided gear.
     * @param gear {String} The gear to set the position.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.getTopOffset = function(gear)
    {
        return this.opt.topOffsets[gear];
    };

    /**
     * @function reportPanelReady
     * @description Set panel as ready to render.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.reportPanelReady = function()
    {
        this.panelReady = true;
    };

    /**
     * @function reportStickReady
     * @description Set stick as ready to render.
     * @memberof module:Gearbox
     * @instance
     * @private
     */
    Gearbox.prototype.reportStickReady = function()
    {
        this.stickReady = true;
    };

    /**
     * @function isReady
     * @description Checks if the gearbox instance is ready to be rendered.
     * @memberof module:Gearbox
     * @instance
     */
    Gearbox.prototype.isReady = function()
    {
        return this.panelReady && this.stickReady;
    };

    /**
     * @function render
     * @description Render method of the Gearbox widget (if it is ready to render).
     * Re-renders the gearbox with the provided new value and configurations.
     * @param value {String} The new gear that will be set in the gearbox.
     * @param opt {Object} Override options when re-rendering. See constructor docs
     * for detailed docs on the available options.
     * @memberof module:Gearbox
     * @instance
     */
    Gearbox.prototype.render = function(value, opt)
    {
        if(this.isReady()) {
            this.setStickPosition(value);
            this.setValue(value);
        }

        return this;
    };


     /**
     * @function getDefaultStyleConfigs
     * @description Returns the default style configurations for the provided style identifier. The
     * possible styles for the Gearbox widget are the "auto", "manual", "manual2" and "manual3".
     * @param style_id {string} The style identifier.
     * @returns {Object} An object of configurations for the provided style identifier. The possible
     * configurations can be consulted in documentation of the constructor method.
     * @memberof module:Gearbox
     * @instance
     */
    Gearbox.prototype.getDefaultStyleConfigs = function (style_id)
    {
        switch (style_id) {
            case 'auto':
                return {
                    panel: 'gear-box-auto.svg',
                    stick: 'gear-stick.svg',
                    leftOffsets: {
                        1: 0.345,
                        2: 0.345,
                        3: 0.345,
                        4: 0.345,
                        5: 0.345,
                        6: 0.345,
                        'D': 0.345,
                        'N': 0.345,
                        'R': 0.345,
                        'P': 0.345,
                    },
                    topOffsets: {
                        1: 0.55,
                        2: 0.55,
                        3: 0.55,
                        4: 0.55,
                        5: 0.55,
                        6: 0.55,
                        'D': 0.55,
                        'N': 0.4,
                        'R': 0.25,
                        'P': 0,
                    }
                };

            case 'manual':
                return {
                    panel: 'gear-box-man.svg',
                    stick: 'gear-stick-1.svg',
                    leftOffsets: {
                        1: 0.1,
                        2: 0.1,
                        3: 0.2,
                        4: 0.2,
                        5: 0.35,
                        6: 0.35,
                        'D': 0.18,
                        'N': 0.18,
                        'R': 0,
                        'P': 0.18,
                    },
                    topOffsets: {
                        1: 0.08,
                        2: 0.4,
                        3: 0.08,
                        4: 0.4,
                        5: 0.08,
                        6: 0.4,
                        'D': 0.25,
                        'N': 0.25,
                        'R': 0.08,
                        'P': 0.25,
                    },
                    widthRatio: 0.5,
                    heightRatio: 0.5,
                };

            case 'manual2':
                return {
                    panel: 'gear-box-man.svg',
                    stick: 'gear-stick-2.svg',
                    leftOffsets: {
                        1: 0.1,
                        2: 0.1,
                        3: 0.18,
                        4: 0.18,
                        5: 0.35,
                        6: 0.35,
                        'D': 0.18,
                        'N': 0.18,
                        'R': 0.35,
                        'P': 0.18,
                    },
                    topOffsets: {
                        1: 0.08,
                        2: 0.4,
                        3: 0.08,
                        4: 0.4,
                        5: 0.08,
                        6: 0.08,
                        'D': 0.25,
                        'N': 0.25,
                        'R': 0.4,
                        'P': 0.25,
                    },
                    widthRatio: 0.5,
                    heightRatio: 0.5,
                };
            case 'manual3':
                return {
                    panel: 'gear-box-man.svg',
                    stick: 'gear-stick-3.svg',
                    leftOffsets: {
                        1: 0.1,
                        2: 0.18,
                        3: 0.18,
                        4: 0.35,
                        5: 0.35,
                        6: 0.35,
                        'D': 0.18,
                        'N': 0.18,
                        'R': 0.1,
                        'P': 0.18,
                    },
                    topOffsets: {
                        1: 0.4,
                        2: 0.08,
                        3: 0.4,
                        4: 0.08,
                        5: 0.4,
                        6: 0.4,
                        'D': 0.25,
                        'N': 0.25,
                        'R': 0.08,
                        'P': 0.25,
                    },
                    widthRatio: 0.5,
                    heightRatio: 0.5,
                };

            default:
                throw 'Style identifier ' + style_id + ' does not match a valid Pointer style.';
        }
    };

    module.exports = Gearbox;
});
