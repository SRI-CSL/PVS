/**
 * @module Gauge
 * @version 1.0.0
 * @description
 * Gauge renders a basic gauge object (using D3 Gauge Plus library). The gauge will be rendered with the
 * pointer showing the current value. The initial value is 0. The render method can
 * then be called, passing the new value as parameter, and the widget will update the gauge
 * to show the new provided value.
 *
 * @author Henrique Pacheco
 * @date Mar 25, 2017
 *
 * @example <caption>Example use of the widget.</caption>
 // Example pvsio-web demo that uses Gauge
 // The following configuration assumes the pvsio-web demo is stored in a folder within pvsio-web/examples/demo/
 require.config({
     baseUrl: "../../client/app",
     paths: {
         d3: "../lib/d3",
         lib: "../lib"
     }
 });
 require(["widgets/car/Gauge"], function (Gauge) {
     "use strict";
     var device = {};
     var speedGauge = new Gauge("speedGauge", {
        top: 100, left: 320, width: 220
      }, {
        style: 'classic',
        max: 360,
        majorTicks: 13,
        min: 0,
        size: 360,
        redZones: [],
        rotation: -45,
        gap:90,
        roundValueBeforeRender: true
    });
    speedGauge.render(10.5); // The gauge is rendered and the pointer indicates 10.5
});
 * });
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3_gauge_plus*/
define(function (require, exports, module) {
    "use strict";

    /**
     * @function <a name="Gauge">Gauge</a>
     * @description Gauge constructor.
     *
     * @param id {String} The ID of the element that will contain the gauge.
     * @param coords {Object} The four coordinates (top, left, width, height) of the display, specifying
     *        the left, top corner, and the width and height of the (rectangular) display.
     *        Default is { top: 0, left: 0, width: 200, height: 80 }.
     * @param opt {Object} Options:
     *        <li>drawOuterCircle - Draw a circle around the gauge. Defaults to false.</li>
     *        <li>gap - Defines the range of the gap between the beginning and the end of the gauge in degrees. Defaults to 90.</li>
     *        <li>greenColor - Color for the green zones of the gauge. Defaults to "#109618".</li>
     *        <li>greenZones - Green zones in the gauge (array of objects with from and to properties as values, defaults to []).</li>
     *        <li>initial - Initial value of the gauge. Defaults to 0.</li>
     *        <li>innerFillColor - Color of the inner circle. Defaults to "#000".</li>
     *        <li>innerStrokeColor - Color of the inner stroke. Defaults to "#fff".</li>
     *        <li>label - Label presented inside the gauge. Defaults to ''.</li>
     *        <li>labelColor - Color of the label. Defaults to "#888".</li>
     *        <li>labelSize - Font size of the label, as a percentage of the gauge radius. Defaults to 0.1.</li>
     *        <li>majorTicks - Number of major ticks to be drawn. Defaults to 9.</li>
     *        <li>majorTickColor - Color of the major ticks to be drawn. Defaults to "#fff".</li>
     *        <li>majorTickWidth - Width of the major ticks to be drawn. Defaults to "3px".</li>
     *        <li>max - Maximum value of the gauge. Defaults to 200.</li>
     *        <li>min - Minimum value of the gauge. Defaults to 0.</li>
     *        <li>minorTicks - Number of minor ticks to be drawn between each major tick. Defaults to 3.</li>
     *        <li>minorTickColor - Color of minor ticks to be drawn. Defaults to "#fff".</li>
     *        <li>minorTickWidth - Width of the minor ticks to be drawn. Defaults to "1px".</li>
     *        <li>outerFillColor - Color of the outer circle. Defaults to "#fff".</li>
     *        <li>outerStrokeColor - Color of the outer stroke. Defaults to "#fff".</li>
     *        <li>pointerFillColor - Color of the gauge pointer. Defaults to "#dc3912".</li>
     *        <li>pointerStrokeColor - Color of the stroke of the gauge pointer. Defaults to "#c63310".</li>
     *        <li>pointerUseBaseCircle - Draw a circle as base of the pointer. Defaults to false.</li>
     *        <li>pointerBaseCircleRadius - Radius of the base circle (as percentage of total radius, defaults to 0.1.</li>
     *        <li>pointerBaseCircleFillColor - Fill color of the base circle. Defaults to "#fff".</li>
     *        <li>pointerBaseCircleStrokeColor - Stroke color of the base circle. Defaults to "red".</li>
     *        <li>pointerBaseCircleStrokeWidth - Width of the stroke of the base circle. Defaults to "1px".</li>
     *        <li>redColor - Color for the red zones of the gauge. Defaults to "#e31406".</li>
     *        <li>redZones - Red zones in the gauge (array of objects with from and to properties as values, defaults to [{ from - (200 - (200 * 0.125)), to - 200 }]).</li>
     *        <li>rotation - Defines the orientation starting point of the gauge (value between 0 and 360). Defaults to 270.</li>
     *        <li>roundValueBeforeRender - Whether pointer value should be rounded before re-rendering the gauge. Defaults to false.</li>
     *        <li>size - Size of the gauge in pixels. Defaults to 400.</li>
     *        <li>style - Apply a default style to the rendered gauge (one of 'classic', 'sport', 'grey' or 'blue', default is 'classic'.</li>
     *        <li>transitionDuration - Duration of the pointer transition. Defaults to 500.</li>
     *        <li>yellowColor - Color for the yellow zones of the gauge. Defaults to "#FF9900".</li>
     *        <li>yellowZones - Yellow zones in the gauge (array of objects with from and to properties as values, defaults to []).</li>
     * @memberof module:Gauge
     * @instance
     */
    function Gauge(id, coords, opt) {
        opt = opt || {};

        // Handle coords
        coords = coords || {};
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 256;
        this.height = coords.height || 256;

        // Aux configurations and variables
        opt.position = opt.position || "absolute";
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";
        this.rendered = false;

        // Create the gauge element
        this.div = d3.select(this.parent)
            .append('div').attr('id', id)
            .style("position", opt.position)
            .style("top", this.top + "px").style("left", this.left + "px")
            .style("width", (this.width) + "px").style("height", (this.height) + "px");

        // D3 Gauge Plus object
        opt.size = (this.width > this.height) ? this.height : this.width;
        this.gauge_obj = this.createGauge(id, opt);

        return this;
    }

    Gauge.prototype.createGauge = function(id, opt) {

        // Get default configs
        var config = this.mergeConfigs({}, this.getDefaultConfigs());

        // Apply style configs if defined
        if(opt.style) {
            config = this.mergeConfigs(config, this.getStyleConfigs(opt.style));
        }

        // Merge the provided options
        config = this.mergeConfigs(config, opt);

        // Save configs for further usage
        this.config = config;

        // Return the created gauge plus pbject
        return new d3_gauge_plus.Gauge(id, config);
    };

    /**
     * @function <a name="render">render</a>
     * @description Render method.
     *
     * @param new_value {Float} The new value to set the gauge pointer.
     * @param opt {Object} Override options when re-rendering. See constructor docs for
     * detailed docs on the available options.
     *
     * @memberof module:Gauge
     * @instance
     */
    Gauge.prototype.render = function(new_value, opt) {

        if(!this.rendered) {
            // display the gauge
            this.gauge_obj.render();
            this.rendered = true;
        }

        // Check if value needs rounding before rendering
        var valueToRender = (this.config.roundValueBeforeRender) ? Math.round(new_value) : new_value;
        if (valueToRender >= 0) {
            this.gauge_obj.setPointer(valueToRender);
        }

        return this;
    };

    Gauge.prototype.remove = function () {
        Gauge.prototype.parentClass.remove.apply(this);
        this.div.remove();
        return this;
    };

    /**
     * @function <a name="hide">hide</a>
     * @description Hides the widget
     * @memberof module:Gauge
     * @instance
     */
    Gauge.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    /**
     * @function <a name="reveal">reveal</a>
     * @description Hides the widget
     * @memberof module:Gauge
     * @instance
     */
    Gauge.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    /**
     * @function <a name="move">move</a>
     * @description Changes the position of the widget according to the coordinates given as parameter.
     * @param data {Object} Coordinates indicating the new position of the widget. The coordinates are given in the form { top: (number), left: (number) }
     * @memberof module:Gauge
     * @instance
     */
     Gauge.prototype.move = function (data) {
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

    Gauge.prototype.renderSample = function (opt) {
        // opt = opt || {};
        // var config = this.mergeConfigs(this.getDefaultConfigs(), opt);
        // var gaugeObj = this.createGauge('sample', config);
        // return this.render('sample', config);
        return this;
    };

    Gauge.prototype.getDefaultConfigs = function() {
        return {
            size: 400,
            rotation: 270,
            gap: 90,
            drawOuterCircle: false,
            outerStrokeColor: "#fff",
            outerFillColor: "#fff",
            innerStrokeColor: "#fff",
            innerFillColor: "#000",
            label: '',
            labelSize: 0.1, // Default font size is 10% of radius.
            labelColor: "#888",
            min: 0,
            max: 200,
            initial: 0,
            clampUnderflow: false,
            clampOverflow: false,
            majorTicks: 9,
            majorTickColor: "#fff",
            majorTickWidth: "3px",
            minorTicks: 3,
            minorTickColor: "#fff",
            minorTickWidth: "1px",
            greenColor: "#109618",
            yellowColor: "#FF9900",
            redColor: "#e31406",

            // Added settings
            pointerFillColor: "#dc3912",
            pointerStrokeColor: "#c63310",
            pointerUseBaseCircle: false,
            // Percentage of total radius
            pointerBaseCircleRadius: 0.1,
            pointerBaseCircleFillColor: "#fff",
            pointerBaseCircleStrokeColor: "red",
            pointerBaseCircleStrokeWidth: "1px",

            transitionDuration: 500,
            greenZones: [ ],
            yellowZones: [ ],
            redZones: [ { from: (200 - (200 * 0.125)), to: 200 } ],

            roundValueBeforeRender: false,
            style: 'classic',
        };
    };


    Gauge.prototype.getStyleConfigs = function(style_identifier) {
        var styles = {
            classic: {
                outerStrokeColor: "#fff",
                outerFillColor: "#fff",
                innerStrokeColor: "#fff",
                innerFillColor: "#2c2b30",
                majorTickColor: "#fff",
                majorTickWidth: "3px",
                minorTickColor: "#fff",
                minorTickWidth: "1px",
                greenColor: "#109618",
                yellowColor: "#FF9900",
                redColor: "#e31406",
                pointerFillColor: "#e93947",
                pointerStrokeColor: "#150507",
                pointerOpacity: 1,
                pointerShowLabel: false,
                pointerUseBaseCircle: true,
                pointerBaseCircleRadius: 0.1,
                pointerBaseCircleFillColor: "#65686d",
                pointerBaseCircleAbovePointer: true,
                pointerBaseCircleStrokeColor: "#000",
                pointerBaseCircleStrokeWidth: "2px",
            },
            sport: {
                innerFillColor: "#2c2b30",
                pointerFillColor: "#a2302d",
                pointerStrokeColor: "#e2d9df",
                pointerShowLabel: false,
                pointerUseBaseCircle: true,
                pointerBaseCircleFillColor: "#131418",
                pointerBaseCircleStrokeColor: "#131418",
                pointerBaseCircleRadius: 0.15
            },
            grey: {
                drawOuterCircle: true,
                outerStrokeColor: "#838286",
                outerFillColor: "#838286",
                innerStrokeColor: "888",
                innerFillColor: "#fff",
                majorTickColor: "#000",
                majorTickWidth: "2px",
                minorTickColor: "#000",
                pointerFillColor: "#dc555a",
                pointerStrokeColor: "#6f6e73",
                pointerShowLabel: false,
                pointerUseBaseCircle: true,
                pointerBaseCircleAbovePointer: true,
                pointerBaseCircleFillColor: "#838286",
                pointerBaseCircleStrokeColor: "#838286",
                pointerBaseCircleRadius: 0.2,
            },
            blue: {
                outerStrokeColor: "#599bcf",
                outerFillColor: "#599bcf",
                innerStrokeColor: "#599bcf",
                innerFillColor: "#599bcf",
                majorTickColor: "#000",
                majorTickWidth: "2px",
                minorTickColor: "#000",
                labelColor: "#000",
                pointerFillColor: "#290107",
                pointerStrokeColor: "#290107",
                pointerShowLabel: false,
                pointerUseBaseCircle: true,
                pointerBaseCircleFillColor: "#3f4552",
                pointerBaseCircleStrokeColor: "#3f4552",
                pointerBaseCircleRadius: 0.2,
            }
        };

        return (styles.hasOwnProperty(style_identifier)) ? styles[style_identifier] : {};
    };


    Gauge.prototype.mergeConfigs = function(conf1, conf2) {
        // Second conf provided overrides the first one
        for (var attr in conf2) { conf1[attr] = conf2[attr]; }
        return conf1;
    };

    module.exports = Gauge;
});
