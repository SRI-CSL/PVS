/**
 * @module CentralPanel
 * @version 1.0.0
 * @description
 * CentralPanel renders a basic car dashboard object, including compenents like the car absolute speed, odometer,
 * engine and environment temperatures, the current clock time, etc..
 *
 * @author Henrique Pacheco
 * @date Apr 14, 2017
 *
 * @example <caption>Usage of CentralPanel within a PVSio-web project.</caption>
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // Require the CentralPanel module
 *     require("widgets/car/CentralPanel");
 *
 *     function main() {
 *          // After CentralPanel module was loaded, initialize it
 *          var centralPanel = new CentralPanel('central-panel');
 *
 *          // Re-render the CentralPanel, provinding new values
 *          centralPanel.render({ speed: 200 });
 *     }
 * });
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var BasicDisplay = require("widgets/BasicDisplay");

    /**
     * @function constructor
     * @description CentralPanel constructor.
     *
     * @param id {String} The ID of the element that will contain the central panel.
     * @param opt {Object} Options:
     *          <li>backgroundColor (String): value for the CSS property background-color (default is "#000").</li>
     * @returns {CentralPanel} The created instance of the widget CentralPanel.
     * @memberof module:CentralPanel
     * @instance
     */
    function CentralPanel(id, coords, opt) {

        opt = opt || {};
        this.backgroundColor = opt.backgroundColor || "#000";

        // Handle coords
        coords = coords || {};
        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 200;
        this.height = coords.height || 80;

        // Aux configurations and variables
        opt.position = opt.position || "absolute";
        this.parent = (opt.parent) ? ("#" + opt.parent) : "body";

        // Create the gauge element
        this.div = d3.select(this.parent)
            .append('div').attr('id', id)
            .style("position", opt.position)
            .style("top", this.top + "px").style("left", this.left + "px")
            .style("width", (this.width) + "px").style("height", (this.height) + "px")
            .style("background-color", this.backgroundColor);

        // ---------------- CURRENT GEAR ---------------------------
        this.gearDisplay = new BasicDisplay(
            'current-gear',
            { top: 3, left: 160, width: 24, height: 26 },
            {
                borderWidth: 2,
                borderStyle: "solid",
                borderColor: "white",
                parent: id,
                fontsize: 22,
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- CLOCK ----------------------------------
        this.clockDisplay = new BasicDisplay(
            'clock',
            { top: 170, left: 5, width: 70, height: 18 },
            {
                fontsize: 18,
                parent: id,
                backgroundColor: this.backgroundColor
            }
        );

        // Separator
        this.separator1 = new BasicDisplay(
            'separator1',
            { top: 33, left: 0, width: 230, height: 1 },
            {
                fontsize: 18,
                parent: id,
                borderWidth: 1,
                borderStyle: "solid",
                borderColor: "white",
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- ENVIRONMENT TEMPERATURE ----------------
        this.envTempDisplay = new BasicDisplay(
            'env-temp',
            { top: 170, left: 155, width: 70, height: 18 },
            {
                fontsize: 18,
                parent: id,
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- SPEED ABS VALUE ------------------------
        this.speedAbsDisplay = new BasicDisplay(
            'speed-abs',
            { top: 40, left: 55, width: 60, height: 32 },
            {
                fontsize: 32,
                align: "right",
                parent: id,
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- SPEED UNIT DISPLAY ---------------------
        this.speedUnitDisplay = new BasicDisplay(
            'speed-unit-display',
            { top: 51, left: 118, width: 70, height: 20 },
            {
                fontsize: 20,
                align: "left",
                parent: id,
                backgroundColor: this.backgroundColor
            }
        );

        // Separator
        this.separator2 = new BasicDisplay(
            'separator2',
            { top: 73, left: 40, width: 150, height: 1 },
            {
                fontsize: 18,
                parent: id,
                borderWidth: 1,
                borderStyle: "solid",
                borderColor: "white",
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- ODOMETER -------------------------------
        this.odometerDisplay = new BasicDisplay(
            'odometer',
            { top: 4, left: 40, width: 110, height: 25 },
            {
                fontsize: 25,
                parent: id,
                backgroundColor: this.backgroundColor
            }
        );

        // ---------------- ENGINE TEMPERATURE INDICATORS ----------
        this.engineTemp1 = new BasicDisplay(
            'eng-temp-1',
            { top: 80, left: 107, width: 60, height: 18 },
            {
                fontsize: 18,
                parent: id,
                align: "right",
                backgroundColor: this.backgroundColor
            }
        );

        this.engineTemp2 = new BasicDisplay(
            'eng-temp-2',
            { top: 102, left: 107, width: 60, height: 18 },
            {
                fontsize: 18,
                parent: id,
                align: "right",
                backgroundColor: this.backgroundColor
            }
        );

        this.engineTemp3 = new BasicDisplay(
            'eng-temp-3',
            { top: 124, left: 107, width: 60, height: 18 },
            {
                fontsize: 18,
                parent: id,
                align: "right",
                backgroundColor: this.backgroundColor
            }
        );

        return this;
    }

    /**
     * @function <a name="CentralPanel">CentralPanel</a>
     * @description Render method.
     *
     * @param new_value {object} An object with the new values
     * @param opt {Object} Override options when re-rendering. See constructor docs for
     * detailed docs on the available options.
     *
     * @memberof module:CentralPanel
     * @instance
     */
    CentralPanel.prototype.render = function(newValues, opt) {

        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            var ans = (v < 100) ? v.toFixed(1).toString() : v.toFixed(0).toString();
            return parseFloat(ans);
        }

        // Parse the current gear from the string provided by the current model state
        function parseGear(gear_str) {
            // String contains
            if(gear_str.indexOf('GEAR_') !== -1) {
                // String replace
                return gear_str.replace('GEAR_','');
            } else {
                return gear_str;
            }
        }

        // Left zero padding for the odometer value
        function parseOdoValue(num) {
            var s = Math.round(num)+"";
            while (s.length < 7) {
                s = "0" + s;
            }
            return s;
        }

        // Left zero pad for hour/minute display
        function addLeadingZero(i) {
            if (i < 10) {
                i = "0" + i;
            }
            return i;
        }

        opt = opt || {};

        var temperature = Math.round(evaluate(newValues.temp.val)) + ' ' + ((newValues.temp.units === "C") ? "°C" : "°F");

        this.gearDisplay.render(parseGear(newValues.gear));
        this.clockDisplay.render(addLeadingZero(newValues.time.hour) + ':' + addLeadingZero(newValues.time.min));
        this.envTempDisplay.render(temperature);
        this.speedAbsDisplay.render(Math.round(evaluate(newValues.speed.val)).toString());
        this.speedUnitDisplay.render((newValues.speed.units === "kph") ? "km/h" : "mph");
        this.odometerDisplay.render(parseOdoValue(evaluate(newValues.odo)));

        // Fake stuff (for now)
        this.engineTemp1.render(temperature);
        this.engineTemp2.render(temperature);
        this.engineTemp3.render(temperature);

        // Separators
        this.separator1.render('');
        this.separator2.render('');

        return this;
    };


    CentralPanel.prototype.remove = function () {
        CentralPanel.prototype.parentClass.remove.apply(this);
        this.div.remove();
        return this;
    };

    CentralPanel.prototype.hide = function () {
        this.div.style("display", "none");
        return this;
    };

    CentralPanel.prototype.reveal = function () {
        this.div.style("display", "block");
        return this;
    };

    CentralPanel.prototype.move = function (data) {
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

    CentralPanel.prototype.renderSample = function (opt) {
        opt = opt || {};
        var widget = new CentralPanel('sample', {}, opt);
        return widget.render('sample', opt);
    };

    module.exports = CentralPanel;
});
