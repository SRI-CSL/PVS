/**
 * @module Widget
 * @desc Base widget implementation
 * @author Patrick Oladimeji
 * @date 10/30/13 22:59:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, _*/
define(function (require, exports, module) {
    "use strict";
    var property = require("util/property"),
        d3 = require("d3/d3");

    function getCoords(mark) {
        if (mark && mark.attr("coords")) {
            return mark.attr("coords");
        }
        var x = +mark.attr("x"), y = +mark.attr("y"), w = +mark.attr("width"), h = +mark.attr("height");
        return x + "," + y +	"," + (x + w) + "," + (y + h);
    }

    /**
        Creates a new Widget
        @constructor
        @param {string} id The id for the widget's html element
        @param {string} type The type of the widget "button or display"
        @this Widget
     */
    function Widget(id, type) {
        /**
            get or set the widget id.
            @type {string}
         */
        this.id = property.call(this, id);
        /**
            get or set the widget type
            @type {string}
         */
        this.type = property.call(this, type);
        /**
            get or set the widget element. This is a d3 selection object.
            @type {d3.selection}
         */
        this.element = property.call(this);
        /**
            get or set the image map area for this widget
            @type {d3.selection}
         */
        this.imageMap = property.call(this);
    }
    /**
     * Removes the widget from the interface
     * @memberof Widget
     */
    Widget.prototype.remove = function () {
        d3.select(this.parentGroup()).remove();
        if (this.imageMap()) {
            this.imageMap().remove();
        }
    };
    /**
     * Removes the widget from the interface
     * @memberof Widget
     */
    Widget.prototype.updateStyle = function (data) {
        // this function is overloaded by widgets supporting style attributes, e.g., displays
        // for widgets without style (e.g. buttons), the function does nothing
        return this;
    };
    /**
        Gets the <g> or group layer on which this widget is drawn
        @memberof Widget
     */
    Widget.prototype.parentGroup = function () {
        if (this.element()) {
            return this.element().node().parentNode;
        }
    };
    /**
        Gets whether an imageMap has been created and set for this widget. Usually called before {Widget#createImageMap}
        @returns {boolean}
        @memberof Widget
     */
    Widget.prototype.needsImageMap = function () {
        return !this.imageMap();
    };
    /**
        Creats and image map area for this widget using its position for the coords of the area.
        Adds the created area to the widget's imageMap property.
        @param {(Element|String)} opt.map Element to use as the image map
        @returns {object} the area element created
        @memberof Widget
     */
    Widget.prototype.createImageMap = function (opt) {
        opt = opt || {};
        opt.map = opt.map || d3.select("map#prototypeMap");
        var coords = getCoords(this.element());
        var widget = this, href = this.type() === "button" ? "#!" : null;
        var area = opt.map.append("area");
        area.attr("class", this.id())
            .attr("id", this.id())
            .attr("shape", "rect")
            .attr("coords", coords)
            .attr("href", href);
        widget.imageMap(area);
        return area;
    };

    /**
        Updates the current widget with the properties supplied
        @param {object} props A JSON object containing keys representing properties in the widget
        @returns {Widget} the updated widget
        @memberof Widget
     */
    Widget.prototype.updateWithProperties = function (props) {
        var w = this;
        _.each(props, function (val, key) {
            if (typeof w[key] === "function") {
                w[key](val);
            }
        });
        return this;
    };
    /**
        Updates the location and size of the widget to the new position
        @param {{x: number, y: number, width: number, height: number}} pos The new position  and size of the widget
        @returns {Widget}
        @memberof Widget
    */
    Widget.prototype.updateLocationAndSize = function (pos) {
        if (this.needsImageMap()) {
            this.createImageMap();
        }
        return this.imageMap().attr("coords", [pos.x, pos.y, pos.x + pos.width, pos.y + pos.height].join(","));
    };

    module.exports = Widget;
});
