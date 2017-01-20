/**
 * @module PIMWidget
 * @desc Widget within the PIM prototype editor
 * @author Nathaniel Watson
 */
define(function (require, exports, module) {
    "use strict";
    var Widget = require("widgets/Widget"),
        property = require("util/property");

    var PIMWidget = function (id, coords, opt) {
        opt = opt || {};
        opt.evts = ["click"];
        coords = coords || {};
        this.imageMap = property.call(this);

        Widget.call(this, id, "pim-button");

        this.y = coords.top || 0;
        this.x = coords.left || 0;
        this.width = coords.width || 32;
        this.height = coords.height || 32;

        this.name = property.call(this, opt.name || "New widget");
        this.targetScreen = property.call(this, opt.targetScreen);

        return this;
    };


    PIMWidget.prototype = Object.create(Widget.prototype);
    PIMWidget.prototype.constructor = PIMWidget;
    PIMWidget.prototype.parentClass = Widget.prototype;

    /**
     * Returns a JSON object representation of this Widget.
     * @returns {object}
     * @memberof PIMWidget
    */
    PIMWidget.prototype.toJSON = function () {
        var targetScreen = this.targetScreen();
        var targetId;

        if (targetScreen != null) {
            targetId = targetScreen.id;
        }

        return {
            id: this.id(),
            type: this.type(),
            name: this.name(),
            targetScreen: targetId,
            coords: {
                x: this.x,
                y: this.y,
                width: this.width,
                height: this.height
            }
        };
    };

    /**
     * Removes the widget's image map from the DOM (but does not remove the widget's reference to it)
     */
    PIMWidget.prototype.removeImageMap = function () {
        if (this.imageMap()) {
            this.imageMap().remove();
            // FIXME! duplicated area maps are erroneously created when imagemapper redraws the areas to pull the selected region on top.
            if (d3.select(".image-map-layer").select("#" + this.imageMap().node().id).node()) {
                d3.select(".image-map-layer").select("#" + this.imageMap().node().id).node().parentNode.remove();
            }
        }
    };

    /**
     * Returns an object containing the x, y, width and height properties of the widget.
     * @returns {object}
     * @memberof PIMWidget
    */
    PIMWidget.prototype.getCoords = function () {
        return {
            x: this.x,
            y: this.y,
            width: this.width,
            height: this.height
        };
    };

    /**
     * @override
     * @function createImageMap
     * @description Creates an image map area for this widget, which is used by the simulator mode
     * @param {function} opt.onCLick Callback to call when the widget is clicked in the simulator
     * @returns {d3.selection} The image map area created for the widget
     * @memberof PIMWidget
     */
    PIMWidget.prototype.createImageMap = function (opt) {
        opt = opt || {};

        var area = opt.area || PIMWidget.prototype.parentClass.createImageMap.apply(this, arguments);
        var _this = this;

        area.on("mousedown", function (e) {
            opt.onClick(_this, e);
        });

        this.imageMap(area);
        return area;
    };

    PIMWidget.prototype.updateLocationAndSize = function (pos) {
        this.x = pos.x;
        this.y = pos.y;
        this.width = pos.width;
        this.height = pos.height;

        PIMWidget.prototype.parentClass.updateLocationAndSize.apply(this, arguments);
    };

    /**
     * Updates the location and size of the widget's image map to the given location.
     * This does not change the stored position of the widget itself, only the image map area associated with the widget.
     * This should be used for presentation/view-level changes such as the prototype being zoomed.
     * @param {object} pos x, y, width, height for the widget's image map area
     */
    PIMWidget.prototype.updateImageMapLocationAndSize = function (pos) {
        this.imageMap().attr("coords", [pos.x, pos.y, pos.x + pos.width, pos.y + pos.height].join(","));
    };

    /**
     *
     * Creates a new widget with the same attributes as the current widget
     * @return {PIMWidget} The copy of the widget
     */
    PIMWidget.prototype.duplicate = function () {
        var widget = new PIMWidget(this.id(), {
            top: this.y,
            left: this.x,
            width: this.width,
            height: this.height
        }, {
            name: this.name()
        });

        widget.targetScreen(this.targetScreen());

        return widget;
    };

    /**
     * Creates a new widget from the data in the provided object.
     * @param {object} jsonObj JSON-style object with the data for the widget
     * @param {ScreenCollection} screens Collection of screens that the widget's targetScreen is contained within
     * @return {Widget} The new widget
     */
    PIMWidget.initFromJSON = function (jsonObj, screens) {
        var widget = new PIMWidget(jsonObj.id, {
            top: jsonObj.coords.y,
            left: jsonObj.coords.x,
            width: jsonObj.coords.width,
            height: jsonObj.coords.height
        }, {
            name: jsonObj.name
        });

        if (jsonObj.targetScreen) {
            widget.targetScreen(screens.get(jsonObj.targetScreen));
        }

        return widget;
    };

    module.exports = PIMWidget;
});
