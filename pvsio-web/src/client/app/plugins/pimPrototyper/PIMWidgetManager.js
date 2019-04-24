/* global _ */

/**
* @module PIMWidgetManager
* @description Compatibility 'layer' that allows classes from the original prototype builder to interact with a PIM
* screen
* @author Nathaniel Watson
*/
define(function (require, exports, module) {
    var BaseWidgetManager = require("pvsioweb/BaseWidgetManager"),
        PIMWidget  = require("./PIMWidget"),
        uidGenerator = require("util/uuidGenerator");

    var PIMWidgetManager = function() {
        this._widgets = {};
    };

    PIMWidgetManager.prototype = Object.create(BaseWidgetManager.prototype);

    /**
     * Updates the widget manager to use the provided screen
     * @param {Screen} scrn Prototype screen whose widgets should be managed by this manager
     * @param {function} widgetClickHandler Function to call when any restored widget is clicked
     * @param {Element} widgetImageMap Image map element that any widget's area maps should be added to
     */
    PIMWidgetManager.prototype.setScreen = function (scrn, widgetClickHandler, widgetImageMap) {
        _.each(this._widgets, function(widget) {
            widget.removeImageMap();
        });

        var _this = this;
        this._screen = scrn;

        if (this._screen != null && this._screen.get("widgets") != null) {
            this._widgets = this._screen.get("widgets");

            _.each(this._widgets, function(widget) {
                _this.trigger("WidgetRegionRestored", widget, widget.getCoords());

                if (widget.needsImageMap()) {
                    widget.createImageMap({
                        onClick: widgetClickHandler,
                        map: widgetImageMap
                    });
                }
                if (widget.image) {
                    widget.createWidgetImage();
                }
            });
        } else {
            this._widgets = {};
        }
    };

    /**
     * Creates a new widget and adds it to the WidgetManager and underlying PIM screen
     * @param {object} data Data object from a NewWidgetView callback
     * @param {object} coord Object with top, left, width, height properties specifying the widget position
     * @param {function} onCreate Called once the widget has been created, but before it is added to the manager
     * @return {Widget} The new widget
     */
    PIMWidgetManager.prototype.addNewWidget = function (data, coord, onCreate) {
        var id = "pimwidget_" + uidGenerator();

        var widget = new PIMWidget(id, coord, data);

        if (onCreate) {
            onCreate(widget);
        }

        widget.updateWithProperties(data);
        this.addWidget(widget);
        this.trigger("WidgetModified", {action: "create", widget: widget});

        return widget;
    };

    /**
     * update all the area maps attributed to all widgets on the project by the given scale factor.
     * For PIM prototypes this only affects the displayed scale, not the scale of the position values held by each width
     * @param {Number} scale the scale to transform the maps by
     */
    PIMWidgetManager.prototype.scaleAreaMaps = function (scale) {
        var _this = this;
        var widgets = _this.getAllWidgets();

        if (widgets) {
            widgets.forEach(function (w) {
                w.updateImageMapLocationAndSize({
                    x: w.x * scale,
                    y: w.y * scale,
                    width: w.width * scale,
                    height: w.height * scale
                });
            });
        }
    };

    return PIMWidgetManager;
});
