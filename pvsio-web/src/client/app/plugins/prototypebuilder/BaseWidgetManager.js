/**
 * @module BaseWidgetManager
 * @description
 * @author Nathaniel Watson
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, _ */
define(function (require, exports, module) {
    var d3 = require("d3/d3");

    function WidgetManager() {}

    _.extend(WidgetManager.prototype, Backbone.Events);

    /**
        Gets the widget with the specified id.
        @param {string} id The html element id of the widget
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.getWidget = function (id) {
        return this._widgets[id];
    };

    /**
        Adds the specified widget to the list of widgets.
        @param {Widget} widget The widget to be added.
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.addWidget = function (widget) {
        this._widgets[widget.id()] = widget;
    };

    /**
        Removes the specified widget from the list of widgets.
        @param {Widget} widget The widget to remove.
        @memberof module:WidgetManager
     */
    WidgetManager.prototype.removeWidget = function (widget) {
        widget.remove();
        delete this._widgets[widget.id()];
    };

    /**
        Gets a list of all the widgets loaded on the page. The returned array contains all
        widget types
        @returns {Widget[]}
        @memberof WidgetManager
    */

    WidgetManager.prototype.getAllWidgets = function () {
        return d3.values(this._widgets);
    };

    /**
        Update  the location of the widget by updating the image map coords to the position given.
        @param {Widget} widget The widget to update
        @param {{x: number, y: number, width: number, height: number}} pos The new position and size
        @param {Number?} scale a scale factor for the pos value. If not supplied defaults to 1
        @memberof WidgetManager
     */
    WidgetManager.prototype.updateLocationAndSize = function (widget, pos, scale) {
        scale = scale || 1;
        if (typeof widget === "string") { widget = this.getWidget(widget); }
        if (widget) {
            pos.x *= scale;
            pos.y *= scale;
            pos.width *= scale;
            pos.height *= scale;
            widget.updateLocationAndSize(pos, { imageMap: true, visibleWhen: "true" });
        }
    };

    /**
        update all the area maps attributed to all widgets on the project by the given scale factor
        @param {Number} scale the scale to transform the maps by
    */
    WidgetManager.prototype.scaleAreaMaps = function (scale) {
        var _this = this;
        var widgets = _this.getAllWidgets();
        function _getPos(el) {
            return {x: el.attr("x"), y: el.attr("y"), height: el.attr("height"), width: el.attr("width")};
        }

        if (widgets) {
            widgets.forEach(function (w) {
                if (w.element()) {
                    var pos = _getPos(w.element());
                    _this.updateLocationAndSize(w, pos, scale);
                }
            });
        }
    };

    module.exports = WidgetManager;
});
