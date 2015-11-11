/**
 * Base module for dialog boxes. This module implements several default functions for dialog boxes.
 * 1. It ensures that all dialog boxes inherited from it are draggable
 * 2. It implements default behaviours for canceling and accepting a form using Esc and Enter  respectively
 * 3. It appends events registered in subclasses to the base events list and where functions are redefined, those functions override the ones defined in this module
 * @author Patrick Oladimeji
 * @date 11/6/14 9:10:56 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        FormUtils = require("pvsioweb/forms/FormUtils");

    var BaseDialog = Backbone.View.extend({
        initialize: function (data) {
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
            this.render(data);
            this._data = data;
            this.focus();
        },
        events: {
            "keydown .panel": "keypress",
            "mousedown .panel-heading": "moveDialog"
        },
        focus: function () {
            d3.select(this.el).select(".panel").attr("tabindex", 1).node().focus();
        },
        moveDialog: function (event) {
            var parent = this.el,
                bbox = d3.select(parent).select(".panel").node().getBoundingClientRect();
            var startx = bbox.left, starty = bbox.top,
                mx = event.clientX, my = event.clientY;
            function mousemove() {
                var e = d3.event;
                d3.select(parent).select(".panel")
                    .style("top", (starty + e.clientY - my) + "px")
                    .style("left", (startx + e.clientX - mx) + "px")
                    .style("position", "absolute");
            }

            d3.select("body").on("mousemove.dialogdrag", mousemove);
            d3.select(parent).select(".panel-heading").on("mouseup", function () {
                d3.select("body").on("mousemove.dialogdrag", null);
            });
        },
        keypress: function (event) {
            switch (event.which) {
            case 13: //enter pressed
                this.ok(event);
                break;
            case 27: //esc pressed
                this.cancel(event);
                break;
            default:
                break;
            }
        },
        ok: function (event) {
            var form = this.el,
                btnName = this._data && this._data.buttons ? this._data.buttons[1].toLowerCase() : "ok";
            if (FormUtils.validateForm(form)) {
                var formdata = FormUtils.serializeForm(form);
                this.trigger(btnName, {data: formdata, el: this.el, event: event}, this);
            }
        },
        cancel: function (event) {
            var btnName = this._data && this._data.buttons ? this._data.buttons[0].toLowerCase() : "cancel";

            this.trigger(btnName, {el: this.el, event: event}, this);
        }
    });

    var baseExtend = BaseDialog.extend;

    BaseDialog.extend = function (data) {
        if (data && data.events) {
            Object.keys(data.events).forEach(function (e) {
                BaseDialog.prototype.events[e] = data.events[e];
            });
            delete data.events;
        }
        return baseExtend.call(BaseDialog, data);
    };

    module.exports = BaseDialog;
});
