/**
 * Edit widget
 * @author Patrick Oladimeji, Paolo Masci
 * @date 11/5/13 13:16:05 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Handlebars, $*/
define(function (require, exports, module) {
    "use strict";
    var FormUtils		= require("./FormUtils"),
        template		= require("text!./templates/editWidget.handlebars"),
        BaseDialog      = require("pvsioweb/forms/BaseDialog"),
        widgetPreviewer = require("pvsioweb/forms/widgetPreviewer"),
        d3				= require("d3/d3");

    function getWidgetEvents(widgetType) {
        var evts = [];
        d3.select("#events").selectAll("input[type='radio']").each(function () {
            if (this.checked) { evts = evts.concat(this.value.split("/")); }
        });
        return evts;
    }
    function updateBoundFunctionsLabel(widgetType) {
        if (d3.select("#custom_event").node()) {
            if (d3.select("#custom_event").node().checked) {
                d3.select("#boundFunctions").attr("readonly", null);
            }
        }
        if (d3.select("#functionText").node()) {
            var f = d3.select("#functionText").property("value"),
                str = "",
                evts = [];
            evts = getWidgetEvents(widgetType);
            str = evts.map(function (d) {
                return d + "_" + f;
            }).join(", ");
            document.getElementById("boundFunctions").value = str;
            d3.select("#boundFunctions").attr("readonly", true);
        }
    }
    function updateTimerEvent() {
        var f = d3.select("#timerEvent").property("value");
        d3.select("#timerFunction").text(f);
    }
    function showWidgetPreview(widgetType) {
        if (widgetType === "button") {
            widgetPreviewer.preview(widgetType, {
                keyboardKey: d3.select("#keyCode").node().value.trim(),
                buttonReadback: d3.select("#buttonReadback").node().value.trim()
            });
        } else if (widgetType === "display") {
            widgetPreviewer.preview(widgetType, {
                auditoryFeedback: d3.select("#auditoryFeedback").node().checked,
                fontsize: d3.select("#fontsize").node().value.trim(),
                fontColor: d3.select("#fontColor").node().value.trim(),
                backgroundColor: d3.select("#backgroundColor").node().value.trim()
            });
        } else if (widgetType === "numericdisplay") {
            widgetPreviewer.preview(widgetType, {
                auditoryFeedback: d3.select("#auditoryFeedback").node().checked,
                fontsize: d3.select("#fontsize").node().value.trim(),
                fontColor: d3.select("#fontColor").node().value.trim(),
                backgroundColor: d3.select("#backgroundColor").node().value.trim()
            });
        } else if (widgetType === "touchscreenbutton") {
            widgetPreviewer.preview(widgetType, {
                buttonReadback: d3.select("#buttonReadback").node().value.trim(),
                fontsize: d3.select("#fontsize").node().value.trim(),
                fontColor: d3.select("#fontColor").node().value.trim(),
                backgroundColor: d3.select("#backgroundColor").node().value.trim()
            });
        } else if (widgetType === "touchscreendisplay") {
            widgetPreviewer.preview(widgetType, {
                auditoryFeedback: d3.select("#auditoryFeedback").node().checked,
                cursorName: d3.select("#cursorName").node().value.trim(),
                fontsize: d3.select("#fontsize").node().value.trim(),
                fontColor: d3.select("#fontColor").node().value.trim(),
                backgroundColor: d3.select("#backgroundColor").node().value.trim()
            });
        } else if (widgetType === "led") {
            var color = d3.select("#ledColor").node().value.trim();
            widgetPreviewer.preview(widgetType, {
                color: color
            });
        }
    }

    var EditWidgetView	= BaseDialog.extend({
        render: function (widget) {
            var t = Handlebars.compile(template);
            var widgetData = widget.toJSON();
            widgetData.isButton = widget.type() === "button";
            widgetData.isDisplay = widget.type() === "display";
            widgetData.isNumericDisplay = widget.type() === "numericdisplay";
            widgetData.isTouchscreenDisplay = widget.type() === "touchscreendisplay";
            widgetData.isTouchscreenButton = widget.type() === "touchscreenbutton";
            widgetData.isLED = widget.type() === "led";
            widgetData.isTimer = widget.type() === "timer";
            this.$el.html(t(widgetData));
            $("body").append(this.el);
            this.widget = widget;

            //update form
            if (widgetData.isButton || widgetData.isTouchscreenButton) {
                if (widget.customFunctionText && typeof widget.customFunctionText === "function" && widget.customFunctionText()) {
                    d3.select("#custom_event").attr("checked", true);
                    document.getElementById("boundFunctions").value = widget.customFunctionText();
                } else {
                    widget.evts().forEach(function (e) {
                        d3.select("input[type='radio'][value='" + e + "']").property("checked", true);
                    });
                }
                updateBoundFunctionsLabel(widget.type());
            }
            if (widget.auditoryFeedback && widget.auditoryFeedback() === "enabled") {
                d3.select("input[type='checkbox'][name='auditoryFeedback']").property("checked", true);
            }
            showWidgetPreview(widget.type());
            return this;
        },
        events: {
            "click #btnOk"                : "ok",
            "click #btnCancel"            : "cancel",
            "change input[type='radio'][name='button_events']"           : "eventsChanged",
            "change input[type='radio'][name='touchscreenbutton_events']": "eventsChanged",
            "change input[type='checkbox']": "updatePreview",
            "input #functionText"         : "eventsChanged",
            "input #timerEvent"           : "timerEventChanged",
            "input #buttonReadback"       : "updatePreview",
            "input #ledColor"             : "updatePreview",
            "input #fontsize"             : "updatePreview",
            "input #fontColor"            : "updatePreview",
            "input #backgroundColor"      : "updatePreview",
            "input #cursorName"           : "updatePreview"
        },
        eventsChanged: function (event) {
            updateBoundFunctionsLabel(this.widget.type());
            showWidgetPreview(this.widget.type());
        },
        updatePreview: function (event) {
            showWidgetPreview(this.widget.type());
        },
        timerEventChanged: function (event) {
            updateTimerEvent(this.widget.type());
        },
        ok: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var formdata = FormUtils.serializeForm(form, "input");
                // update auditory feedback and touchscreen properties if the properties are supported by the widget
                if (this.widget.auditoryFeedback) {
                    formdata.auditoryFeedback = (d3.select("input[type='checkbox'][name='auditoryFeedback']").property("checked")) ? "enabled" : "disabled";
                }
                if (formdata.button_events) {
                    formdata.evts = formdata.button_events;
                    delete formdata.button_events;
                    if (formdata.evts[0] === "custom") {
                        formdata.customFunctionText = document.getElementById("boundFunctions").value;
                    } else {
                        formdata.customFunctionText = null;
                    }
                }
                // trigger event
                this.trigger("ok", { data: formdata, el: this.el, event: event }, this);
            }
        },
        cancel: function (event) {
            this.trigger("cancel", {el: this.el, event: event}, this);
        }
    });

    module.exports = {
        create: function (widget) {
            return new EditWidgetView(widget);
        }
    };
});
