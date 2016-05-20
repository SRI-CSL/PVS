/**
 * Create the widgets form using backbonejs and handlebars
 * @author Patrick Oladimeji
 * @date 11/4/13 22:12:09 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, $, Handlebars*/
define(function (require, exports, module) {
    "use strict";
    var FormUtils					= require("./FormUtils"),
        template					= require("text!./templates/createWidget.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        d3							= require("d3/d3");

    function updateBoundFunctionsLabel() {
        var f = d3.select("#functionText").property("value"),
            str = "",
            events = [];
        d3.selectAll("input[type='radio'][name='events']").each(function () {
            if (this.checked) {
                events = events.concat(this.value.split("/"));
            }
        });
        str = events.map(function (d) {
            return d + "_" + f;
        }).join(", ");
        d3.select("#boundFunctions").text(str);
    }
    
    var NewWidgetView	= BaseDialog.extend({
        render: function (data) {
            var t = Handlebars.compile(template);
            this.$el.html(t(data));
            $("body").append(this.el);
            $("#tabHeaders #displayTab").tab("show");
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel",
            "click #displayTab": "displayTabClicked",
            "click #buttonTab": "buttonTabClicked",
            "change input[type='radio'][name='events']": "eventsChanged",
            "keyup #functionText": "eventsChanged"
        },
        eventsChanged: function (event) {
            updateBoundFunctionsLabel();
        },
        ok: function (event) {
            var form = d3.select(this.el).select("form");
            var type = form.select("li.active a").html().toLowerCase();
            var activeForm = form.select("#" + type).node();
            if (FormUtils.validateForm(activeForm)) {
                var formdata = FormUtils.serializeForm(activeForm, "input");
                formdata.type = type;
                this.trigger("ok", {data: formdata, el: this.el, event: event}, this);
            }
        },
        cancel: function (event) {
            this.trigger("cancel", {el: this.el, event: event}, this);
        },
        displayTabClicked: function (event) {
            event.preventDefault();
            $(event.target).tab("show");
        },
        buttonTabClicked: function (event) {
            event.preventDefault();
            $(event.target).tab("show");
        }
    });
    
    module.exports = {
        create: function (data) {
            data = data || { top: 10, left: 10, width: 60, height: 32 };
            var form = new NewWidgetView(data);
            return form;
        }
    };
});
