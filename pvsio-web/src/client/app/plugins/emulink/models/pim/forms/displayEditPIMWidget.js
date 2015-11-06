/**
 * Displays edit window for the widgets of a state.
 * @author Nathan Robb
 * @date 20/10/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, $, Backbone, Handlebars, self */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayEditPIMWidget.handlebars"),
        FormUtils = require("plugins/emulink/forms/FormUtils");

    var EditPIMWidgetView = Backbone.View.extend({
        initialize: function (data) {
            // Set z-index to be above the edit state modal.
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px").style("z-index", 999);
            // Internal count for displaying widgets.
            this._count = 0;
            this.render(data);
            this._data = data;
            this._widgets = data.value.widgets;
        },
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            this.$widgetsList = $('#pmWidgetsList', this.el);
            this.$errorDisplay = $('#editWidgetsError', this.el);
            this.buildWidgetList(data.value.widgets);
            d3.select(this.el).select("#newWidgetName").node().focus();
            return this;
        },
        events: {
            "click #btnRight2": "right",
            "click #btnLeft2": "left",
            "click #btnAdd": "add",
            "click .btnRemove": "removeWidget"
        },
        right: function (event) {
            this.trigger(this._data.buttons[1].toLowerCase().replace(new RegExp(" ", "g"), "_"),
                {data: this._widgets, el: this.el}, this);
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
        },
        // Adds a new widget to the widget list.
        add: function (event) {
            var _this = this;
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var selectors = ["newWidgetName", "newWidgetCategory", "newWidgetBehaviours"];
                var formData = FormUtils.serializeForm(form, selectors);
                var newWidgetName = formData.labels.get("newWidgetName");
                var newWidgetCategory = formData.labels.get("newWidgetCategory");
                var newWidgetBehaviours = formData.labels.get("newWidgetBehaviours");

                var newWidget = _this.validateWidget(newWidgetName, newWidgetCategory, newWidgetBehaviours);
                d3.select(_this.el).select("#newWidgetName").node().focus();
                if (!newWidget)
                    return;

                // Valid widget clear the inputs for the next widget to get added.
                FormUtils.clearForm(selectors);
                // Add new widget to the list.
                _this._widgets.push(newWidget);
                this.$widgetsList.append(_this.widgetListItem(newWidget));
            }
        },
        removeWidget: function (event) {
            if (confirm("Delete this widget?")) {
                // TODO: Replace with a unique widget ID, this works but not the best implementation.
                var widgetId = event.currentTarget.parentNode.parentNode.id;
                this._widgets.splice(widgetId, 1);
                $('#' + widgetId, this.$widgetsList).remove();
            }
        },
        buildWidgetList: function (widgets) {
            var _this = this;
            this.$widgetsList.html(widgets.map(_this.widgetListItem, _this));
        },
        widgetListItem: function (w) {
            var item =
                '<div id="' + this._count++ + '" class="row" style="padding: 2px 0 2px 0;">' +
                    '<div class="col-md-3">' + w.name + '</div>' +
                    '<div class="col-md-3">' + w.category + '</div>' +
                    '<div class="col-md-5">' + w.behaviours.join(", ") + '</div>' +
                    '<div class="col-md-1"><button type="button" class="btn btn-danger btn-xs btnRemove right" aria-label="Close"><span aria-hidden="true">&times;</span></button></div>' +
                '</div>';
            return item;
        },
        // newWidgetBehaviours optional. returns null if inputs aren't valid.
        validateWidget: function (newWidgetName, newWidgetCategory, newWidgetBehaviours) {
            // Custom PIM validation on Behaviours.
            var errors = [];
            if (!newWidgetName || !(newWidgetName = newWidgetName.trim()))
                errors.push('Name must have a value.');
            if (!newWidgetCategory || !(newWidgetCategory = newWidgetCategory.trim()))
                errors.push('Category must have a value.');
            // Optional.
            if (newWidgetBehaviours) {
                // Retrieve behaviours.
                newWidgetBehaviours =
                    newWidgetBehaviours
                        .split(',')
                        .map(function(b) { return b.trim(); })
                        .filter(function(b) { return b; });

                // Invalid behaviour test (true if invalid. used for $.grep).
                var invalidBehaviour =
                    function(b) {
                        if (!b || b.length < 3) { return true; }
                        b = b.substring(0, 2);
                        return !(b === 'S_' || b === 'I_');
                    };

                // Test if any of the behaviours fail the validBehaviour test (i.e. return true).
                if ($.grep(newWidgetBehaviours, invalidBehaviour).length) {
                    errors.push('All behaviours must begin with either S_ or I_ and must have a value.');
                }
            }
            if (errors.length) {
                this.$errorDisplay.parent().removeClass('hide');
                this.$errorDisplay.html(errors.join('<br />'));
                return null;
            }
            this.$errorDisplay.parent().addClass('hide');
            this.$errorDisplay.empty();
            return {
                name: newWidgetName.trim(),
                category: newWidgetCategory.trim(),
                behaviours: newWidgetBehaviours || []
            };
        }
    });

    module.exports = {
        /**
         * @param {
         *    {header} form header
         *    {textLabel} form labels
         *    {placeholder} form placeholder values
         *    {buttons} names for cancel and ok buttons
         * }
         */
        create: function (data) {
            return new EditPIMWidgetView(data);
        }
    };
});

