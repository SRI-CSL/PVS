/**
 * Displays edit window for PIM interaction behaviours.
 * @author Nathan Robb
 * @date 20/10/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, $, Backbone, Handlebars, self */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayEditPIMTransition.handlebars"),
        FormUtils = require("plugins/emulink/forms/FormUtils");

    var EditPIMTransitionView = Backbone.View.extend({
        initialize: function (data) {
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
            this.render(data);
            this._data = data;
            // Used for displaying the error message for a badly formatted transition.
            this.$errorDisplay = $('#editTransitonError', this.el);
        },
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            d3.select(this.el).select("#i_behaviour").node().focus();
            return this;
        },
        events: {
            "click #btnRight": "right",
            "click #btnLeft": "left",
            "keyup .panel": "keyup"
        },
        right: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var selectors = [ "i_behaviour" ];
                var formdata = FormUtils.serializeForm(form, selectors);
                this.trigger(this._data.buttons[1].toLowerCase().replace(new RegExp(" ", "g"), "_"),
                    {data: formdata, el: this.el}, this);
            }
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
        },
        keyup: function (event) {
            switch(event.keyCode) {
                case 13: //enter pressed
                    this.right(event);
                    break;
                case 27: //esc pressed
                    this.left(event);
                    break;
                default: break;
            }
        }
    });

    module.exports = {
        /**
         * @param {
         *    {header} form header
         *    {buttons} names for cancel and ok buttons
         * }
         */
        create: function (data) {
            return new EditPIMTransitionView(data);
        }
    };
});

