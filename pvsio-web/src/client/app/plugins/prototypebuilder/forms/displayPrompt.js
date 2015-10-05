/**
 * Displays a prompt to the user. Uses event dispatcher to tell the caller what button was clicked
 in response to the question.
 * @author Patrick Oladimeji
 * @date 3/25/14 14:52:24 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        formTemplate = require("text!./templates/displayPrompt.handlebars"),
        FormUtils = require("./FormUtils");

    var PromptView = BaseDialog.extend({
        initialize: function (data) {
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
            this.render(data);
            this._data = data;
        },
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnRight": "right",
            "click #btnLeft": "left"
        },
        right: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var formdata = FormUtils.serializeForm(form);
                this.trigger(this._data.buttons[1].toLowerCase(), {data: formdata, el: this.el}, this);
            }
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
        }
    });

    module.exports = {
        /**
         * creates a new form view to display questions. Renders two buttons for
         * taking positive or negative responses to the question posed.
         * @param {header: {string}} data Data to use to render the form
         */
        create: function (data) {
            return new PromptView(data);
        }
    };
});
