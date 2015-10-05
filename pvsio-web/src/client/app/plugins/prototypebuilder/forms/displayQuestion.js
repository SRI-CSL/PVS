/**
 * Displays a question to the user. Uses event dispatcher to tell the caller what button was clicked
 in response to the question.
 * @author Patrick Oladimeji
 * @date 2/20/14 14:52:24 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var formTemplate = require("text!./templates/displayQuestion.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog");

    var QuestionView = BaseDialog.extend({
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel"
        }
    });

    module.exports = {
        /**
         * creates a new form view to display questions. Renders two buttons for
         * taking positive or negative responses to the question posed.
         * @param {header: {string}, question: {string}} data Data to use to render the form
         */
        create: function (data) {
            return new QuestionView(data);
        }
    };
});
