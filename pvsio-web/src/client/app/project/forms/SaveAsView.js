/**
 * Template for Save As dialogs
 * @author Paolo Masci
 * @date Feb 6, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Handlebars, $*/
define(function (require, exports, module) {
    "use strict";
    var saveAsTemplate = require("text!./templates/saveAs.handlebars"),
        BaseDialog  = require("pvsioweb/forms/BaseDialog");

    var SaveAsView = BaseDialog.extend({
        render: function (data) {
            var dialog = Handlebars.compile(saveAsTemplate)(data);
            this.$el.html(dialog);
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel"
        }
    });

    module.exports = {
        create: function (data) {
            return new SaveAsView(data);
        }
    };
});
