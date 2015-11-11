/**
 * Module responsible for displaying and allowing access for modification fo preferences.
 * @author Patrick Oladimeji
 * @date 30/06/2015 11:45:29
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Handlebars, $*/
define(function (require, exports, module) {
    "use strict";
    var dialogTemplate = require("text!./templates/preference-dialog.handlebars"),
        BaseDialog  = require("pvsioweb/forms/BaseDialog"),
        Preferences = require("preferences/PreferenceStorage");

    var PreferenceDialog = BaseDialog.extend({
        render: function () {
            var prefs = Preferences.getInstance().getAllPreferences();
            var template = Handlebars.compile(dialogTemplate);
            this.$el.html(template(prefs));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel"
        }
    });

    module.exports = {
        create: function () {
            return new PreferenceDialog();
        }
    };
});
