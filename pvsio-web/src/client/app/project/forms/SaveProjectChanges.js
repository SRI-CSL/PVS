/**
 * Shows a dialog to save changes to a project
 * @author Patrick Oladimeji
 * @date Dec 29, 2013 : 23:23:48
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, $, Handlebars*/
define(function (require, exports, module) {
    "use strict";
    var BaseDialog              = require("pvsioweb/forms/BaseDialog"),
        template				= require("text!./saveChanges.handlebars");

    var SaveProjectChangesView = BaseDialog.extend({
        render: function (data) {
            var t = Handlebars.compile(template);
            this.$el.html(t(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnYes": "yes",
            "click #btnNo": "no"
        },

        yes: function (event) {
            this.trigger("yes", {el: this.el, event: event}, this);
        },
        no: function (event) {
            this.trigger("no", {el: this.el, event: event}, this);
        }
    });

    module.exports = {
        create: function (project) {
            return new SaveProjectChangesView({name: project.name()});
        }
    };
});
