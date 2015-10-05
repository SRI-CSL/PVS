/**
 *
 * @author Patrick Oladimeji
 * @date 3/28/14 10:48:55 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, $, Handlebars, Backbone */
define(function (require, exports, module) {
    "use strict";
    var template = require("text!./templates/scriptItemView.handlebars");


    var ScriptItemView = Backbone.View.extend({
        initialize: function (data) {
            this.render(data);
        },
        render: function (data) {
            var t = Handlebars.compile(template);
            this.$el.html(t(data));
            $("#scripts ul").append(this.el);
            return this;
        },
        events: {
            "click li": "scriptClicked"
        },
        scriptClicked: function (event) {
            this.trigger("scriptClicked", $(event.target).attr("name"));
        }
    });

    module.exports = {
        create: function (script) {
            return new ScriptItemView(script);
        }
    };
});
