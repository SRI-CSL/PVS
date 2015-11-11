/**
 * handles creating a form for opening a project
 * @author Patrick Oladimeji
 * @date Jan 5, 2013 : 6:42:35 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, $, Handlebars*/
define(function (require, exports, module) {
    "use strict";
    var d3						= require("d3/d3"),
        template				= require("text!./templates/openProject.handlebars"),
        BaseDialog              = require("pvsioweb/forms/BaseDialog"),
        FormUtils				= require("./FormUtils");


    var OpenProjectView = BaseDialog.extend({
        render: function (data) {
            var t = Handlebars.compile(template);
            this.$el.html(t(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click .caption button": "open",
            "click #btnCancel": "cancel",
            "keydown .panel": "keypress"
        },
        open: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var formdata = {projectName: d3.select(event.currentTarget).attr("data-project")};
                this.trigger("ok", {data: formdata, el: this.el, event: event}, this);
            }
        },
        keypress: function (event) {//override base keypress so that enter doesnt trigger ok
            switch (event.which) {
            case 27:
                this.cancel(event);
                break;
            }
        }
    });

    module.exports = {
        create: function (options, labelFunc) {
            labelFunc = labelFunc || function (d) { return d.label; };
            return new OpenProjectView({projects: options.map(labelFunc)});
        }
    };
});
