/**
 * handles creating a form for opening a project
 * @author Patrick Oladimeji
 * @date Jan 5, 2013 : 6:42:35 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, $, Handlebars*/
define(function (require, exports, module) {
    "use strict";
    var template   = require("text!./templates/openFiles.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog");
    
    
    var OpenFilesView = BaseDialog.extend({
        render: function (opt) {
            var t = Handlebars.compile(template);
            this.$el.html(t(opt));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel"
        }
    });
    
    module.exports = {
        create: function (opt, labelFunc) {
            labelFunc = labelFunc || function (d) { return d.label; };
            opt = opt || {};
            opt.accept = (opt.extensions && typeof opt.extensions === "object")
                            ? opt.extensions.join(",") : ".pvs";
            opt.title = opt.title || "Import files into Project";
            return new OpenFilesView(opt);
        }
    };
});
