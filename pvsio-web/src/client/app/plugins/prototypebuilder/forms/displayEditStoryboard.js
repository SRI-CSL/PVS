/**
 * Edit storyboard -- this module is a variant of editWidget
 * @author Paolo Masci
 * @date 27/11/14 9:32:12 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var FormUtils  = require("./FormUtils"),
        template   = require("text!./templates/displayEditStoryboard.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        d3		   = require("d3/d3");

    var renderDialog = function (data) {
        var t = Handlebars.compile(template);
        this.$el.html(t(data));
        $("body").append(this.el);
        d3.select(this.el).select("#storyboard-Key").node().focus();
    };

    var DisplayEditStoryboard = BaseDialog.extend({
        render: renderDialog,
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel",
            "click #btnAddStoryboardImages": "btnAddStoryboardImages"
        },
        ok: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var selectors = [ "#storyboard-Key", "#image-Key" ]; // image-Keys are file paths
                var formdata = FormUtils.serializeForm(form, selectors);
                var data = {
                    storyboardKey: formdata["storyboard-Key"],
                    images: {}
                };
                var keys = Object.keys(formdata).filter(function (key) {
                    return key !== "storyboard-Key";
                });
                keys.forEach(function (key) {
                    var imageData = document.getElementById(key + "-Data");
                    data.images[key] = {
                        imageKey: formdata[key],
                        imageData: (imageData) ? imageData.src : null,
                        imagePath: key
                    };
                });

                this.trigger("ok", {data: data, el: this.el}, this);
            }
        },
        cancel: function (event) {
            this.trigger("cancel", {el: this.el, event: event}, this);
        },
        btnAddStoryboardImages: function (event) {
            this.trigger("btnAddStoryboardImages", {el: this.el, event: event}, this);
        }
    });

    module.exports = {
        create: function (data) {
            return new DisplayEditStoryboard(data);
        }
    };
});
