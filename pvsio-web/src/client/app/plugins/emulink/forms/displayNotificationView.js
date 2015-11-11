/**
 * Displays compilation errors in a textarea
 * @author Paolo Masci
 * @date 13/11/14 11:59:42 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*jshint unused:false*/
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayNotificationView.handlebars"),
        BaseDialog   = require("pvsioweb/forms/BaseDialog"),
        FormUtils = require("./FormUtils");

    var DisplayNotificationView = BaseDialog.extend({
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
            "click #btnRight": "right"
        },
        right: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var selectors = [ ];
                var formdata = FormUtils.serializeForm(form, selectors);
                this.trigger(this._data.buttons[0].toLowerCase().replace(new RegExp(" ", "g"), "_"),
                             {data: formdata, el: this.el}, this);
            }
        },
        keypress: function (event) {
            var form = this.el;
            switch (event.which) {
            case 13: //enter pressed
                this.right(event);
                break;
            case 27: //esc pressed
                this.left(event);
                break;
            default:
                break;
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
            return new DisplayNotificationView(data);
        }
    };
});
