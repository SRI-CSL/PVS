/**
 * Displays a modal dialog for entering parameters (e.g., required for a printer).
 * @author Paolo Masci
 * @date Feb 15, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*jshint unused:false*/
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayAskParameters.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        FormUtils = require("./FormUtils");

    var DisplayAskParameters = BaseDialog.extend({
        initialize: function (data) {
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
            this.render(data);
            this._data = data;
            this.focus();
        },
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnRight": "right",
            "click #btnLeft": "left",
            "keydown .panel": "keypress"
        },
        right: function (event) {
            var form = this.el;
            if (FormUtils.validateForm(form)) {
                var selectors = this._data.params.map(function (param) {
                    return param.id;
                });
                var formdata = FormUtils.serializeForm(form, selectors);
                var _this = this;
                formdata.options.keys().forEach(function (option) {
                    var tmp = _this._data.params.filter(function (x) { return x.id === option; });
                    if (tmp && tmp.length > 0) {
                        var ans = tmp[0].options[formdata.options.get(option)];
                        if (ans) {
                            formdata.options.set(option, ans.value);
                        }
                    }
                });
                formdata.labels.keys().forEach(function (key) {
                    if (formdata.labels.get(key) && formdata.labels.get(key) === "") {
                        formdata.labels.remove(key);
                    }
                });
                this.trigger(this._data.buttons[1].toLowerCase().replace(new RegExp(" ", "g"), "_"),
                             {data: formdata, el: this.el}, this);
            }
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
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
            return new DisplayAskParameters(data);
        }
    };
});
