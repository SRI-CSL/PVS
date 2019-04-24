/**
 * Displays rename window for machine states.
 * @author Paolo Masci
 * @date 18/12/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayEditState.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        FormUtils = require("./FormUtils");
        // EmuchartsTextEditor = require("plugins/emulink/EmuchartsTextEditor");

    var AddStateView = BaseDialog.extend({
        initialize: function (data) {
            d3.select(this.el).attr("class", "overlay").style("top", self.scrollY + "px");
            this.render(data);
            this._data = data;
        },
        render: function (data) {
            var template = Handlebars.compile(formTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            d3.select(this.el).select("#newStateName").node().focus();
            /*
            var textArea = d3.select(this.el).select("#newStateEnter").node();
            var size = {
                width: "100%",//textArea.getBoundingClientRect().width,
                height: textArea.getBoundingClientRect().height
            };
            var editor = new EmuchartsTextEditor({
                textArea: textArea,
                size: size
            });
            if (d3.select(".overlay").select(".CodeMirror-code").select("pre").node()) {
                d3.select(".overlay").select(".CodeMirror-code").select("pre").node().focus();
            } else {
                textArea.focus();
            }*/
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
                var selectors = [ "newStateName", "newStateColor", "newStateEnter", "newStateExit" ];
                var formdata = FormUtils.serializeForm(form, selectors);
                this.trigger(this._data.buttons[1].toLowerCase().replace(new RegExp(" ", "g"), "_"),
                             {data: formdata, el: this.el}, this);
            }
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
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
            return new AddStateView(data);
        }
    };
});
