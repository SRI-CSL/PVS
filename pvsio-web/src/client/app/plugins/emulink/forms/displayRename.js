/**
 * Displays edit window for states and transitions.
 * @author Paolo Masci
 * @date 5/24/14 2:08:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*jshint unused:false*/
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3"),
        formTemplate = require("text!./templates/displayRename.handlebars"),
        BaseDialog = require("pvsioweb/forms/BaseDialog"),
        FormUtils = require("./FormUtils"),
        EmuchartsTextEditor = require("plugins/emulink/EmuchartsTextEditor");


    var DisplayRenameView = BaseDialog.extend({
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
            var textArea = d3.select(this.el).select("#newLabel").node();
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
            }
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
                var selectors = [ "newLabel" ];
                var formdata = FormUtils.serializeForm(form, selectors);
                this.trigger(this._data.buttons[1].toLowerCase(), {data: formdata, el: this.el}, this);
            }
        },
        left: function (event) {
            this.trigger(this._data.buttons[0].toLowerCase(), {el: this.el}, this);
        },
        keypress: function (event) {
            var form = this.el;
            switch (event.which) {
            case 13: //enter pressed
//                this.right(event); // do nothing in this case
                break;
            case 27: //esc pressed
//                this.left(event); // do nothing in this case
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
         *    {elem} list of diagram elements already present in the diagram
         *    {buttons} names for cancel and ok buttons
         * }
         */
        create: function (data) {
            return new DisplayRenameView(data);
        }
    };
});
