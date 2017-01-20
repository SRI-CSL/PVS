/**
 * View that provides inputs for creating a new PIM widget or editing an existing one
 * @author Nathaniel Watson
 */
/*global define */
define(function (require, exports, module) {
    var BaseDialog = require("pvsioweb/forms/BaseDialog"),
        ScreenDropdownView = require("./ScreenDropdownView"),
        d3 = require("d3/d3"),
		FormUtils = require("pvsioweb/forms/FormUtils"),
        template = require("text!./templates/WidgetConfigView.handlebars");

    var WidgetConfigView = BaseDialog.extend({

        events: {
            "click .btn-cancel": "cancel",
            "click .btn-create": "ok"
        },

        /**
         * Creates a new view and renders it
         * @param {object} data Options for the view
         * @param {ScreenCollection} data.screenCollection Collection of screens to display as targets
         * @param {PIMWidget} [data.widget] Widget whose data should be used to pre-populate the form
         * @param {string} [data.title] Text to display as the dialog title
         * @param {string} [data.okText] Text to display on the OK button
         */
        initialize: function (data) {
            var _this = this;

            this._d3El = d3.select(this.el);
            this._d3El.attr("class", "overlay").style("top", self.scrollY + "px");

            this._screenDropdown = new ScreenDropdownView({
                collection: data.screenCollection,
                up: false,
                buttonClasses: ""
            });

            this.listenTo(this._screenDropdown, "screenSelected", function (selected) {
                _this._screen = selected;
                this._screenDropdown.setSelected(selected);
            });

            this._template = Handlebars.compile(template);
            this._data = data;
            this.render();
            this.focus();
        },

        render: function () {
            this.$el.html(this._template(this._data));
            this._screenDropdown.setElement(this._d3El.select(".dropdown-container").node()).render();
            if (this._data.widget != null) {
                this._screenDropdown.setSelected(this._data.widget.targetScreen());
            }
            $("body").append(this.el);
            return this;
        },

        ok: function (event) {
            if (this._validate()) {
				var form = this.el;
                var formdata = FormUtils.serializeForm(form);
				formdata.targetScreen = this._screen;
                this.trigger("ok", {data: formdata, el: this.el, event: event}, this);
            }
        },

        _validate: function () {
            return this._d3El.select("form").node().checkValidity();
        },

        remove: function () {
            this._screenDropdown.remove();
            BaseDialog.prototype.remove.apply(this);
        }
    });

    return WidgetConfigView;
});
