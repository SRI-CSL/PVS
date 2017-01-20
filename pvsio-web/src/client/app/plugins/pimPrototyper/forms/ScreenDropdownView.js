/**
 * View that provides a dropdown for selecting a PIM prototyping screen
 * @author Nathaniel Watson
 */
/*global define, Backbone */
define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3"),
        template = require("text!./templates/ScreenDropdownView.handlebars");

	var ScreenDropdown = Backbone.View.extend({

        /**
         * @function initialize
         * @description Creates a new dropdown. Note: This does not render the view.
         * @param {Object} options Options for the view.
         * @param {ScreenCollection} options.collection Required. Collection of screens to display in the dropdown
         */
        initialize: function (options) {
            this._setupCollectionListeners();
            this._template = Handlebars.compile(template);
            this._options = options;

            return this;
        },

        /**
         * @function render
         * @description Updates and redraws the view.
         * @return {ScreenDropdown} The view
         */
        render: function () {
            this.$el.html(this._template());
            this.$el.addClass("btn-group " + (this._options.up ? "dropup" : "dropdown"));
            this.d3El = d3.select(this.el);
            this._dropdown = this.d3El.select(".screen-dropdown");

            this._updateScreenList();
            return this;
        },

        /**
         * Sets the provided screen as selected. Note this only affects the view, not the underlying ScreenCollection.
         * @param {Screen} selected Screen to display as selected
         */
        setSelected: function (selected) {
            this._selected = selected;
            this._updateSelectedText();
        },

        /**
         * Sets the collection of screens being represented by this view
         * @param {ScreenCollection} collection Collection of screens to use
         */
        setCollection: function (collection) {
            this.stopListening(this.collection);
            this.collection = collection;
            this._setupCollectionListeners();
            this._updateScreenList();
        },

        _setupCollectionListeners: function () {
            this.listenTo(this.collection, "add remove change:name", this._updateScreenList); // TODO: don't re-render the list when a single item changes
        },

        _updateInitialScreen: function () {
            this._dropdown.selectAll("li a").select("span").attr("style", function (d) {
                return (d.attributes.isInitial) ? "color: blue; width:20px;" : "color: transparent; width:20px;";
            });
            this._updateSelectedText();
        },

        _updateSelectedText: function () {
            this.d3El.select(".btn-screen-dropdown_label").text(this._selected ? this._selected.get("name") : "Select a screen");
            this.d3El.select("#btn-screen-dropdown_label_star").node().style.display = (this._selected && this._selected.attributes.isInitial) ? "block" : "none";
        },

        _updateScreenList: function () {
            var _this = this;

            this._updateSelectedText();

            var selection = this._dropdown.selectAll("li")
                .data(this.collection.models, function (screen) {
                    return screen.attributes.id;
                });

            var listItemsEnter = selection.enter().append("li");

            listItemsEnter.append("a")
                .on("click", function(d) {
                    _this.trigger("screenSelected", d);
                });

            listItemsEnter.select("a")
                .attr("data-toggle", "popover").attr("data-trigger", "hover").attr("data-content", function (d) {
                    return (d.attributes.isInitial) ? "Initial Screen" : "";
                })
                .append("span").attr("class", "glyphicon glyphicon-star").attr("style", function (d) {
                    return (d.attributes.isInitial) ? "color: blue; width:20px;" : "color: transparent; width:20px;";
                });

            listItemsEnter.select("a").append("span").text(function(d) {
                    return d.get("name");
                });

            selection.exit().remove();
        }
    });

    return ScreenDropdown;
});
