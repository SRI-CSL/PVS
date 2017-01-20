/**
 * View that provides controls for managing a PIM prototype's screens
 * @author Nathaniel Watson
 */
/*global define, Backbone */
define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3"),
        template = require("text!./templates/ScreenControlsView.handlebars"),
        ScreenDropdownView = require("./ScreenDropdownView"),
        NewScreenView = require("./NewScreenView"),
        EditScreenView = require("./EditScreenView"),
        Screen = require("../Screen"),
        DisplayQuestion = require("pvsioweb/forms/displayQuestion");

    var ScreenControlsView = Backbone.View.extend({

        events: {
            "click .btn-screen-add": "showAddScreen",
            "click .btn-screen-duplicate": "_duplicateScreen",
            "click .btn-screen-options": "showEditScreen",
            "click .btn-screen-image": "onClickChangeImage",
            "click .btn-screen-delete": "showDeleteConfirmation"
        },

        /**
         * @function initialize
         * @description Creates a new screen controls view and renders it to the provided element
         * @param {Object} options Options for the view.
         * @param {ScreenCollection} options.collection Required. Collection of screens to use
         */
        initialize: function (options) {
            var _this = this;
            this._setupCollectionListeners();

            this._screenDropdown = new ScreenDropdownView({
                collection: this.collection,
                up: true,
                buttonClasses: ""
            });

            this.listenTo(this._screenDropdown, "screenSelected", function (selected) {
                _this.collection.setSelected(selected);
            });

            this._template = Handlebars.compile(template);
            this.$el.addClass("pim-screen-controls");
            this.render();

            return this;
        },

        /**
         * @function render
         * @description Updates and redraws the view.
         * @return {ScreenControlsView} The view
         */
        render: function () {
            this.$el.html(this._template());
            this.d3El = d3.select(this.el);
            this._screenButtons = [
                this.d3El.select(".btn-screen-options"),
                this.d3El.select(".btn-screen-image"),
                this.d3El.select(".btn-screen-delete"),
                this.d3El.select(".btn-screen-duplicate")
            ];

            this._screenDropdown.setElement(this.d3El.select(".dropdown-container").node()).render();
            this._setButtonStates(false); // disable buttons by default
            return this;
        },

        showAddScreen: function () {
            var _this = this;

            new NewScreenView()
                .on("cancel", function(data, view) {
                    view.remove();
                })
                .on("ok", function(data, view) {
                    view.remove();
                    var screen = new Screen({ name: data.data.screenName });
                    _this.collection.add(screen);
                    _this.collection.setSelected(screen);
                });
        },

        showEditScreen: function () {
            var _this = this;
            var screen = this.collection.getSelected();

            new EditScreenView({ model: screen })
                .on("cancel", function(data, view) {
                    view.remove();
                })
                .on("ok", function(data, view) {
                    var oldInitial = _this.collection.find(function(s) {
                        return s.get("isInitial");
                    });

                    if (oldInitial != null) {
                        oldInitial.set("isInitial", false);
                    }

                    screen.set({
                        name: data.data.screenName,
                        isInitial: (!!data.data.isInitial)
                    });
                    _this._screenDropdown._updateInitialScreen();
                    view.remove();
                });
        },

        showDeleteConfirmation: function () {
            var _this = this;

            var data = {header: "Confirm Delete",
                        question: "Are you sure you want to delete the current screen? This cannot be undone.",
                        buttons: ["Cancel", "Delete"],
                        primaryLevel: "danger"};
            DisplayQuestion.create(data)
                .on("delete", function (e, view) {
                    _this.collection.remove(_this.collection.getSelected());
                    view.remove();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
        },

        /**
         * Sets the collection of screens being represented by this view
         * @param {ScreenCollection} collection Collection of screens to use
         */
        setCollection: function (collection) {
            this.stopListening(this.collection);
            this.collection = collection;
            this._screenDropdown.setCollection(collection);
            this._screenDropdown.setSelected(null);
            this._setupCollectionListeners();
        },

        _setupCollectionListeners: function () {
            this.listenTo(this.collection, "selectionChanged", this._onSelectionChanged);
        },

        onClickChangeImage: function () {
            this.trigger("changeImageClicked");
        },

        _onSelectionChanged: function (newSelection, oldSelection) {
            this._screenDropdown.setSelected(newSelection);

            if (newSelection == null) {
                this._setButtonStates(false);
            } else if (oldSelection == null) {
                this._setButtonStates(true);
            }
        },

        _setButtonStates: function (enabled) {
            for (var index in this._screenButtons) {
                this._screenButtons[index].attr("disabled", enabled ? null : "true");
            }
        },

        _duplicateScreen: function () {
            var _this = this;
            var screen = this.collection.getSelected();
            var duplicate = screen.duplicate();
            duplicate.set("name", "copy of " + screen.get("name"));
            duplicate.set("isInitial", false); // don't allow more than one initial screen
            _this.collection.add(duplicate);
            _this.collection.setSelected(duplicate);
        }
    });

    return ScreenControlsView;
});
