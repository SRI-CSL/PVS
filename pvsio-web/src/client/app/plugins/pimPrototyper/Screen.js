/**
 * Represents a screen with the PIM Prototyping mode. Responsible for managing the screen's widgets and overall
 * attributes.
 */

/*global define, Backbone, _ */
define(function (require, exports, module) {
    "use strict";

    var Descriptor = require("project/Descriptor"),
        uuid = require("util/uuidGenerator");

    var Screen = Backbone.Model.extend({
        defaults: {
            name: "New screen",
            isInitial: false
        },

        initialize: function (opts) {
            this.set("widgets", {});
            var id = (opts && opts.id != null) ? opts.id : "SCREEN_" + uuid();
            this.set("id", id);
        },

        toJSON: function () {
            // Copy the attributes from the Backbone model
            var json = _.clone(this.attributes);
            if (json.image != null) {
                // Replace the image object with its file name
                json.image = json.image.name;
            }

            var flatWidgets = [];

            // Replace each widget object with its JSON representation
            _.forEach(json.widgets, function (w) {
                flatWidgets.push(w.toJSON());
            });

            json.widgets = flatWidgets;

            return json;
        },

        /**
         * Creates a copy of the screen and all its widgets
         * @return {Screen} The copy of the screen
         */
        duplicate: function () {
            var clone = this.clone();
            clone.initialize();
            var originalWidgets = this.get("widgets");
            var cloneWidgets = clone.get("widgets");

            _.forEach(originalWidgets, function (w) {
                cloneWidgets[w.id()] = w.duplicate();
            });

            return clone;
        }
    });

    /**
     * Creates a new screen from the data in the provided object. Note: this does not restore/populate the screen's
     * 'wdigets' attribute.
     * @param {object} jsonObj JSON-style object with the data for the screen
     * @param {string} imageDirectory location of the prototype's images, relative to the projects/ directory
     * @return {Screen} The new screen
     */
    Screen.initFromJSON = function (jsonObj, imageDirectory) {
        var scr = new Screen({ id: jsonObj.id, name: jsonObj.name, isInitial: jsonObj.isInitial });

        if (jsonObj.image) {
            scr.set("image", new Descriptor(imageDirectory + jsonObj.image, null, { encoding: "base64" }));
        }

        return scr;
    };

    return Screen;
});
