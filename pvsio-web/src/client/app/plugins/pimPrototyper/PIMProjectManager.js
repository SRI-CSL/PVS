/**
* @module PIMProjectManager
* @description Manages file-related operations for the PIM prototyper, such as saving a project to disk
* @author Nathaniel Watson
*/

/*global _ */
define(function (require, exports, module) {
    "use strict";
    var Descriptor = require("project/Descriptor"),
        ScreenCollection = require("./ScreenCollection"),
        Screen = require("./Screen"),
        PIMWidget = require("./PIMWidget"),
        property = require("util/property"),
        Logger = require("util/Logger");

    var PIM_IMAGES_FOLDER = "pim-images/";
    var PIM_FILE_NAME = "pim-prototype.json";

    /**
     * Initialises the PIM project manager
     */
    var PIMProjectManager = function (projectManager) {
        var _this = this;
        this.screens = property.call(this, new ScreenCollection());
        this._projectManager = projectManager;
        _this._onProjectChanged(this._projectManager.project());

        this._projectManager.addListener("ProjectChanged", function (ev) {
            _this._onProjectChanged(ev.current);
        });
    };

    _.extend(PIMProjectManager.prototype, Backbone.Events);

    /**
     * Adds an image to the project folder
     * @param {string} imagePath Location where the image should be saved, relative to the project's PIM folder
     * @param {string} imageData base64 encoded image data
     */
    PIMProjectManager.prototype.addImage = function (imagePath, imageData) {
        var _this = this;

        return new Promise(function (resolve, reject) {
            var newImage = new Descriptor(PIM_IMAGES_FOLDER + imagePath, imageData, { encoding: "base64" });

            _this._project.addFile(newImage.path, newImage.content, { encoding: "base64", overWrite: true }).then(function (res) {
                resolve(res);
            }).catch(function (err) {
                Logger.error(err);
                reject(err);
            });
        });
    };

    /**
     * Serializes the current set of screens (i.e. the active PIM protottype) to a JSON string
     * @return {string} The serialized screens
     */
    PIMProjectManager.prototype.serializeProject = function () {
        var screens = this.screens().toJSON();
        return JSON.stringify(screens, null, " ");
    };

    /**
     * Restores a project from the data in the provided object. The restored project will be set as the active project
     * within the PIM project manager.
     * @param {object} json JSON-style object to restore from
     */
    PIMProjectManager.prototype.initFromJSON = function (json) {
        var pimImageFolder = this._project.name() + "/" + PIM_IMAGES_FOLDER;

        var screens = new ScreenCollection();
        _.forEach(json, function (scr) {
            screens.add(Screen.initFromJSON(scr, pimImageFolder));
        });

        // Loop over all the widgets and restore them. This is done seperately since the widgets make reference to
        // screens within the screen collection.
        _.forEach(json, function (scr) {
            var widgets = {};

            _.forEach(scr.widgets, function (w) {
                widgets[w.id] = PIMWidget.initFromJSON(w, screens);
            });

            screens.get(scr.id).set("widgets", widgets);
        });

        this.screens(screens);
    };

    PIMProjectManager.prototype._onProjectChanged = function (newProject) {
        var _this = this;
        this._project = newProject;

        var pimDescriptor = this._project.getDescriptor(this._project.name() + "/" + PIM_FILE_NAME);

        // If the new project has a PIM prototype file, load it in, otherwise create an empty descriptor for it
        if (pimDescriptor) {
            pimDescriptor.getContent().then(function (res) {
                var json = JSON.parse(res);
                _this.initFromJSON(json);
                _this.trigger("PIMProjectChanged");
            });
        } else {
            pimDescriptor = new Descriptor(PIM_FILE_NAME, "[]", { encoding: "utf8" });
            this._project.addDescriptor(pimDescriptor);
            this.screens(new ScreenCollection());
            this.trigger("PIMProjectChanged");
        }

        pimDescriptor.setContentGenerator(function () {
            return _this.serializeProject();
        });
    };

    module.exports = PIMProjectManager;
});
