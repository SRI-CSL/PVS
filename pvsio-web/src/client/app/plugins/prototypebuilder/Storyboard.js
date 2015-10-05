/**
 * Storyboard widget -- this module is a variant of the Display widget, and it's used to create a storyboard programmatically. Rendering the storyboard widget means changing the prototype image.
 * @author Paolo Masci
 * @date 28/11/14 3:33:15 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Promise, d3 */
define(function (require, exports, module) {
    "use strict";
    var Widget       = require("./Widget"),
        uidGenerator = require("util/uuidGenerator"),
        fs	         = require("util/fileHandler"),
        displayEditStoryboard = require("pvsioweb/forms/displayEditStoryboard"),
        eventDispatcher = require("util/eventDispatcher"),
        StateParser  = require("util/PVSioStateParser");

    function Storyboard(id) {
        id = id || "storyboard";// + "_" + uidGenerator();
        Widget.call(this, id, "storyboard");
        this.storyboardKey = "screen";
        this.images = {}; // key = image path, value = unique string
        eventDispatcher(this);
    }

    Storyboard.prototype = Object.create(Widget.prototype);
    Storyboard.prototype.constructor = Storyboard;
    Storyboard.prototype.parentClass = Widget.prototype;

    Storyboard.prototype.render = function (state) {
        var imageName = StateParser.resolve(state, this.storyboardKey);
        if (imageName) {
            var image = this.images[imageName];
            if (image) {
                var imageData = this.images[imageName].imageData;
                if (imageData) {
                    d3.select("#imageDiv img").attr("src", imageData);
                }
            }
        }
    };

    /**
     * @function addImages
     * @memberof Storyboard
     * @param descriptors {Array(Object)} Array of image descriptors.
     * @returns {Promise(Array({image}))}. Each image has the following properties:
     *        <li>imagePath (String)</li>: file path of the image; this is a relative path, and the base path starts is the current project folder.
     *        <li>imageName (String)</li>: file name of the image.
     *        <li>imageData (String)</li>: image data, in the form data:image/...;base64...
     *        <li>placeholder (String)</li>: image name
     */
    Storyboard.prototype.addImages = function (descriptors) {
        var _this = this;

        function loadImage(desc) {
            try {
                return fs.readLocalFile(desc)
                    .then(function (res) {
                        var imagePath = "storyboard/" + uidGenerator() + "_" + res.path;
                        var imageData = res.content;
                        var imageKey = res.path
                                            .split(".").slice(0, -1).join("")
                                            .replace(/\W/g, "_");
                        _this.images[imagePath] = {
                            imagePath: imagePath,
                            imageKey: imageKey,
                            imageData: imageData
                        };
                        //return new Descriptor(imagePath, imageData, { encoding: "base64" };
                    });
            } catch (err) {
                console.log(err);
                //return err;
            }
        }

        var loadImagesPromise = [];
        if (descriptors && descriptors.length) {
            descriptors.forEach(function (desc) {
                if (desc.type && desc.type.indexOf("image/") === 0) {
                    loadImagesPromise.push(loadImage(desc));
                } else {
                    console.log("Warning: detected invalid image file descriptor...");
                }
            });
        }
        return Promise.all(loadImagesPromise);
    };

    Storyboard.prototype.getImages = function () {
        return this.images || [];
    };

    Storyboard.prototype.toJSON = function () {
        var images = [];
        if (this.images) {
            var _this = this;
            var keys = Object.keys(this.images);
            keys.forEach(function (key) {
                images.push({
                    imageKey : _this.images[key].imageKey,
                    imagePath: _this.images[key].imagePath
                });
            });
        }
        return {
            type: this.type(),
            id: this.id(),
            storyboardKey: this.storyboardKey,
            images: images
        };
    };

    Storyboard.prototype.remove = function () {
    };



    /**
     * @function displayEditStoryboardDialog
     * @memberof Storyboard
     * @fires event { type: "EditStoryboardComplete", widget: Storyboard }
     */
    Storyboard.prototype.displayEditStoryboardDialog = function () {
        var _this = this;
        displayEditStoryboard.create({
            header: "Editing storyboard...",
            message: "Please assign unique names to each storyboard image",
            images: _this.images,
            storyboard: _this.storyboardKey
        }).on("ok", function (e, view) {
            _this.storyboardKey = e.data.storyboardKey;
            _this.images = e.data.images;
            view.remove();
            _this.fire({ type: "EditStoryboardComplete", widget: _this });
        }).on("cancel", function (e, view) {
            // just remove window
            view.remove();
        }).on("btnAddStoryboardImages", function (e, view) {
            d3.select("#btnSelectMultiplePictures").on("change", function () {
                if (d3.event && d3.event.currentTarget && d3.event.currentTarget.files) {
                    var keys = Object.keys(d3.event.currentTarget.files).filter(function (key) {
                        return key !== "length";
                    });
                    var data = [];
                    keys.forEach(function (key) {
                        data.push(d3.event.currentTarget.files[key]);
                    });
                    // remove old dialog
                    view.remove();
                    // and create a new dialog with the added images
                    _this.addImages(data).then(
                        function (images) {
                            _this.displayEditStoryboardDialog();
                        }
                    );
                }
            });
            d3.select("#btnSelectMultiplePictures").node().click();
        });
    };

    module.exports = Storyboard;
});
