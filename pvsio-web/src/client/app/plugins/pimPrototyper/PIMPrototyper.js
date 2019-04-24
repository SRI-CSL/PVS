/**
 *
 * @author Nathaniel Watson, Paolo Masci
 * @date June 30, 2018
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global layoutjs, console */
define(function (require, exports, module) {
    "use strict";
    const normalize = require("util/Normalize").getInstance();
    var PVSioWebClient = require("PVSioWebClient"),
        ScreenControlsView = require("./forms/ScreenControlsView"),
        Screen = require("./Screen"),
        ScreenCollection = require("./ScreenCollection"),
        PIMWidgetManager = require("./PIMWidgetManager"),
        PrototypeImageView = require("pvsioweb/forms/PrototypeImageView"),
        WidgetsListView = require("pvsioweb/forms/WidgetsListView"),
        WidgetConfigView = require("./forms/WidgetConfigView"),
        template = require("text!./forms/templates/PIMPrototyperPanel.handlebars"),
        ProjectManager = require("project/ProjectManager"),
        PIMProjectManager = require("./PIMProjectManager"),
        PluginManager = require("plugins/PluginManager"),
        EmuWrapper = require("./EmuWrapper"),
        Emulink = require("plugins/emulink/Emulink"),
        DisplayQuestion = require("pvsioweb/forms/displayQuestion"),
        MIME = require("util/MIME").getInstance();

    var instance;

    var PIMPrototyper = function() {
    };

    /**
     * Initialises the PIM prototyping panel and adds it to the DOM
     * @param {(string|Element)} parentElement Element to add the PIM panel to
     * @private
     */
    PIMPrototyper.prototype._init = function (parentElement) {
        var _this = this;
        this.collapsed = false;
        this._projectManager = new PIMProjectManager(ProjectManager.getInstance());
        this._screens = new ScreenCollection();

        this._projectManager.on("PIMProjectChanged", function () {
            _this._onProjectChanged();
        });

        var opts = {
            headerText: this.getName(),
            owner: this.getId(),
            ownerObject: this,
            showContent: !this.collapsed,
            onClick: function (collapsed) {
                _this.collapsed = collapsed;
            }
        };

        if (parentElement) {
            opts.parent = parentElement;
        }

        this._container = PVSioWebClient.getInstance().createCollapsiblePanel(opts);
        this._container.html(Handlebars.compile(template)());
        this._container.classed("pim-prototyper", true);

        this._modeButtons = {
            builder: this._container.select(".prototype-builder-button"),
            simulator: this._container.select(".prototype-simulator-button")
        };

        this._modeButtons.builder.on("click", function() {
            _this.switchToBuilderView();
        });

        this._modeButtons.simulator.on("click", function() {
            _this.switchToSimulatorView();
        });

        this._container.select(".pim-convert-button").on("click", function() {
            var data = {header: "Warning: chart will be cleared",
                        question: "This will clear any existing chart in the EmuCharts Editor. Do you want to continue?",
                        buttons: ["Cancel", "Continue"],
                        primaryLevel: "success"};
            DisplayQuestion.create(data)
                .on("continue", function (e, view) {
                    view.remove();
                    var emulink = Emulink.getInstance();

                    (PluginManager.getInstance().isLoaded(emulink)
                        ? Promise.resolve()
                        : PluginManager.getInstance().enablePlugin(emulink))
                    .then(function () {
                        var emuManager = emulink.getEmuchartsManager();
                        var emuEditor = emuManager.getSelectedEditor();

                        emuManager.delete_chart();

                        if (_this._emuWrapper != null) {
                            _this._emuWrapper.setEmuchartsEditor(emuEditor);
                        } else {
                            _this._emuWrapper = new EmuWrapper(emuEditor);
                        }

                        // Make sure the Emucharts editor is in PIM mode
                        if (!emuManager.getIsPIM()) {
                            emuManager.toPIM(true);
                        }

                        _this._emuWrapper.addScreens(_this._screens);
                    });
                }).on("cancel", function (e, view) {
                    view.remove();
                });
        });

        this._fileSystem = require("filesystem/FileSystem").getInstance();
        this._widgetManager = new PIMWidgetManager();

        this._screenControlsView = new ScreenControlsView({
            el: this._container.select(".panel-footer").node(),
            collection: this._screens
         });

        this._prototypeImageView = new PrototypeImageView({
            el: this._container.select(".prototype-image-container").node(),
            widgetManager: this._widgetManager
        });

        this._widgetsListView = new WidgetsListView({
            el: this._container.select(".prototype-list-container").node(),
            widgetManager: this._widgetManager,
            labelFunction: function (widget) {
                return widget.name();
            }
        });

        this._setUpChildListeners();
        this._onProjectChanged();

        this._container.select(".ljs-hcontent").on("resize", function () {
            _this._prototypeImageView.resize();
        });

        layoutjs({el: "#body"});
        this.switchToBuilderView();
    };

    let name = "Storyboard Editor";
    PIMPrototyper.prototype.getName = function () {
        return name;
    };

    PIMPrototyper.prototype.getId = function () {
        return normalize.removeSpaceDash(name);
    };

    PIMPrototyper.prototype.initialise = function () {
        this._init();
        return Promise.resolve(true);
    };

    PIMPrototyper.prototype.unload = function () {
        this._prototypeImageView.remove();
        this._widgetsListView.remove();
        PVSioWebClient.getInstance().removeCollapsiblePanel(this._container);
        return Promise.resolve(true);
    };

    PIMPrototyper.prototype.getDependencies = function () {
        return [];
    };

    PIMPrototyper.prototype.handleKeyDownEvent = function (e) {
        if (!this.collapsed) {
            this._prototypeImageView._mapCreator.handleKeyDownEvent(e);
        }
    };

    /**
     * Change the image for the active screen (or create a screen if none are currently selected)
     * @param {string} imagePath
     * @param {string} imageData base64 encoded data
     */
    PIMPrototyper.prototype.changeImage = function (imagePath, imageData) {
        var _this = this;

        return this._projectManager.addImage(imagePath, imageData).then(function (res) {
            var selected = _this._screens.getSelected();

            // The user is changing the image with no screen selected, so add a new screen
            if (selected == null) {
                selected = new Screen({ name: "New screen" });
                _this._screens.add(selected);
                _this._screens.setSelected(selected);
            }

            selected.set("image", res);

            return res;
        });
    };

    PIMPrototyper.prototype.switchToSimulatorView = function () {
        this._switchToView(true);
    };

    PIMPrototyper.prototype.switchToBuilderView = function () {
        this._switchToView(false);
    };

    PIMPrototyper.prototype._switchToView = function (toViewMode) {
        var _this = this;
        var slideDuration = 300;
        this._container.select(".image-map-layer").style("opacity", toViewMode ? 0 : 1).style("z-index", toViewMode ? -2 : 190);
        this._modeButtons.simulator.classed("btn-info active", toViewMode);
        this._modeButtons.builder.classed("btn-info active", !toViewMode);
        this._container.select(".ljs-left")
            .transition()
            .duration(slideDuration)
            .styleTween("width", function () {
                var from = this.style.width,
                    to = toViewMode ? "0%" : "20%";
                return d3.interpolateString(from, to);
            });
        this._container.select(".ljs-hcontent")
            .transition()
            .duration(slideDuration)
            .styleTween("width", function () {
                var from = this.style.width,
                    to = toViewMode ? "100%" : "80%";
                return d3.interpolateString(from, to);
            })
            .each("end", function () {
                _this._prototypeImageView.resize();
            });
        if (toViewMode) {
            this._screenControlsView.toViewMode();
        } else {
            this._screenControlsView.toEditMode();
        }
    };

    PIMPrototyper.prototype._onWidgetClicked = function (widget) {
        var target = widget.targetScreen();

        if (target != null) {
            this._screens.setSelected(target);
        }
    };

    PIMPrototyper.prototype._showLoadImageDialog = function () {
        var _this = this;

        return new Promise(function (resolve, reject) {
            if (PVSioWebClient.getInstance().serverOnLocalhost()) {
                _this._fileSystem.readFileDialog({
                    encoding: "base64",
                    title: "Select a picture",
                    filter: MIME.imageFilter
                }).then(function (descriptors) {
                    _this.changeImage(descriptors[0].name, descriptors[0].content).then(function (res) {
                        _this._prototypeImageView.setImage(res).then(function (res) {
                            _this.updateControlsHeight();
                            if (_this._container.select("#imageDiv svg").node() === null) {
                                // we need to create the svg layer, as it's not there
                                // this happens when a new project is created without selecting an image
                                _this._prototypeImageView.updateMapCreator();
                            }
                            resolve(res);
                        });
                    });
                }).catch(function (err) { reject(err); });
            } else {
                // TODO: nwatson: implement/hook into file upload functionality
            }
        });
    };

    PIMPrototyper.prototype._onSelectedScreenChange = function (selectedScreen) {
        var _this = this;
        this._prototypeImageView.softClearWidgetAreas();
        this._widgetManager.setScreen(
            selectedScreen,
            function(widget) {
                _this._onWidgetClicked(widget);
            },
            this._prototypeImageView.getImageMap()
        );
        this._widgetsListView.update();

        if (selectedScreen == null || selectedScreen.get("image") == null) {
            this._prototypeImageView.clearImage();
        } else {
            var image = selectedScreen.get("image");
            image.getContent().then(function () {
                _this._prototypeImageView.setImage(image);
            }).catch(function (err) {
                console.error("Failed to select screen: " + err);
            });
        }
    };

    PIMPrototyper.prototype._onProjectChanged = function () {
        this._screens = this._projectManager.screens();
        this._screens.on("selectionChanged", this._onSelectedScreenChange, this);
        this._screenControlsView.setCollection(this._screens);
        // when a project is loaded, automatically select the initial screen, if one has been set
        var initialScreen = this._screens.models.filter(function (model) { return model.attributes.isInitial; });
        if (initialScreen && initialScreen.length > 0) {
            this._screens.setSelected(initialScreen[0]);
        } else {
            this._onSelectedScreenChange();
        }
    };

    /**
     * Sets up listeners on child views that are used to communicate between the children and back to this class
     * @private
     */
    PIMPrototyper.prototype._setUpChildListeners = function () {
        try {
            var _this = this;

            this._prototypeImageView.on("loadImageClicked", this._showLoadImageDialog, this);
            this._screenControlsView.on("changeImageClicked", this._showLoadImageDialog, this);

            this._prototypeImageView.on("WidgetRegionDrawn", function(coord, region) {
                new WidgetConfigView({
                    screenCollection: _this._screens,
                    title: "New widget",
                    okText: "Create",
                })
                .on("cancel", function(data, view) {
                    view.remove();
                    d3.select(region.node().parentNode).remove();
                })
                .on("ok", function(data, view) {
                    _this._widgetManager.addNewWidget(data.data, coord, function(w) {
                        region.classed(w.type(), true).attr("id", w.id());
                        w.element(region);
                        w.createImageMap({
                            onClick: function(widget) {
                                _this._onWidgetClicked(widget);
                            },
                            map: _this._prototypeImageView.getImageMap()
                        });
                    });

                    view.remove();
                });
            });

            this._prototypeImageView.on("WidgetEditRequested", function(widgetID) {
                _this.editWidgetDialog(widgetID);
            });
            this._widgetsListView.on("WidgetEditRequested", function (widgetID) {
                _this.editWidgetDialog(widgetID);
            });
            this._prototypeImageView.on("WidgetSelected", function(widget, add) {
                _this._widgetsListView.selectWidget(widget, add);
            });
            this._widgetsListView.on("WidgetSelected", function(widget, add) {
                _this._prototypeImageView.selectWidget(widget, add);
            });
        } catch (pimprototyper_init_error) {
            console.error(pimprototyper_init_error);
        }
        return this;
    };

    PIMPrototyper.prototype.editWidgetDialog = function (widgetID) {
        var widget = this._widgetManager.getWidget(widgetID);
        var _this = this;

        new WidgetConfigView({
            screenCollection: this._screens,
            title: "Edit widget",
            okText: "Save",
            widget: widget
        })
        .on("cancel", function(data, view) {
            view.remove();
        })
        .on("ok", function(data, view) {
            widget.updateWithProperties(data.data);
            _this._widgetManager.trigger("WidgetModified");
            view.remove();
        });
        return this;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new PIMPrototyper();
            }
            return instance;
        }
    };
});
