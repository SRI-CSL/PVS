/**
 *
 * @author Patrick Oladimeji
 * @date 11/21/13 15:03:48 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise, layoutjs, d3*/
define(function (require, exports, module) {
    "use strict";
    var PVSioWebClient      = require("PVSioWebClient"),
        WSManager           = require("websockets/pvs/WSManager"),
        ProjectManager	    = require("project/ProjectManager"),
        WidgetManager       = require("pvsioweb/WidgetManager").getWidgetManager(),
        Logger              = require("util/Logger"),
        Recorder            = require("util/ActionRecorder"),
        Prompt              = require("pvsioweb/forms/displayPrompt"),
        PrototypeImageView  = require("pvsioweb/forms/PrototypeImageView"),
        WidgetsListView     = require("pvsioweb/forms/WidgetsListView"),
        TimersListView      = require("pvsioweb/forms/TimersListView"),
        NewWidgetView       = require("pvsioweb/forms/newWidget"),
        EditWidgetView      = require("pvsioweb/forms/editWidget"),
        template            = require("text!pvsioweb/forms/templates/prototypeBuilderPanel.handlebars"),
        toolbar             = require("text!pvsioweb/forms/templates/prototypeBuilderToolbar.handlebars"),
        ScriptPlayer        = require("util/ScriptPlayer"),
//        fs              = require("util/fileHandler"),
        PluginManager       = require("plugins/PluginManager"),
        FileSystem          = require("filesystem/FileSystem"),
        NotificationManager = require("project/NotificationManager"),
        SaveProjectChanges  = require("project/forms/SaveProjectChanges"),
        Descriptor          = require("project/Descriptor"),
        MIME                = require("util/MIME").getInstance();

    var instance;
    var currentProject,
        projectManager,
        pbContainer,
        pvsioWebClient;
    var _prototypeBuilder;
    var fs;
    var widgetListView;
    var prototypeImageView;

    function PrototypeBuilder() {
        pvsioWebClient = PVSioWebClient.getInstance();
        projectManager = ProjectManager.getInstance();
        currentProject = projectManager.project();
        fs = new FileSystem();
        _prototypeBuilder = this;
    }

    PrototypeBuilder.prototype.getName = function () {
        return "Prototype Builder";
    };

    PrototypeBuilder.prototype.getId = function () {
        return "PrototypeBuilder";
    };

    /**
     * Switches the prototoyping layer to the builder layer
     * @private
     */
    function switchToBuilderView() {
        d3.select("#prototype-builder-container .image-map-layer").style("opacity", 1).style("z-index", 190);
        d3.select("#btnBuilderView").classed("btn-default", false).classed("btn-primary", true).classed("active", true);
        d3.select("#btnSimulatorView").classed("btn-default", true).classed("btn-primary", false).classed("active", false);
        d3.select("#btnRebootPrototype").classed("disabled", true);
        WidgetManager.stopTimers();
        expandWidgetsList();
    }

    /** Switches the prototyping layer to the simulator/testing layer
        @private
    */
    function switchToSimulatorView() {
        d3.select("#prototype-builder-container .image-map-layer").style("opacity", 0.1).style("z-index", -2);
        d3.select("#btnBuilderView").classed("btn-default", true).classed("btn-primary", false).classed("active", false);
        d3.select("#btnSimulatorView").classed("btn-default", false).classed("btn-primary", true).classed("active", true);
        d3.select("#btnRebootPrototype").classed("disabled", false);
        console.log("bootstrapping widgets with init(0)...");
        WidgetManager.initialiseWidgets();
        console.log("bootstrapping wallclock timers...");
        WidgetManager.startTimers();
        collapseWidgetsList();
    }

    function updateControlsHeight() {
        d3.select("#builder-controls").style("height", d3.select("#prototype-builder-container").node().getBoundingClientRect().height + "px");
    }

    function updateImageAndLoadWidgets() {
        var p = projectManager.project();
        var image = p.getImage();
        WidgetManager.clearWidgets();

        if (prototypeImageView) {
            prototypeImageView.clearWidgetAreas();
        }

        d3.select("div#body").style("display", null);
        if (!image) {
            // remove previous image, if any
            if (prototypeImageView) {
                prototypeImageView.clearImage();
            }
            return Promise.resolve();
        }
        // else
        function updateImageDescriptor(content) {
            return new Promise(function (resolve, reject) {
                image.content = content;
                resolve(image);
            });
        }
        function showImage() {
            return new Promise(function (resolve, reject) {
                prototypeImageView.setImage(image).then(function (scale) {
                    updateControlsHeight();
                    prototypeImageView.updateMapCreator(scale, function () {
                        var wdStr = p.getWidgetDefinitionFile().content;
                        if (wdStr && wdStr !== "") {
                            var wd = JSON.parse(wdStr);
                            WidgetManager.restoreWidgetDefinitions(wd);
                            //update the widget area map scales
                            WidgetManager.scaleAreaMaps(scale);
                            //select prototype builder
                            switchToBuilderView();
                        }
                        resolve();
                    });
                }).catch(function (err) {
                    Logger.log(err);
                    reject(err);
                });
            });
        }
        return new Promise(function (resolve, reject) {
            image.loadContent().then(function (res) {
                updateImageDescriptor(res);
            }).then(function (res) {
                return showImage();
            }).then(function (res) {
                resolve(image);
            }).catch(function (err) {
                console.error(err);
                reject(err);
            });
        });
    }

    function onWidgetsFileChanged(event) {
        updateImageAndLoadWidgets().then(function (res) {
            widgetListView.update();
        }).catch(function (err) { Logger.error(err); });
        TimersListView.create(WidgetManager);
    }

    function collapseWidgetsList() {
        var width = d3.select("#builder-controls").node().getBoundingClientRect().width;
        if (width > 0) {
            widgetListView.width = width;
            d3.select("#builder-controls").transition().duration(300)
                .styleTween("width", function() { return d3.interpolateString(widgetListView.width + "px", "0px"); });
        }
    }
    function expandWidgetsList() {
        var width = d3.select("#builder-controls").node().getBoundingClientRect().width;
        if (width === 0) {
            d3.select("#builder-controls").transition().duration(300)
                .styleTween("width", function() { return d3.interpolateString("0px", widgetListView.width + "px"); });
        }
    }

    function restartPVSio() {
        var ws = WSManager.getWebSocket();
        ws.lastState("init(0)");
        var project = projectManager.project();
        if (project.mainPVSFile()) {
            // the main file can be in a subfolder: we need to pass information about directories!
            var mainFile = project.mainPVSFile().path.replace(project.name() + "/", "");
            ws.startPVSProcess({name: mainFile, projectName: project.name()}, function (err) {
                //make projectManager bubble the process ready event
                if (!err) {
                    projectManager.fire({type: "PVSProcessReady"});
                } else {
                    projectManager.fire({type: "PVSProcessDisconnected", err: err});
                }
            });
        }
    }

    function onProjectChanged(event) {
        var pvsioStatus = d3.select("#lblPVSioStatus");
        pvsioStatus.select("span").remove();
        restartPVSio();
        switchToBuilderView();
        WidgetManager.clearWidgets();
        prototypeImageView.clearWidgetAreas();
        ScriptPlayer.clearView();
        onWidgetsFileChanged(event);
    }

    function onSelectedFileChanged(event) {
        var desc = projectManager.project().getDescriptor(event.selectedItem.path);
        if (desc) {
            if (projectManager.project().mainPVSFile() && projectManager.project().mainPVSFile().path === desc.path) {
                d3.select("#btnSetMainFile").attr("disabled", true);
            } else {
                d3.select("#btnSetMainFile").attr("disabled", null);
            }
        }
    }

    function bindListeners() {
        var actions, recStartState, recStartTime, scriptName;
        /**
         * Add event listener for toggling the prototyping layer and the interaction layer
         */
        d3.select("#btnBuilderView").on("click", function () {
            switchToBuilderView();
        });
        d3.select("#btnSimulatorView").on("click", function () {
            var msg = "";
            if (!prototypeImageView.hasImage()) {
                msg = "Please load a user interface picture before switching to Simulator View.\n\n " +
                        "This can be done from within Builder View, using the \"Load Picture\" button.";
                return alert(msg);
            }
            if (!projectManager.project().mainPVSFile()) {
                msg = "Please set a Main File before switching to Simulator View.\n\n" +
                        "This can be done using Model Editor:\n" +
                        "  (i) select a file from the file browser shown on the right panel of the Model Editor\n" +
                        "  (ii) click on \"Set as Main File\" to set the selected file as Main File.";
                return alert(msg);
            }
            switchToSimulatorView();
        });
        d3.select("#btnSaveProject").on("click", function () {
            projectManager.saveProject().then(function (res) {
                // FIXME: Implement APIs to save the diagram in the EmuCharts module!
                if (d3.select("#btn_menuSaveChart").node()) {
                    d3.select("#btn_menuSaveChart").node().click();
                }
            });
        });
        d3.select("#btnSaveProjectAs").on("click", function () {
            if (d3.select("#btn_menuSaveChart").node()) {
                d3.select("#btn_menuSaveChart").node().click();
            }
            var name = projectManager.project().name();
            var date = (new Date().getFullYear()) + "." +
                            (new Date().getMonth() + 1) + "." + (new Date().getDate());
            if (!name.endsWith(date)) {
                name += "_" + date;
            }
            projectManager.saveProjectDialog(name);
        });
        d3.select("#openProject").on("click", function () {
            function openProject() {
                projectManager.openProjectDialog().then(function (project) {
                    var notification = "Project " + project.name() + " opened successfully!";
                    Logger.log(notification);
                }).catch(function (err) {
                    if (err && err.error) {
                        NotificationManager.error(err.error);
                    } else {
                        Logger.log(JSON.stringify(err));
                    }
                });
            }
            var currentProject = projectManager.project();
            if (currentProject && currentProject._dirty()) {
                //show save project dialog for the current project
                SaveProjectChanges.create(currentProject)
                    .on("yes", function (e, view) {
                        view.remove();
                        projectManager.saveProject().then(function (res) {
                            openProject();
                        }).catch(function (err) { alert(err); });
                    }).on("no", function (e, view) {
                        view.remove();
                        openProject();
                    });
            } else {
                openProject();
            }
        });
        d3.select("#newProject").on("click", function () {
            projectManager.createProjectDialog().then(function (res) {
                var notification = "Project " + res.project().name() + "created!";
                Logger.log(notification);
            });
        });
        d3.select("#btnLoadPicture").on("click", function () {
            prototypeImageView.onClickLoad();
        });
        d3.select("#btnRecord").on("click", function () {
            if (!d3.select("#btnRecord").attr("active")) {
                d3.select("#btnRecord")
                    .attr("active", "true")
                    .style("cursor", "default")
                    .style("color", "grey");
                d3.select("#btnStop")
                    .style("cursor", "pointer")
                    .style("color", "rgb(8, 88, 154)");
                Recorder.startRecording();
                recStartState = WSManager.getWebSocket().lastState();
                recStartTime = new Date().getTime();
                scriptName = "Script_" + recStartTime;
            }
        });
        d3.select("#btnStop").on("click", function () {
            if (d3.select("#btnRecord").attr("active")) {
                d3.select("#btnRecord")
                    .attr("active", null)
                    .style("color", "red")
                    .style("cursor", "pointer");
                d3.select("#btnStop")
                    .style("cursor", "default")
                    .style("color", "grey");
                actions = Recorder.stopRecording();
                //do something with actions
                Logger.log(actions);
                //ask user to give name to script
                Prompt.create({header: "Would you like to save this script?",
                               message: "Please enter a name for your script",
                               buttons: ["Cancel", "Save"]})
                    .on("save", function (e, view) {
                        scriptName = e.data.prompt.trim() || scriptName;
                        view.remove();
                        var script = {name: scriptName, actions: actions, startState: recStartState};
                        //add the script to the project
                        projectManager.project().addScript(script);
                    }).on("cancel", function (e, view) {
                        view.remove();
                    });
            }
        });
        d3.select("#btnReconnect").on("click", function () {
            projectManager.reconnectToServer();
        });
        d3.select("#btnRebootPrototype").on("click", function (){
            //reboot is emulated by restarting the pvsioweb process on the server
            restartPVSio();
            switchToSimulatorView();
        });
        d3.select("#btnAddNewTimer").on("click", function () {
//            WidgetManager.addTimer();
        });
        d3.select("#btnAddNewWidget").on("click", function () {

        });
    }

    /////These are the api methods that the prototype builder plugin exposes
    PrototypeBuilder.prototype.getDependencies = function () { return []; };

    PrototypeBuilder.prototype.updateImageAndLoadWidgets = function () {
        return updateImageAndLoadWidgets();
    };

    /**
        Change the image in the current project to the one specified in the parameter
        @param {string} imagePath image name, including path name (given as relative path, where the base path is the project name)
        @param {string} imageData base64 encoded data
        @returns {Promise} a promise that resolves when the image change process has completed
    */
    PrototypeBuilder.prototype.changeImage = function (imagePath, imageData) {
        return new Promise(function (resolve, reject) {
            var pm = projectManager, project = projectManager.project();
            var oldImage = project.getImage(),
                newImage = new Descriptor(imagePath, imageData, { encoding: "base64" });
            function done() {
                var token = {
                    type: "changeProjectSetting",
                    projectName: project.name(),
                    key: "prototypeImage",
                    value: newImage.path
                };
                WSManager.getWebSocket().send(token, function (err) {
                    //if there was no error update the main file else alert user
                    if (!err) {
                        //project.prototypeImage = newImage;
                        project.prototypeImage = new Descriptor(project + "/" + imagePath, imageData, { encoding: "base64" }); //FIXME: in the current implementation project.prototypeImage needs to start with the project name -- we need to check whether this is actually needed, if not we should remove this prefix as this makes things easier when renaming projects.
                        resolve({
                            path: newImage.path,
                            content: newImage.content
                        });
                    } else {
                        Logger.log(err);
                        reject(err);
                    }
                });
            }
            if (oldImage) {
                pm.project().removeFile(oldImage.path).then(function (res) {
                    pm.project().addFile(newImage.path, newImage.content, { encoding: "base64", overWrite: true })
                        .then(function (res) {
                            done();
                        }).catch(function (err) { console.log(err); reject(err); });
                    }).catch(function (err) { console.log(err); reject(err); });
            } else {
                pm.project().addFile(newImage.path, newImage.content, { encoding: "base64", overWrite: true }).then(function (res) {
                    done();
                }).catch(function (err) { console.log(err); reject(err); });
            }
        });
    };

    /**
     * @function preparePageForUmageUpload
     * @description Sets up the handlers for dealing with the user choosing to change the prototype image
     * @memberof module:PrototypeBuilder
     * @instance
     */
    var preparePageForImageUpload = function () {
        // FIXME: dont rely on extensions, use a "type" field in the Descriptor
        // to specify whether the file is an image or a text file

        prototypeImageView.on('loadImageClicked', function() {
            return new Promise(function (resolve, reject) {
                if (PVSioWebClient.getInstance().serverOnLocalhost()) {
                    fs.readFileDialog({
                        encoding: "base64",
                        title: "Select a picture",
                        filter: MIME.imageFilter
                    }).then(function (descriptors) {
                        _prototypeBuilder.changeImage(descriptors[0].name, descriptors[0].content).then(function (res) {
                            prototypeImageView.setImage(res).then(function (res) {
                                updateControlsHeight();
                                if (d3.select("#imageDiv svg").node() === null) {
                                    // we need to create the svg layer, as it's not there
                                    // this happens when a new project is created without selecting an image
                                    prototypeImageView.updateMapCreator();
                                }
                                resolve(res);
                            });
                        });
                    }).catch(function (err) { reject(err); });
                } else {
                    d3.select("#btnSelectPicture").node().click();
                    resolve();
                }
            });
        });

        function _updateImage(file) {
            return new Promise(function (resolve, reject) {
                fs.readLocalFile(file).then(function (res) {
                    _prototypeBuilder.changeImage(res.name, res.content).then(function (res) {
                        prototypeImageView.setImage(res).then(function (res) {
                            updateControlsHeight();
                            resolve(res);
                        });
                    });
                }).catch(function (err) { reject(err); });
            });
        }

        prototypeImageView.on('imageDropped', function(file) {
            _updateImage(file);
        });

        d3.selectAll("#btnEditStoryboard").on("click", function () {
            WidgetManager.displayEditStoryboardDialog();
        });

        d3.select("#btnSelectPicture").on("change", function () {
            var file = d3.event.currentTarget.files[0];
            if (file && projectManager.isImage(file.name)) {
                _updateImage(file).then(function (res) {
                    if (d3.select("#imageDiv svg").node() === null) {
                        // we need to create the svg layer, as it's not there
                        // this happens when a new project is created without selecting an image
                        prototypeImageView.updateMapCreator();
                    }
                });
            }
        });
    };

    /**
     * Sets up listeners on child views that are used to communicate between the children and back to this class
     * @private
     */
    PrototypeBuilder.prototype._setUpChildListeners = function() {
        prototypeImageView.on("WidgetRegionDrawn", function(coord, region) {
            NewWidgetView.create(coord)
                .on("ok", function (e, view) {
                    view.remove();
                    e.data.scale = prototypeImageView.resize();
                    var widget = WidgetManager.addNewWidget(e.data, coord, function(w, renderResponse) {
                        region.classed(w.type(), true).attr("id", w.id());
                        w.element(region);
                        if (w.needsImageMap()) {
                            w.createImageMap({ callback: renderResponse });
                        }
                        // w.updateLocationAndSize({ x: e.data.x, y: e.data.y, width: e.data.width, height: e.data.height }, { imageMap: true });
                        // w.updateStyle(e.data);
                    });
                    widget.renderSample({ visibleWhen: "true" });

                    widgetListView.selectWidget(widget, false);
                }).on("cancel", function (e, view) {
                    view.remove();
                    d3.select(region.node().parentNode).remove();
                });
        });

        prototypeImageView.on("WidgetEditRequested", function(widgetID) {
            var widget = WidgetManager.getWidget(widgetID);
            EditWidgetView.create(widget)
                .on("ok", function (e, view) {
                    view.remove();
                    WidgetManager.editWidget(widget, e.data);
                }).on("cancel", function (e, view) {
                    // remove dialog
                    view.remove();
                });
        });

        prototypeImageView.on("WidgetSelected", function(widget, add) {
            widgetListView.selectWidget(widget, add);
        });

        widgetListView.on("WidgetSelected", function(widget, add) {
            prototypeImageView.selectWidget(widget, add);
        });
    };

    /**
        @returns {Promise} a promise that resolves when the prototype builder has been initialised
    */
    PrototypeBuilder.prototype.initialise = function () {
        var _this = this;
        this.collapsed = false;
        pbContainer = pvsioWebClient.createCollapsiblePanel({
            headerText: this.getName(),
            showContent: !this.collapsed,
            ownerObject: this,
            onClick: function (collapsed) {
                if (pbContainer.style("display") !== "none" && pbContainer.select("svg").empty()) {
                    updateImageAndLoadWidgets();
                }

                _this.collapsed = collapsed;
            },
            parent: "#body",
            owner: this.getId()
        });
        pbContainer.append("div").html(toolbar);
        pbContainer.append("div").attr("style", "display: flex;").html(template);
        layoutjs({el: "#body"});
        projectManager.addListener("ProjectChanged", onProjectChanged);
        projectManager.addListener("WidgetsFileChanged", onWidgetsFileChanged);
        projectManager.addListener("SelectedFileChanged", onSelectedFileChanged);
        updateImageAndLoadWidgets().then(function (res) {
            widgetListView.update();
        }).catch(function (err) { Logger.error(err); });

        // add default tick timer
        WidgetManager.addWallClockTimer();
        //TimersListView.create();

        // Create child views
        widgetListView = new WidgetsListView({
            el: $("#widgetsList"),
            widgetManager: WidgetManager,
            labelFunction: function (widget) {
                switch (widget.type()) {
                    case "display"       : { return "Display: " + widget.displayKey(); }
                    case "numericdisplay": { return "Numeric Display: " + widget.displayKey(); }
                    case "led"           : { return "LED: " + widget.ledKey(); }
                    case "button"        : { return "Button: " + widget.functionText(); }
                    case "touchscreenbutton": { return "Touch-Button: " + widget.functionText(); }
                    case "touchscreendisplay": { return "Touch-Display: " + widget.displayKey(); }
                    default: { return widget.type() + ": " + widget.id(); }
                }
            }
        });

        prototypeImageView = new PrototypeImageView({
            el: $("#imageDiv"),
            widgetManager: WidgetManager,
            mapID: "prototypeMap"
        });

        pbContainer.select(".ljs-hcontent").on("resize", function () {
            prototypeImageView.resize();
        });

        preparePageForImageUpload();
        this._setUpChildListeners();
        bindListeners();
        d3.select("#header #txtProjectName").html(projectManager.project().name());
        return updateImageAndLoadWidgets();
    };

    PrototypeBuilder.prototype.unload = function () {
        widgetListView.remove();
        prototypeImageView.remove();
        pvsioWebClient.removeCollapsiblePanel(pbContainer);
        projectManager.removeListener("ProjectChanged", onProjectChanged);
        projectManager.removeListener("WidgetsFileChanged", onWidgetsFileChanged);
        projectManager.removeListener("SelectedFileChanged", onSelectedFileChanged);
        require("widgets/ButtonHalo").getInstance().removeKeypressHandlers();
        return Promise.resolve(true);
    };

    PrototypeBuilder.prototype.handleKeyDownEvent = function (e) {
        if (PluginManager.getInstance().isLoaded(this) && !this.collapsed) {
            prototypeImageView._mapCreator.handleKeyDownEvent(e);
            require("widgets/ButtonHalo").getInstance().handleKeyDownEvent(e);
        }
    };

    PrototypeBuilder.prototype.handleKeyUpEvent = function (e) {
        if (PluginManager.getInstance().isLoaded(this) && !this.collapsed) {
            require("widgets/ButtonHalo").getInstance().handleKeyUpEvent(e);
        }
    };
    /**
        Gets an instance of the project manager
        @deprecated use ProjectManager.getInstance()
    */
    PrototypeBuilder.prototype.getProjectManager = function () {
        console.log("deprecated call to PrototypeBuilder.getProjectManager() use ProjectManager.getInstance() instead");
        return projectManager;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new PrototypeBuilder();
            }
            return instance;
        }
    };
});
