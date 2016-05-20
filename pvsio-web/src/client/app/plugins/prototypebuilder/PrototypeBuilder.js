/**
 *
 * @author Patrick Oladimeji
 * @date 11/21/13 15:03:48 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise, layoutjs, d3*/
define(function (require, exports, module) {
    "use strict";
    var PVSioWebClient  = require("PVSioWebClient"),
        WSManager       = require("websockets/pvs/WSManager"),
        ProjectManager	= require("project/ProjectManager"),
        WidgetManager   = require("pvsioweb/WidgetManager").getWidgetManager(),
        Logger          = require("util/Logger"),
        Recorder        = require("util/ActionRecorder"),
        Prompt          = require("pvsioweb/forms/displayPrompt"),
        WidgetsListView = require("pvsioweb/forms/WidgetsListView"),
        TimersListView  = require("pvsioweb/forms/TimersListView"),
        template		= require("text!pvsioweb/forms/templates/prototypeBuilderPanel.handlebars"),
        ScriptPlayer    = require("util/ScriptPlayer"),
//        fs              = require("util/fileHandler"),
        FileSystem          = require("filesystem/FileSystem"),
        NotificationManager = require("project/NotificationManager"),
        SaveProjectChanges = require("project/forms/SaveProjectChanges"),
        Descriptor      = require("project/Descriptor"),
        MIME            = require("util/MIME").getInstance();

    var instance;
    var currentProject,
        projectManager,
        pbContainer,
        pvsioWebClient;
    var img; // this is the prototype image displayed in the PVSio-web user interface
    var _prototypeBuilder;
    var fs;

    function PrototypeBuilder() {
        pvsioWebClient = PVSioWebClient.getInstance();
        projectManager = ProjectManager.getInstance();
        currentProject = projectManager.project();
        img = null;
        fs = new FileSystem();
        _prototypeBuilder = this;
    }

    PrototypeBuilder.prototype.getName = function () {
        return "Prototype Builder";
    };

    /**
     * @function renderImage
     * @description Updates the project image with in the prototype builder
     * @param image {Descriptor} Descriptor of the prototype picture.
     * @returns {Promise(real)} A Promise that resolves to a real value that specifies the scale of the rendered image
     * @memberof module:ProjectManager
     * @private
     */
    var renderImage = function (image) {
        return new Promise(function (resolve, reject) {
            function imageLoadComplete(res) {
                //if the image width is more than the the containing element scale it down a little
                var parent = d3.select("#prototype-builder-container"),
                    scale = 1;
                function resize() {
                    if (img) {
                        var pbox = parent.node().getBoundingClientRect(),
                            adjustedWidth = img.width,
                            adjustedHeight = img.height;
                        scale = 1;

                        if (img.width > pbox.width && pbox.width > 0 && pbox.height > 0) {
                            adjustedWidth = pbox.width;
                            scale = adjustedWidth / img.width;
                            adjustedHeight = scale * img.height;
                        }

                        d3.select("#imageDiv").style("width", adjustedWidth + "px").style("height", adjustedHeight + "px");
                        d3.select("#imageDiv img").attr("src", img.src).attr("height", adjustedHeight).attr("width", adjustedWidth);
                        d3.select("#imageDiv svg").attr("height", adjustedHeight).attr("width", adjustedWidth);
                        d3.select("#imageDiv svg > g").attr("transform", "scale(" + scale + ")");
                        //hide the draganddrop stuff
                        d3.select("#imageDragAndDrop.dndcontainer").style("display", "none");

                        //update widgets maps after resizing
                        d3.select("#builder-controls").style("height", (50 + adjustedHeight) + "px");
                        WidgetManager.scaleAreaMaps(scale);
                    }
                }
                if (parent.node()) {
                    resize();
                    parent.node().addEventListener("resize", resize);
                }
                resolve(scale);
            }

            img = new Image();
            img.onload = imageLoadComplete;
            img.onerror = function (res) {
                alert("Failed to load picture " + image.name);
                reject(res);
            };
            img.name = image.path;
            img.src = image.content;
        });
    };

    function updateImageAndLoadWidgets() {
        var p = projectManager.project();
        var image = p.getImage();
        WidgetManager.clearWidgetAreas();
        d3.select("div#body").style("display", null);
        if (!image) {
            // remove previous image, if any
            d3.select("#imageDiv img").attr("src", "").attr("height", "430").attr("width", "1128");
            d3.select("#imageDiv svg").attr("height", "0").attr("width", "0");
            d3.select("#imageDiv").attr("style", "");
            d3.select("#body").attr("style", "height: 480px"); // 430 + 44 + 6
            //show the image drag and drop div
            d3.select("#imageDragAndDrop.dndcontainer").style("display", null);
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
                renderImage(image).then(function (scale) {
                    WidgetManager.updateMapCreator(scale, function () {
                        var wdStr = p.getWidgetDefinitionFile().content;
                        if (wdStr && wdStr !== "") {
                            var wd = JSON.parse(p.getWidgetDefinitionFile().content);
                            WidgetManager.restoreWidgetDefinitions(wd);
                            //update the widget area map scales
                            WidgetManager.scaleAreaMaps(scale);
                            //select prototype builder
                            switchToBuilderView();
                        }
                        resolve();
                    });
                    d3.select("#imageDragAndDrop.dndcontainer").style("display", "none");
                }).catch(function (err) {
                    Logger.log(err);
                    //show the image drag and drop div
                    d3.select("#imageDragAndDrop.dndcontainer").style("display", null);
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
                console.log(err);
                reject(err);
            });
        });
    }


    /**
     * Switches the prototoyping layer to the builder layer
     * @private
     */
    function switchToBuilderView() {
        d3.select(".image-map-layer").style("opacity", 1).style("z-index", 190);
        d3.selectAll("#controlsContainer button, div.display").classed("selected", false);
        d3.select("#controlsContainer .active").classed("active", false);
        d3.select("#btnBuilderView").classed('active', true);
        d3.selectAll("div.display,#controlsContainer button").classed("builder", true);
        d3.selectAll("div.display,#controlsContainer button").classed("simulator", false);
        WidgetManager.stopTimers();
    }
    function onProjectChanged(event) {
        var pvsioStatus = d3.select("#lblPVSioStatus");
        pvsioStatus.select("span").remove();
        var project = event.current;
        var ws = WSManager.getWebSocket();
        ws.lastState("init(0)");
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

        switchToBuilderView();
        WidgetManager.clearWidgetAreas();
        ScriptPlayer.clearView();
        updateImageAndLoadWidgets().then(function (res) {
            WidgetsListView.create();
        }).catch(function (err) { Logger.error(err); });
        TimersListView.create();
    }

    function onWidgetsFileChanged(event) {
        updateImageAndLoadWidgets().then(function (res) {
            WidgetsListView.create();
        }).catch(function (err) { Logger.error(err); });
        TimersListView.create();
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
    /** Switches the prototyping layer to the simulator/testing layer
        @private
    */
    function switchToSimulatorView() {
        d3.select(".image-map-layer").style("opacity", 0.1).style("z-index", -2);
        d3.selectAll("#controlsContainer button, div.display").classed("selected", false);
        d3.select("#controlsContainer .active").classed("active", false);
        d3.select("#btnSimulatorView").classed("active", true);
        d3.select("#btnSimulatorView").classed("selected", true);
        d3.selectAll("div.display,#controlsContainer button").classed("simulator", true);
        d3.selectAll("div.display,#controlsContainer button").classed("builder", false);
        WidgetManager.startTimers();
    }

    function bindListeners() {
        var actions, recStartState, recStartTime, scriptName;
        /**
         * Add event listener for toggling the prototyping layer and the interaction layer
         */
        d3.select("#btnBuilderView").classed("selected", true).on("click", function () {
            switchToBuilderView();
        });
        d3.select("#btnSimulatorView").on("click", function () {
            var img = d3.select("#imageDiv img");
            var msg = "";
            if (!img || !img.attr("src") || img.attr("src") === "") {
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
            var name = projectManager.project().name() + "_" + (new Date().getFullYear()) + "." +
                            (new Date().getMonth() + 1) + "." + (new Date().getDate());
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
        d3.select("#btnAddNewTimer").on("click", function () {
//            WidgetManager.addTimer();
        });
        d3.select("#btnAddNewWidget").on("click", function () {
            
        });
    }

    /////These are the api methods that the prototype builder plugin exposes
    PrototypeBuilder.prototype.getDependencies = function () { return []; };


    /**
        Change the image in the current project to the one specified in the parameter
        @param {string} imagePath
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
                        project.prototypeImage = newImage;
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
                    pm.project().addFile(newImage.path, newImage.content, { encoding: "base64", overWrite: true });
                }).then(function (res) {
                    done();
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
     * @description ...
     * @memberof module:ProjectManager
     * @instance
     */
    var preparePageForImageUpload = function () {
        // FIXME: dont rely on extensions, use a "type" field in the Descriptor
        // to specify whether the file is an image or a text file

        // add listener for upload button
        d3.selectAll("#btnLoadPicture").on("click", function () {
            return new Promise(function (resolve, reject) {
                if (PVSioWebClient.getInstance().serverOnLocalhost()) {
                    fs.readFileDialog({
                        encoding: "base64",
                        title: "Select a picture",
                        filter: MIME.imageFilter
                    }).then(function (descriptors) {
                        _prototypeBuilder.changeImage(descriptors[0].name, descriptors[0].content).then(function (res) {
                            renderImage(res).then(function (res) {
                                if (d3.select("#imageDiv svg").node() === null) {
                                    // we need to create the svg layer, as it's not there
                                    // this happens when a new project is created without selecting an image
                                    WidgetManager.updateMapCreator();
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
        d3.selectAll("#btnEditStoryboard").on("click", function () {
            WidgetManager.displayEditStoryboardDialog();
        });

        function _updateImage(file) {
            return new Promise(function (resolve, reject) {
                fs.readLocalFile(file).then(function (res) {
                    _prototypeBuilder.changeImage(res.name, res.content).then(function (res) {
                        renderImage(res).then(function (res) {
//                            projectManager.project().getImage();
                            resolve(res);
                        });
                    });
                }).catch(function (err) { reject(err); });
            });
        }

        d3.select("#btnSelectPicture").on("change", function () {
            var file = d3.event.currentTarget.files[0];
            if (file && projectManager.isImage(file.name)) {
                _updateImage(file).then(function (res) {
                    if (d3.select("#imageDiv svg").node() === null) {
                        // we need to create the svg layer, as it's not there
                        // this happens when a new project is created without selecting an image
                        WidgetManager.updateMapCreator();
                    }
                });
            }
        });

        var c = document.getElementById("imageDiv");
        c.ondragover = function () {
            d3.select(c).style("border", "5px dashed black");
            return false;
        };
        c.ondragend = function (e) {
            d3.select(c).style("border", null);
            e.preventDefault();
            e.stopPropagation();
            return false;
        };
        c.ondrop =  function (e) {
            d3.select(c).style("border", null);
            var file = e.dataTransfer.files[0];
            _updateImage(file);
            e.preventDefault();
            e.stopPropagation();
            return false;
        };
    };

    /**
        @returns {Promise} a promise that resolves when the prototype builder has been initialised
    */
    PrototypeBuilder.prototype.initialise = function () {
        pbContainer = pvsioWebClient.createCollapsiblePanel({
            headerText: "Prototype Builder",
            showContent: true,
            onClick: function () {
                if (pbContainer.style("display") !== "none" && pbContainer.select("svg").empty()) {
                    updateImageAndLoadWidgets();
                }
            },
            parent: "#body",
            owner: this.getName()
        });
        pbContainer.html(template);
        layoutjs({el: "#body"});
        preparePageForImageUpload();
        projectManager.addListener("ProjectChanged", onProjectChanged);
        projectManager.addListener("WidgetsFileChanged", onWidgetsFileChanged);
        projectManager.addListener("SelectedFileChanged", onSelectedFileChanged);
        updateImageAndLoadWidgets().then(function (res) {
            WidgetsListView.create();
        }).catch(function (err) { Logger.error(err); });
        
        // add default tick timer
        WidgetManager.addWallClockTimer();
        //TimersListView.create();

        bindListeners();
        d3.select("#header #txtProjectName").html(projectManager.project().name());
        return updateImageAndLoadWidgets();
    };

    PrototypeBuilder.prototype.unload = function () {
        pvsioWebClient.removeCollapsiblePanel(pbContainer);
        projectManager.removeListener("ProjectChanged", onProjectChanged);
        projectManager.removeListener("WidgetsFileChanged", onWidgetsFileChanged);
        projectManager.removeListener("SelectedFileChanged", onSelectedFileChanged);
        return Promise.resolve(true);
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
