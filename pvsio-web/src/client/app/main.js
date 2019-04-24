/**
 * Main entry point for pvsioweb module
 * @author Patrick Oladimeji
 * @date 4/19/13 17:23:31 PM
 */
/*jshint unused: false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true*/
/*global define, d3, layoutjs, Promise*/
define(function (require, exports, module) {
    "use strict";
    const PVSioWebClient = require("PVSioWebClient");
    const Logger = require("util/Logger");
    const ui = require("util/toolkitInterface");
    const ProjectManager = require("project/ProjectManager").getInstance();
    const PluginManager  = require("plugins/PluginManager");
    const Constants = require("util/Constants");
    const displayQuestion = require("pvsioweb/forms/displayQuestion");
    const ProjectAutoSaver = require("plugins/autoSaver/ProjectAutoSaver");
    const BrowserUtils   = require("util/BrowserUtils");
    const NotificationManager = require("project/NotificationManager");
    const SaveProjectChanges  = require("project/forms/SaveProjectChanges");

    let client = PVSioWebClient.getInstance();
    let pluginManager = PluginManager.getInstance();
    let splashTimeout = null;
    let reconnectOptions = (window.location.href.indexOf(".herokuapp.com") >= 0 
                            || window.location.href.indexOf("pvsioweb.org") >= 0) ? { silentMode: true} : null;

    //register event listeners
    client.addListener('WebSocketConnectionOpened', function (e) {
        ui.webSocketConnected();
    }).addListener("WebSocketConnectionClosed", function (e) {
        ui.webSocketDisconnected();
        ui.reconnectToServer(reconnectOptions);
    }).addListener("processExited", function (e) {
        ui.pvsProcessDisconnected();
    });
    ProjectManager.addListener("PVSProcessReady", function (event) {
        ui.pvsProcessConnected();
    }).addListener("PVSProcessDisconnected", function (event) {
        ui.pvsProcessDisconnected();
    });


    function hideSplash() {
        setTimeout(function () {
            d3.select("#PVSio-web-logo").style("display", "none");
        }, 500);
        d3.select("#PVSio-web-logo").transition().duration(500).style("opacity", 0);
        d3.selectAll(".toolkit-body").transition().duration(1000).style("opacity", 1);
    }

    function showInterface(opt) {
        return function (res) {
            return new Promise(function (resolve, reject) {
                console.log("Browser version: " + BrowserUtils.getVersion());
                console.log("Toolkit version: PVSio-web " + client.version());
                if (BrowserUtils.isBrowserSupported() === false) {
                    var msg = BrowserUtils.requiredBrowserWarning();
                    d3.select(".warnings").style("display", "block").append("p").html(msg);
                    d3.select(".warnings").select("#dismissWarnings").on("click", function () {
                        d3.select(".warnings").style("display", "none");
                    });
                    console.log(msg);
                }
                d3.select("#btnSaveProject").on("click", function () {
                    ProjectManager.saveProject({ filter: function (desc) { return desc.name.indexOf(".emdl") !== (desc.name.length - 5); }});
                    // FIXME: implement API plugin.saveAll in all plugins that saves all files relevant to each plugin, and invoke the APIs here.
                    var emulink = require("plugins/PluginManager").getInstance()
                                    .getEnabledPlugins().filter(function (p) {
                                        return p.getId() === "EmuChartsEditor";
                                    });
                    if (emulink && emulink[0]) {
                        emulink[0].saveAllCharts();
                    }
                });
                d3.select("#btnSaveProjectAs").on("click", function () {
                    if (d3.select("#btn_menuSaveChart").node()) {
                        d3.select("#btn_menuSaveChart").node().click();
                    }
                    var name = ProjectManager.project().name();
                    var date = (new Date().getFullYear()) + "." +
                                    (new Date().getMonth() + 1) + "." + (new Date().getDate());
                    if (!name.endsWith(date)) {
                        name += "_" + date;
                    }
                    ProjectManager.saveProjectDialog(name);
                });
                d3.select("#openProject").on("click", function () {
                    function openProject() {
                        ProjectManager.openProjectDialog().then(function (project) {
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
                    var currentProject = ProjectManager.project();
                    if (currentProject && currentProject._dirty()) {
                        //show save project dialog for the current project
                        SaveProjectChanges.create(currentProject)
                            .on("yes", function (e, view) {
                                view.remove();
                                ProjectManager.saveProject().then(function (res) {
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
                    ProjectManager.createProjectDialog().then(function (res) {
                        var notification = "Project " + res.project().name() + "created!";
                        Logger.log(notification);
                    });
                });                
                //hide pvsio-web loading screen if noSplash is set in opt and make the tool visible
                if (opt && opt.noSplash) {
                    hideSplash();
                    resolve(res);
                } else {
                    resolve(res);
                }
            });
        };
    }

    function loadUI() {
        ui.init();
    }

    module.exports = {
        start: function (opt) {
            clearTimeout(splashTimeout);
            return new Promise(function (resolve, reject) {
                client.connectToServer()
                    .then(loadUI)
                    .then(pluginManager.loadPlugins)
                    .then(ProjectManager.createDefaultProject)
                    .then(showInterface(opt))
                    .then(function (res) {
                    //we have finished loading pvsio-web related things so hide the splash screen and show the tool -- we can also use this promise chain to show real updates to the user about what has been loaded
                        hideSplash();
                        pluginManager.enablePlugin(ProjectAutoSaver.getInstance());
                        resolve(res);
                    }).catch(function (err) {
                        console.error(err);
                        reject(err);
                    });
            });
        }
    };
});
