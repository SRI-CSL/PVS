/**
 * user interface tests
 * @author Patrick Oladimeji
 * @date 6/25/14 20:07:07 PM
 */
/*jshint unused: false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global d3, $, it, expect, define, describe, beforeEach, afterAll, beforeAll, spyOn, Promise, Event*/
define(function (require, exports, module) {
    "use strict";
    var main = require("main"),
        util = require("./UIUtil"),
        ProjectManager = require("project/ProjectManager");

    module.exports = {
        run: function () {
            var pm = ProjectManager.getInstance(),
                p;

            describe("User interface Elements", function () {
                beforeEach(function (done) {
                    d3.select("div.overlay").remove();
                    pm = ProjectManager.getInstance();
                    main.start({noSplash: true}).then(function () {
                        p = pm.project();
                        done();
                    }).catch(util.expectError(done));
                });

                it("Open Project dialog can be opened", function (done) {
                    util.pressingButtonOpensDialog("#openProject", "Open Project", done);
                });

                it("Open project dialog can be dismissed", function (done) {
                    util.dialogCanBeDismissed("#openProject", "Open Project", done);
                });

                it("New Project dialog can be opened", function (done) {
                    util.pressingButtonOpensDialog("#newProject", "New Project", done);
                });

                it("New project dialog can be dismissed", function (done) {
                    util.dialogCanBeDismissed("#newProject", "New Project", done);
                });

                it("Import files dialog can be opened", function (done) {
                    util.loadPlugin("ModelEditor").then(function () {
                        util.pressingButtonOpensDialog("#btnImportFiles", "Import files into Project", done);
                    }).catch(util.expectError(done));
                });

                it("Import files dialog can be dismissed", function (done) {
                    util.loadPlugin("ModelEditor").then(function () {
                        util.dialogCanBeDismissed("#btnImportFiles", "Import files into Project", done);
                    }).catch(util.expectError(done));
                });

                it("AlarisGP project can be opened", function (done) {
                    util.openSampleProject("AlarisGP").then(function () {
                        expect(pm.project().name()).toEqual("AlarisGP");
                        done();
                    }).catch(util.expectError(done));
                });

                it("AlarisPC_PumpModules project can be opened", function (done) {
                    util.openSampleProject("AlarisPC_PumpModules").then(function () {
                        expect(pm.project().name()).toEqual("AlarisPC_PumpModules");
                        done();
                    }).catch(util.expectError(done));
                });

                it("SmithsMedical_MedFusion3500 project can be opened", function (done) {
                    util.openSampleProject("SmithsMedical_MedFusion3500").then(function () {
                        expect(pm.project().name()).toEqual("SmithsMedical_MedFusion3500");
                        done();
                    }).catch(util.expectError(done));
                });

                it("EmuCharts tool can be loaded", function (done) {
                    util.loadPlugin("EmuChartsEditor").then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='EmuChartsEditor']");
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(util.expectError(done));
                });

                it("EmuCharts tool can be unloaded", function (done) {
                    var p = "EmuChartsEditor";
                    util.unloadPlugin(p)
                        .then(function () {
                            var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='EmuChartsEditor']");
                            expect(pluginPanel.empty()).toEqual(true);
                            done();
                        }).catch(util.expectError(done));
                });

                it("Model Editor tool can be loaded", function (done) {
                    var p = "ModelEditor";
                    util.loadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='ModelEditor']");
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(util.expectError(done));
                });

                it("Model Editor tool can be unloaded", function (done) {
                    var p = "ModelEditor";
                    util.unloadPlugin(p)
                        .then(function () {
                            var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='{ModelEditor}']");
                            expect(pluginPanel.empty()).toEqual(true);
                            done();
                        }).catch(util.expectError(done));
                });

                it("State Machine Viewer tool can be loaded", function (done) {
                    var p = "StateMachineViewer";
                    util.loadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='StateMachineViewer']");
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(util.expectError(done));
                });

                it("State Machine Viewer tool can be unloaded", function (done) {
                    var p = "StateMachineViewer";
                    util.unloadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='StateMachineViewer']");
                        expect(pluginPanel.empty()).toEqual(true);
                        done();
                    }).catch(util.expectError(done));
                });
            });

            describe("Prototype Builder", function () {
                var pluginId = "PrototypeBuilder", editorPanel;

                beforeEach(function (done) {
                    d3.select("div.overlay").remove();
                    pm = ProjectManager.getInstance();
                    main.start({noSplash: true}).then(function () {
                        p = pm.project();
                        editorPanel = "div.collapsible-panel-parent[plugin-owner='PrototypeBuilder'] .collapsible-panel";
                        done();
                    }).catch(util.expectError(done));
                });

                it("can be collapsed", function (done) {
                    util.togglePanel(pluginId)()
                        .then(function () {
                            expect(d3.select(editorPanel).style("display")).toEqual("none");
                            done();
                        }).catch(util.expectError(done));
                });

                it("can be collapsed and expanded", function (done) {
                    util.togglePanel(pluginId)()
                        .then(util.togglePanel(pluginId))
                        .then(function () {
                            expect(d3.select(editorPanel).style("display")).toEqual("block");
                            done();
                        }).catch(util.expectError(done));
                });

                describe("Editor File Lists", function () {
                    it("has context menus", function (done) {
                        util.loadPlugin("ModelEditor").then(function () {
                            util.togglePanel(pluginId)()
                                .then(util.rightclick("#pvsFiles"))
                                .then(function () {
                                    expect(d3.select("div.contextmenu").empty()).toBeFalsy();
                                    done();
                                });
                            }).catch(util.expectError(done));
                    });
                });
            });

            describe("FileSystem management in ListView", function () {
                beforeEach(function (done) {
                    d3.select("div.overlay").remove();
                    pm = ProjectManager.getInstance();
                    main.start({noSplash: true}).then(function () {
                        setTimeout(done, 0);//using a timeout to push this to the end of the event queue so any files are added and project is ready before performing tests
                    });
                });

                it("can add files to the project", function (done) {
                    var filesLength = pm.project().pvsFilesList().length;
                    util.loadPlugin("ModelEditor")
                        .then(util.listViewContextMenu("#pvsFiles", "#newfile"))
                        .then(function () {
                            setTimeout(function () {
                                expect(pm.project().pvsFilesList().length).toEqual(filesLength + 1);
                                var desc = pm.project().pvsFilesList()[pm.project().pvsFilesList().length - 1];
                                expect(desc.path.indexOf(pm.project().name()) === 0).toBeTruthy();
                                done();
                            }, 1000);
                        }).catch(util.expectError(done));
                });

                it("can remove files from the project", function (done) {
                    var filesLength = pm.project().pvsFilesList().length;
                    util.loadPlugin("ModelEditor")
                        .then(util.listViewContextMenu("#pvsFiles", "#newfile"))
                        .then(util.click("#pvsFiles li:last-child"))
                        .then(util.listViewContextMenu("#pvsFiles", "#delete"))
                        .then(util.click("div.overlay #btnOk"))
                        .then(function () {
                            setTimeout(function () {
                                expect(pm.project().pvsFilesList().length).toEqual(filesLength);
                                done();
                            }, 500);
                        }).catch(util.expectError(done));
                });
            });

        }
    };
});
