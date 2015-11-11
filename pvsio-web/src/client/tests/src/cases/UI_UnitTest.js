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
        ProjectManager			= require("project/ProjectManager");

    module.exports = {
        run: function () {
            var pm = ProjectManager.getInstance(),
                p;
            function wait(milliseconds) {
                return function () {
                    return new Promise(function (resolve, reject) {
                        setTimeout(function () {
                            resolve("time up");
                        }, milliseconds);
                    });
                };
            }

            function click(elid) {
                return function () {
                    var el = d3.select(elid).node();
                    return new Promise(function (resolve, reject) {
                        if (el) {
                            el.click();
                            setTimeout(function () {
                                resolve(elid);
                            }, 500);
                        } else {
                            reject(elid + " does not exist in the dom");
                        }
                    });
                };
            }

            function rightclick(el) {
                return function () {
                    var element = d3.select(el).node();
                    var event = new Event("contextmenu");
                    element.dispatchEvent(event);
                    return new Promise(function (resolve) {
                        setTimeout(function () {
                            resolve(event);
                        }, 200);
                    });
                };
            }
            /**
                Utility function to select context menu item (i.e., right click menu) on an element
                @param {string} el html element selector (e.g., class, tag or id) for the element
                    recieving the right click
                @param {string} menu the html element selector for the menu item to select
                @returns {Promise} a promise that is settled when the context menu item has been clicked
            */
            function listViewContextMenu(el, menu) {
                return function () {
                    el = el || "#pvsFiles";
                    return rightclick(el)()
                        .then(click(".contextmenu " + menu));
                };
            }

            function expectError(done) {
                return function (err) {
                    expect(err).toBeFalsy();
                    done();
                };
            }

            function dialogCanBeDismissed(btnTrigger, title, done) {
                title = title || "dialog triggered by " + btnTrigger;
                var str = title + " can be dismissed",
                    clickbutton = click(btnTrigger);
                clickbutton()
                    .then(click("div.overlay #btnCancel"))
                    .then(function () {
                        //expect overlay to have disappeared after clicking cancel
                        expect($("div.overlay").length).toEqual(0);
                        done();
                    }).catch(expectError(done));
            }

            function pressingButtonOpensDialog(btnTrigger, title, done) {
                title = title || "dialog triggered by " + btnTrigger;
                var str = title + " is opened by clicking " + btnTrigger;
                var clickbutton = click(btnTrigger);
                clickbutton()
                    .then(function () {
                        var dialogTitle = $("div.overlay .panel-heading").html();
                        expect(dialogTitle).toEqual(title);
                        done();
                    }).catch(expectError(done));
            }

            function openSampleProject(projectName) {
                var str = projectName + " project opens successfully",
                    clickOpenProject = click("#openProject"),
                    clickProject = click("button[data-project='{}']".format(projectName));
                return clickOpenProject()
                        .then(clickProject);
//                        .then(function () {
//                            expect(pm.project().name()).toEqual(projectName);
//                            done();
//                        }).catch(expectError(done));
//                });
            }

            function loadPlugin(pluginName) {
                var str = pluginName + " plugin adds a collapsible panel to the ui",
                    clickPlugin = click("input[name='{}']".format(pluginName));
                return clickPlugin();
            }

            function unloadPlugin(pluginName) {
                var str = pluginName + " plugin can be unloaded from the ui",
                    clickPlugin = click("input[name='{}']".format(pluginName));
                return clickPlugin()//to load
                        .then(clickPlugin);
            }
            /**
                used to toggle (expand/collapse) a collapsible panel
            */
            function togglePanel(pluginName) {
                var el = "div.collapsible-panel-parent[plugin-owner='" + pluginName + "'] span.toggle-collapse";
                return click(el);
            }


            describe("User interface Elements", function () {
                beforeEach(function (done) {
                    d3.select("div.overlay").remove();
                    pm = ProjectManager.getInstance();
                    main.start({noSplash: true}).then(function () {
                        p = pm.project();
                        done();
                    }).catch(expectError(done));
                });

                it("Open Project dialog can be opened", function (done) {
                    pressingButtonOpensDialog("#openProject", "Open Project", done);
                });

                it("Open project dialog can be dismissed", function (done) {
                    dialogCanBeDismissed("#openProject", "Open Project", done);
                });

                it("New Project dialog can be opened", function (done) {
                    pressingButtonOpensDialog("#newProject", "New Project", done);
                });

                it("New project dialog can be dismissed", function (done) {
                    dialogCanBeDismissed("#newProject", "New Project", done);
                });

                it("Import files dialog can be opened", function (done) {
                    loadPlugin("Model Editor").then(function () {
                        pressingButtonOpensDialog("#btnImportFiles", "Import files into Project", done);
                    }).catch(expectError(done));
                });

                it("Import files dialog can be dismissed", function (done) {
                    loadPlugin("Model Editor").then(function () {
                        dialogCanBeDismissed("#btnImportFiles", "Import files into Project", done);
                    }).catch(expectError(done));
                });

                it("AlarisGP project can be opened", function (done) {
                    openSampleProject("AlarisGP").then(function () {
                        expect(pm.project().name()).toEqual("AlarisGP");
                        done();
                    }).catch(expectError(done));
                });

                it("AlarisPC_PumpModules project can be opened", function (done) {
                    openSampleProject("AlarisPC_PumpModules").then(function () {
                        expect(pm.project().name()).toEqual("AlarisPC_PumpModules");
                        done();
                    }).catch(expectError(done));
                });

                it("SmithsMedical_MedFusion3500 project can be opened", function (done) {
                    openSampleProject("SmithsMedical_MedFusion3500").then(function () {
                        expect(pm.project().name()).toEqual("SmithsMedical_MedFusion3500");
                        done();
                    }).catch(expectError(done));
                });

                it("EmuCharts tool can be loaded", function (done) {
                    loadPlugin("EmuCharts Editor").then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='EmuCharts Editor']");
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(expectError(done));
                });

                it("EmuCharts tool can be unloaded", function (done) {
                    var p = "EmuCharts Editor";
                    unloadPlugin(p)
                        .then(function () {
                            var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='EmuCharts Editor']");
                            expect(pluginPanel.empty()).toEqual(true);
                            done();
                        }).catch(expectError(done));
                });

                it("Model Editor tool can be loaded", function (done) {
                    var p = "Model Editor";
                    loadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='{}']".format(p));
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(expectError(done));
                });

                it("Model Editor tool can be unloaded", function (done) {
                    var p = "Model Editor";
                    unloadPlugin(p)
                        .then(function () {
                            var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='{}']".format(p));
                            expect(pluginPanel.empty()).toEqual(true);
                            done();
                        }).catch(expectError(done));
                });

                it("Graph Builder tool can be loaded", function (done) {
                    var p = "Graph Builder";
                    loadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='{}']".format(p));
                        expect(pluginPanel.empty()).toBeFalsy();
                        done();
                    }).catch(expectError(done));
                });

                it("Graph Builder tool can be unloaded", function (done) {
                    var p = "Graph Builder";
                    unloadPlugin(p).then(function () {
                        var pluginPanel = d3.select(".collapsible-panel-parent[plugin-owner='{}']".format(p));
                        expect(pluginPanel.empty()).toEqual(true);
                        done();
                    }).catch(expectError(done));
                });
            });

            describe("Prototype Builder", function () {
                var pluginName = "Prototype Builder", editorPanel;

                beforeEach(function (done) {
                    d3.select("div.overlay").remove();
                    pm = ProjectManager.getInstance();
                    main.start({noSplash: true}).then(function () {
                        p = pm.project();
                        editorPanel = "div.collapsible-panel-parent[plugin-owner='{}'] .collapsible-panel".format(pluginName);
                        done();
                    }).catch(expectError(done));
                });

                it("can be collapsed", function (done) {
                    togglePanel(pluginName)()
                        .then(function () {
                            expect(d3.select(editorPanel).style("display")).toEqual("none");
                            done();
                        }).catch(expectError(done));
                });

                it("can be collapsed and expanded", function (done) {
                    togglePanel(pluginName)()
                        .then(togglePanel(pluginName))
                        .then(function () {
                            expect(d3.select(editorPanel).style("display")).toEqual("block");
                            done();
                        }).catch(expectError(done));
                });

                describe("Editor File Lists", function () {
                    it("has context menus", function (done) {
                        loadPlugin("Model Editor").then(function () {
                            togglePanel(pluginName)()
                                .then(rightclick("#pvsFiles"))
                                .then(function () {
                                    expect(d3.select("div.contextmenu").empty()).toBeFalsy();
                                    done();
                                });
                            }).catch(expectError(done));
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
                    var filesLength = pm.project().getDescriptors().length;
                    loadPlugin("Model Editor")
                        .then(listViewContextMenu("#pvsFiles", "#newfile"))
                        .then(function () {
                            setTimeout(function () {
                                expect(pm.project().getDescriptors().length).toEqual(filesLength + 1);
                                var desc = pm.project().getDescriptors()[pm.project().getDescriptors().length - 1];
                                expect(desc.path.indexOf(pm.project().name()) === 0).toBeTruthy();
                                done();
                            }, 300);
                        }).catch(expectError(done));
                });

                it("can remove files from the project", function (done) {
                    var filesLength = pm.project().getDescriptors().length;
                    loadPlugin("Model Editor")
                        .then(listViewContextMenu("#pvsFiles", "#newfile"))
                        .then(click("#pvsFiles li:last-child"))
                        .then(listViewContextMenu("#pvsFiles", "#delete"))
                        .then(click("div.overlay #btnOk"))
                        .then(function () {
                            setTimeout(function () {
                                expect(pm.project().getDescriptors().length).toEqual(filesLength);
                                done();
                            }, 500);
                        }).catch(expectError(done));
                });
            });

        }
    };
});
