/*jshint unused: false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global d3, $, it, expect, define, describe, beforeEach, afterAll, beforeAll, spyOn, Promise, Event, fail*/

/**
 * Behaviour tests for the PIM prototyping mode (and related features)
 * @author Nathaniel Watson <nathaniel@nwatson.nz>
 */
define(function (require, exports, module) {
    "use strict";
    var main = require("main"),
        util = require("../UIUtil"),
        PluginManager = require("plugins/PluginManager"),
        PIMPrototyper = require("plugins/pimPrototyper/PIMPrototyper"),
        Descriptor = require("project/Descriptor"),
        Screen = require("plugins/pimPrototyper/Screen"),
        PIMWidget = require("plugins/pimPrototyper/PIMWidget");

    var img = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==";

    return function () {
        function createScreens (done) {
                var widget, widgetId;

                var scrn = new Screen({
                    name: "1",
                    image: new Descriptor("test-img", img, { encoding: "base64" })
                });

                var coords = {
                    top: 0,
                    left: 0,
                    width: 20,
                    height: 20
                };

                PIMPrototyper.getInstance()._screens.add(scrn);
                PIMPrototyper.getInstance()._screens.setSelected(scrn);
                PIMPrototyper.getInstance()._widgetManager.addNewWidget({
                    name: "widget"
                }, coords, function(w) {
                    widget = w;
                    widgetId = w.id();
                });

                PIMPrototyper.getInstance()._screens.setSelected(null);
                PIMPrototyper.getInstance()._screens.setSelected(scrn);

                return {
                    widget: widget,
                    widgetId: widgetId,
                    scrn: scrn
                };
            }

        beforeEach(function (done) {
            d3.select("div.overlay").remove();
            main.start({noSplash: true}).then(function () {
                done();
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/2
        describe("Switching to PIM prototyping mode", function() {
            var buttonSelector = ".plugin-box input[name='Storyboard Editor']";

            it("provides a button to switch to the mode", function() {
                /*
                 Given the UI is loaded
                 Then a tab for activating the mode exists
                 */

                expect(d3.select(buttonSelector).empty()).toBe(false);
            });

            it("switches to PIM mode correctly", function() {
                /*
                Given the prototyping module is visible
                When the Storyboard Editor tab is clicked
                Then the PIM prototyping interface is shown
                 */

                d3.select("#plugin_StoryboardEditor").node().click();
                expect(d3.select(".collapsible-panel-parent[plugin-owner='StoryboardEditor']").empty()).toBe(false);
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/3
        describe("Switching screens in the PIM prototyping mode", function() {
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    done();
                }).catch(util.expectError(done));
            });

            it("provides a dropdown that lists the screens", function() {
                /*
                Given At least two images have been loaded into the builder
                Then a dropdown menu exists that lists these images
                */

                for (var i = 0; i < 2; i++) {
                    PIMPrototyper.getInstance()._screens.add(new Screen({ name: "New screen" }));
                }

                var dropdownButton = d3.select(".pim-prototyper .btn-screen-dropdown");
                expect(dropdownButton.empty()).toBe(false);

                var dropdownMenu = d3.select(".pim-prototyper .screen-dropdown");
                expect(dropdownMenu.empty()).toBe(false);
                expect(dropdownMenu.selectAll("li").size()).toBe(2);
            });

            it("switches screens when a screen is selected from the dropdown", function(done) {
                /*
                Given at least two images have been loaded into the builder
                When an item is selected from the dropdown
                Then the screen being edited is swapped for the one that was selected from the dropdown
                And the state that the previous screen was in is not lost
                */

                PIMPrototyper.getInstance()._screens.add(new Screen({
                    name: "1",
                    image: new Descriptor("test-img", img, { encoding: "base64" })
                }));

                PIMPrototyper.getInstance()._screens.add(new Screen({ name: "2" }));

                var dropdownMenu = d3.select(".pim-prototyper .screen-dropdown");
                dropdownMenu.select("li a").node().click();

                util.wait(200)().then(function () {
                    expect(d3.select(".pim-prototyper .prototype-image-inner img").attr("src")).toBe(img);
                    done();
                });
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/4
        describe("Adding and removing screens from the PIM prototype", function() {
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    done();
                }).catch(util.expectError(done));
            });

            it("allows a screen to be added via a button and a dialog", function() {
                /*
                Given the PIM Builder mode is active
                Then a button exists for adding a screen.
                When the "add screen" button is clicked
                Then a dialog is shown for adding a new screen
                And the dialog allows a screen name to be set
                */

                var screenName = "test screen";

                d3.select(".pim-prototyper .btn-screen-add").node().click();
                var overlay = d3.select("div.overlay");
                var nameInput = overlay.select("input[name='screenName']");
                expect(nameInput.empty()).toBe(false);

                nameInput.attr("value", screenName);
                overlay.select(".btn-create").node().click();
                expect(PIMPrototyper.getInstance()._screens.where({ name: screenName }).length).toBe(1);
            });

            it("shows a confirmation before a screen is deleted", function() {
                /*
                Given at least one image has been loaded into the builder
                Then a button exists for deleting the current screen.
                When the button is clicked
                Then a dialog is shown to confirm the screen deletion
                And choosing to delete the screen removes its data and any display of it within the prototype
                And the editor switches to a different screen
                 */

                var dropdownButton = d3.select(".pim-prototyper .btn-screen-dropdown");
                var screenName = "to be deleted";
                var scrn = new Screen({ name: screenName });
                PIMPrototyper.getInstance()._screens.add(scrn);
                PIMPrototyper.getInstance()._screens.setSelected(scrn);
                var selectedText = dropdownButton.text();

                d3.select(".pim-prototyper .btn-screen-delete").node().click();
                var overlay = d3.select("div.overlay");
                expect(overlay.select(".panel").empty()).toBe(false);

                overlay.select("#btnOk").node().click();
                expect(PIMPrototyper.getInstance()._screens.where({ name: screenName }).length).toBe(0);
                expect(dropdownButton.text()).not.toBe(selectedText);
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/5
        describe("Editing screens in the PIM prototype", function() {
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    done();
                }).catch(util.expectError(done));
            });

            it("allows an existing screen to be edited via a dialog", function() {
                /*
                Given a screen is being edited within the PIM builder
                When the screen settings button is clicked
                Then a dialog opens
                And the dialog provides input for changing the screen name
                And the dialog provides input for marking the screen as the initial screen
                */
                var scrn = new Screen({ name: "New screen" });
                PIMPrototyper.getInstance()._screens.add(scrn);
                PIMPrototyper.getInstance()._screens.setSelected(scrn);

                d3.select(".pim-prototyper .btn-screen-options").node().click();
                var overlay = d3.select("div.overlay");
                expect(overlay.select(".panel").empty()).toBe(false);

                var nameInput = overlay.select("input[name='screenName']");
                expect(nameInput.empty()).toBe(false);
                var initialInput = overlay.select("input[name='isInitial']");
                expect(initialInput.empty()).toBe(false);

                overlay.select(".btn-cancel").node().click();
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/6
        describe("Setting the start screen in the PIM prototype", function() {
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    done();
                }).catch(util.expectError(done));
            });

            it("alters the screens so that the correct one (and only one) is marked as the start screen", function() {
                /*
                Given a screen is active/being edited in the PIM builder
                When the button for marking the current screen as the start screen is clicked
                Then the current screen is marked as the start screen
                And none of the other screens are the start screen
                 */
                 var scrn1 = new Screen({ name: "screen1", isInitial: true });
                 var scrn2 = new Screen({ name: "screen2", isInitial: false });
                 PIMPrototyper.getInstance()._screens.add(scrn1);
                 PIMPrototyper.getInstance()._screens.add(scrn2);
                 PIMPrototyper.getInstance()._screens.setSelected(scrn2);

                 d3.select(".pim-prototyper .btn-screen-options").node().click();
                 var overlay = d3.select("div.overlay");

                 var initialInput = overlay.select("input[name='isInitial']");
                 initialInput.property("checked", true);

                 overlay.select(".btn-create").node().click();

                 expect(scrn2.get("isInitial")).toBe(true);
                 expect(scrn1.get("isInitial")).toBe(false);
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/7
        describe("The link between the prototype and the FSM chart", function() {
            var scrns, widget;
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    var imgDescriptor = new Descriptor("test-img", img, { encoding: "base64" });

                    scrns = [];

                    for (var i = 0; i < 2; i++) {
                        scrns.push(new Screen({
                            name: "screen" + i,
                            image: imgDescriptor
                        }));

                        PIMPrototyper.getInstance()._screens.add(scrns[i]);
                    }

                    PIMPrototyper.getInstance()._screens.setSelected(scrns[0]);
                    PIMPrototyper.getInstance()._widgetManager.addNewWidget({
                        name: "widget"
                    }, {
                        top: 0,
                        left: 0,
                        width: 20,
                        height: 20
                    }, function(w) {
                        widget = w;
                    });

                    widget.targetScreen(scrns[1]);
                    PIMPrototyper.getInstance()._screens.setSelected(null);
                    PIMPrototyper.getInstance()._screens.setSelected(scrns[0]);

                    done();

                }).catch(util.expectError(done));
            });

            function convertChart() {
                var button = d3.select(".pim-prototyper .pim-convert-button");
                button.node().click();
                var overlay = d3.select("div.overlay");
                overlay.select("button[type='submit']").node().click();
            }

            it("provides a button to perform the conversion", function() {
                /*
                Given the prototyping module is visible
                Then a button exists for converting the prototype to a chart
                 */
                var button = d3.select(".pim-prototyper .pim-convert-button");
                expect(button.empty()).toBe(false);
            });

            it("warns the user before converting to a chart", function() {
                /*
                Given the prototyping module is visible
                When the Convert button is clicked
                Then the user is informed that the existing chart will be lost.
                 */
                var button = d3.select(".pim-prototyper .pim-convert-button");
                button.node().click();
                var overlay = d3.select("div.overlay");
                expect(overlay.select(".panel").empty()).toBe(false);

            });

            it("adds states to the chart for each new screen", function(done) {
                /*
                Given the prototyping module is visible
                When the Convert button is clicked
                And the user chooses to convert the prototype
                Then the chart editor should display a chart
                And the chart should have a state for each screen of the prototype
                 */
                convertChart();
                var emuPanel = d3.select(".collapsible-panel-parent[plugin-owner='EmuChartsEditor']");
                expect(emuPanel.empty()).toBe(false);

                util.wait(1000)().then(function() {
                    var states = d3.selectAll("#States .state");
                    expect(states.size()).toBe(scrns.length);
                    done();
                });
            });

            it("adds transitions to the chart for each link in the prototype", function(done) {
                /*
                Given the prototyping module is visible
                When the Convert button is clicked
                And the user chooses to convert the prototype
                Then the chart editor should display a chart
                And the chart should have a transition for each linked widget in the prototype
                */
                convertChart();

                util.wait(1000)().then(function() {
                    var transitions = d3.selectAll("#Transitions .transition");
                    expect(transitions.size()).toBe(1);
                    done();
                });
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/8
        describe("Adding hotspots to the PIM prototype", function() {
            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    done();
                }).catch(util.expectError(done));
            });

            it("is possible to add hotspots by dragging over the image", function() {
                /*
                Given a screen is active/being edited in the PIM builder
                When the screen image is dragged over
                Then a hotspot is added and drawn as rectangle where the user dragged
                And a dialog is shown for configuring the hotspot
                */
                var scrn = new Screen({
                    name: "1",
                    image: new Descriptor("test-img", "", { encoding: "base64" })
                });

                PIMPrototyper.getInstance()._screens.add(scrn);
                PIMPrototyper.getInstance()._screens.setSelected(scrn);

                var svg = d3.select(".pim-prototyper .image-map-layer");
                svg.attr("width", "20px")
                    .attr("height", "20px");

                svg.node().dispatchEvent(new MouseEvent("mousedown"));
                svg.node().dispatchEvent(new MouseEvent("mousemove"));
                svg.node().dispatchEvent(new MouseEvent("mouseup"));

                expect(svg.select("rect").empty()).toBe(false);

                var overlay = d3.select("div.overlay");
                expect(overlay.select(".panel").empty()).toBe(false);
                overlay.select(".btn-cancel").node().click();
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/9
        describe("Editing hotspots in the PIM prototype", function() {
            var widgetId, scrn, widget;

            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    var screenObj = createScreens();
                    widget = screenObj.widget;
                    widgetId = screenObj.widgetId;
                    scrn = screenObj.scrn;
                    done();
                });
            });

            it("shows an edit dialog when a hotspot is clicked", function() {
                /*
                Given a hotspot exists within the current screen
                When the hotspot is double-clicked
                Then a dialog is shown for editing the hotspot
                And the dialog allows naming the hotspot
                And the dialog allows selecting where the hotspot links to
                */

                var svg = d3.select(".pim-prototyper .image-map-layer");
                var hotspot = svg.select("#" + widgetId);
                hotspot.node().dispatchEvent(new MouseEvent("dblclick"));

                var overlay = d3.select("div.overlay");
                expect(overlay.select(".panel").empty()).toBe(false);

                var nameInput = overlay.select("input[name='name']");
                expect(nameInput.empty()).toBe(false);
                var screenDropdown = overlay.select(".btn-screen-dropdown");
                expect(screenDropdown.empty()).toBe(false);
                overlay.select(".btn-cancel").node().click();
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/10
        describe("Setting a hotspot's behaviour", function() {
            var widgetId, scrn, widget;

            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    var screenObj = createScreens();
                    widget = screenObj.widget;
                    widgetId = screenObj.widgetId;
                    scrn = screenObj.scrn;
                    done();
                });
            });

            it("can set a hotspot to link to a screen", function() {
                /*
                Given a hotspot is being edited
                When a screen is selected from the dropdown
                And the hotspot settings are saved
                Then the hotspot's action is set to the screen that was selected
                */

                var svg = d3.select(".pim-prototyper .image-map-layer");
                var hotspot = svg.select("#" + widgetId);
                hotspot.node().dispatchEvent(new MouseEvent("dblclick"));

                var overlay = d3.select("div.overlay");

                var screenDropdownBtn = overlay.select(".btn-screen-dropdown");
                screenDropdownBtn.node().click();

                var screenDropdownItem = overlay.select(".screen-dropdown a");
                screenDropdownItem.node().click();

                overlay.select(".btn-create").node().click();

                expect(widget.targetScreen()).toBe(scrn);
            });
        });

        // https://github.com/nathanielw/pvsio-web/issues/11
        describe("Viewing a PIM prototype in the simulator", function() {
            var widgetId, scrn, widget;

            beforeEach(function (done) {
                PluginManager.getInstance().enablePlugin(PIMPrototyper.getInstance()).then(function () {
                    var screenObj = createScreens();
                    widget = screenObj.widget;
                    widgetId = screenObj.widgetId;
                    scrn = screenObj.scrn;
                    done();
                });
            });

            it("links hotspots between screens", function(done) {
                /*
                Given the PIM Simulator is the active mode
                And a PIM exists with hotspots linking multiple screens
                Then the same hotspots should be present in the simulator view
                When a hotspot is clicked
                Then the simulator switches to showing the linked screen's image
                And the hotspots from the previous screen are removed/hidden
                And the hotspots from the linked screen are shown
                */
                var img2 = "data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs=";

                var scrn2 = new Screen({
                    name: "2",
                    image: new Descriptor("test-img2", img2, { encoding: "base64" })
                });

                PIMPrototyper.getInstance()._screens.add(scrn2);
                widget.targetScreen(scrn2);

                d3.select(".pim-prototyper .pim-mode-toggle .prototype-simulator-button").node().click();
                var areaMap = d3.select(".pim-prototyper .prototype-image-inner map");
                var areas = areaMap.selectAll("area");
                expect(areas.size()).toEqual(1); // checking the hotspots are present

                areaMap.select("area").node().dispatchEvent(new MouseEvent("mousedown"));

                util.wait(500)().then(function() {
                    expect(d3.select(".pim-prototyper .prototype-image-inner img").attr("src")).toBe(img2);
                    areas = areaMap.selectAll("area");
                    expect(areas.size()).toEqual(0); // checking the hotspots have changed

                    done();
                });
            });
        });
    };
});
