/* global _ */
define(function (require, exports, module) {

    var EmuWrapper = require("plugins/pimPrototyper/EmuWrapper"),
        Screen = require("plugins/pimPrototyper/Screen");

    return function() {
        describe("the EmuWrapper class", function () {
            var screenName, node, editorStub;

            beforeEach(function () {
                screenName = "test_screen";
                node = {};

                editorStub = {
                    emucharts: {
                        add_node: function(){ return node; },
                        add_edge: function(){}
                    },

                    renderStates: function(){}
                };
            });

            describe("the addScreenState function", function () {
                it("calls the add_node method of the editor with the correct arguments", function() {
                    var scr = new Screen({ name: screenName });

                    spyOn(editorStub.emucharts, "add_node").and.callThrough();
                    var emuWrapper = new EmuWrapper(editorStub);
                    emuWrapper.addScreenState(scr);
                    expect(editorStub.emucharts.add_node).toHaveBeenCalledWith({ name: screenName, id: scr.id });
                });

                it("adds PIM-related values to the returned state", function() {
                    var scr = new Screen({ name: screenName });

                    var emuWrapper = new EmuWrapper(editorStub);
                    emuWrapper.addScreenState(scr);
                    expect(node.widgets).not.toBeUndefined();
                });

                it("correctly adds the screen's widgets to the state", function() {
                    var scr = new Screen({ name: screenName });
                    var widget = {
                        name: function() { return "test_widget"; },
                        targetScreen: function() { return scr; }
                    };
                    scr.set("widgets", { 1: widget });

                    var emuWrapper = new EmuWrapper(editorStub);
                    emuWrapper.addScreenState(scr);
                    expect(node.widgets[0].name).toBe(widget.name());
                    expect(node.widgets[0].behaviours[0]).not.toBeUndefined();
                    expect(node.widgets[0].category).not.toBeUndefined();
                });
            });

            describe("the _addWidgetTransition function", function () {
                it("calls the add_edge method of the editor with the correct arguments", function() {
                    var scr = new Screen({ name: screenName });
                    var widget = {
                        name: function() { return "test_widget"; },
                        targetScreen: function() { return scr; }
                    };
                    scr.set("widgets", { 1: widget });

                    var args;

                    spyOn(editorStub.emucharts, "add_edge").and.callFake(function (a) {
                        args = _.clone(a);
                    });

                    var emuWrapper = new EmuWrapper(editorStub);
                    emuWrapper._addWidgetTransition(widget, scr);

                    expect(editorStub.emucharts.add_edge).toHaveBeenCalled();
                    expect(args.target.id).toBe(widget.targetScreen().id);
                    expect(args.source.id).toBe(scr.id);
                    expect(args.name).toBe(emuWrapper._createBehaviourName(widget.targetScreen()));
                });

                it("does not attempt to add a transition if the widget has no links", function() {
                    var scr = new Screen({ name: screenName });
                    var widget = {
                        name: function() { return "test widget"; },
                        targetScreen: function() {}
                    };
                    scr.set("widgets", { 1: widget });

                    spyOn(editorStub.emucharts, "add_edge");

                    var emuWrapper = new EmuWrapper(editorStub);
                    emuWrapper._addWidgetTransition(widget, scr);

                    expect(editorStub.emucharts.add_edge).not.toHaveBeenCalled();
                });
            });

            describe("the _createBehaviourName function", function () {
                it("removes all whitespace", function () {
                    var emuWrapper = new EmuWrapper(editorStub);
                    var name = emuWrapper._createBehaviourName({ get: function() { return "	screen  name"; }});
                    expect(name.match(/\s/)).toBeNull();
                });

                it("contains the screen's name", function () {
                    var emuWrapper = new EmuWrapper(editorStub);
                    var screenName = "theTarget";
                    var name = emuWrapper._createBehaviourName({ get: function() { return screenName; }});
                    expect(name.indexOf(screenName)).not.toBe(-1);
                });
            });
        });
    };
});
