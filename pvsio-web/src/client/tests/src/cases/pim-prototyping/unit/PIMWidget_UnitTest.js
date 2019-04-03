
define(function (require, exports, module) {

    var PIMWidget = require("plugins/pimPrototyper/PIMWidget"),
    ScreenCollection = require("plugins/pimPrototyper/ScreenCollection");

    return function() {
        describe("the PIM Widget class", function () {
            describe("getCoords function", function () {
                it("returns an object with the correct x, y, width and height values", function() {
                    var inputCoords = {
                        top: 50,
                        left: 60,
                        width: 70,
                        height: 80
                    };

                    var widget = new PIMWidget(1, inputCoords);
                    var coords = widget.getCoords();

                    expect(coords.x).toBe(inputCoords.left);
                    expect(coords.y).toBe(inputCoords.top);
                    expect(coords.width).toBe(inputCoords.width);
                    expect(coords.height).toBe(inputCoords.height);
                });
            });

            describe("toJSON function", function () {
                it("returns the expected object representation", function() {
                    var inputCoords = {
                        top: 50,
                        left: 60,
                        width: 70,
                        height: 80
                    };

                    var id = 1;

                    var opt = {
                        targetScreen: new Backbone.Model(),
                        name: "a widget"
                    };

                    var widget = new PIMWidget(id, inputCoords, opt);
                    var json = widget.toJSON();

                    expect(json).toEqual({
                        id: id,
                        coords: {
                            x: inputCoords.left,
                            y: inputCoords.top,
                            width: inputCoords.width,
                            height: inputCoords.height
                        },
                        targetScreen: opt.targetScreen.id,
                        name: opt.name,
                        type: "pim-button",
                        image: null
                    });
                });
            });

            describe("initFromJSON function", function () {
                it("correctly deserilizes a json representation of a widget", function() {
                    var inputCoords = {
                        top: 50,
                        left: 60,
                        width: 70,
                        height: 80
                    };

                    var id = 1;

                    var opt = {
                        name: "a widget"
                    };

                    var widget = new PIMWidget(id, inputCoords, opt);
                    var json = widget.toJSON();

                    var deserializedWidget = PIMWidget.initFromJSON(json);
                    expect(JSON.stringify(deserializedWidget)).toEqual(JSON.stringify(widget));
                });

                it("correctly restores the widget's target screen to point to the screen object", function() {
                    var screens = new ScreenCollection();
                    for (var i = 0; i < 5; i++) {
                        screens.add(new Backbone.Model({ id: i }));
                    }

                    var widget = new PIMWidget(1, {}, { targetScreen: screens.at(2) });
                    var json = widget.toJSON();

                    var deserializedWidget = PIMWidget.initFromJSON(json, screens);
                    expect(deserializedWidget.targetScreen()).toBe(widget.targetScreen());
                });
            });

            describe("duplicate function", function () {
                var widget;
                var clone;

                beforeEach(function() {
                    var inputCoords = {
                        top: 50,
                        left: 60,
                        width: 70,
                        height: 80
                    };

                    var id = 1;

                    var opt = {
                        name: "a widget"
                    };

                    widget = new PIMWidget(id, inputCoords, opt);
                    clone = widget.duplicate();
                });

                it("creates a new widget", function() {
                    expect(widget).not.toBe(clone);
                });

                it("copies the widget's attributes", function() {
                    expect(widget.name()).toEqual(clone.name());
                    expect(widget.targetScreen()).toEqual(clone.targetScreen());
                    expect(widget.targetScreen()).toEqual(clone.targetScreen());
                    expect(widget.getCoords()).toEqual(clone.getCoords());
                });
            });
        });
    };
});
