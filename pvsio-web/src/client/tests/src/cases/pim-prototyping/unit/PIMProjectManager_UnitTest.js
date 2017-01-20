
define(function (require, exports, module) {

    var PIMProjectManager = require("plugins/pimPrototyper/PIMProjectManager"),
        Project = require("project/Project");

    return function() {
        describe("the PIM Project Manager class", function () {
            var pm;
            var json = [{
                "id":1,
                "name":"test",
                "isInitial":true,
                "widgets":[{
                    "id":"1",
                    "type":"pim-button",
                    "name":"up",
                    "targetScreen":"2",
                    "coords":{"x":1,"y":96,"width":39,"height":38}
                }],
                "image":"pic.jpg"
            }, {
                "id":2,
                "name":"test2",
                "isInitial":false,
                "widgets":[],
                "image":"pic.jpg"
            }];

            beforeEach(function () {
                pm = new PIMProjectManager({
                    addListener: function(){},
                    project: function(){ return new Project(); }
                });
            });

            describe("initFromJSON function", function () {
                it("correctly deserilizes a JSON representation of a project and stores the result", function() {
                    pm.initFromJSON(json);
                    expect(pm.screens().length).toBe(json.length);
                });

                it("correctly restores widget definitions", function() {
                    pm.initFromJSON(json);
                    expect(pm.screens().get(1).get("widgets")[json[0].widgets[0].id]).not.toBeUndefined();
                });
            });
        });
    };
});
