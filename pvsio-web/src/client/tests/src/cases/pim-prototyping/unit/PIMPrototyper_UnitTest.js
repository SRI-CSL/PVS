
define(function (require, exports, module) {

    var PIMPrototyper = require("plugins/pimPrototyper/PIMPrototyper"),
    Descriptor = require("project/Descriptor");

    return function() {
        describe("the Storyboard Editor class", function () {
            var prototyper = PIMPrototyper.getInstance();
            var descriptor;

            beforeEach(function () {
                prototyper._init(document.createElement("div"));

                // Stub the projectManager, since it deals with network and file IO
                prototyper._projectManager = {
                    addImage: function(imagePath, imageData) {
                        return new Promise(function (resolve, reject) {
                            descriptor = new Descriptor(imagePath, imageData, "");
                            resolve(descriptor);
                        });
                    }
                };
            });

            describe("changeImage function", function () {
                it("creates a new screen (and selects it) if none are selected", function(done) {
                    spyOn(prototyper._screens, "setSelected");
                    var imagePath = "test";
                    var imageData = "testData";
                    prototyper.changeImage(imagePath, imageData).then(function () {
                        expect(prototyper._screens.setSelected).toHaveBeenCalled();
                        expect(prototyper._screens.length).toBe(1);
                        done();
                    });
                });

                it("sets the image on the selected screen", function(done) {
                    prototyper._screens.add({});
                    var scrn = prototyper._screens.first();
                    prototyper._screens.setSelected(scrn);
                    spyOn(scrn, "set");

                    var imagePath = "test";
                    var imageData = "testData";
                    prototyper.changeImage(imagePath, imageData).then(function () {
                        expect(scrn.set).toHaveBeenCalledWith("image", descriptor);
                        done();
                    });
                });
            });
        });
    };
});
