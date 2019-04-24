/**
 * Test cases for Descriptor module
 * @author Patrick Oladimeji
 * @date 6/24/14 15:59:43 PM
 */
/*jshint undef:true, unused:false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, describe, beforeEach, afterEach, expect, it*/
define(function (require, exports, module) {
    "use strict";
    var Descriptor = require("project/Descriptor");

    module.exports = {
        run: function () {
            describe("Descriptor", function () {
                var f, spec = "Documents/main.pvs", image = "Documents/image.png",
                    specname = "main.pvs", imagename = "image.png";

                beforeEach(function () {
                    f = new Descriptor(spec, "");
                });

                afterEach(function () {
                    f = null;
                });

                it("has a .pvs extension", function () {
                    expect(f.extension).toEqual(".pvs");
                });
                it("has the correct name", function () {
                    expect(f.name).toEqual(specname);
                });

                it("spec is not an image", function () {
                    expect(f.isImage()).toBeFalsy();
                    expect(f.isPVSFile()).toBeTruthy();
                });

                it("has a toString method", function () {
                    var obj = JSON.parse(f.toString());
                    expect(obj.path).toEqual(spec);
                    expect(obj.encoding).toEqual("utf8");
                });

                it("uses the provided function to generate its text content", function (done) {
                    var content = "test";

                    f.content = "{}";

                    f.setContentGenerator(function () {
                        return content;
                    });

                    f.getContent().then(function (res) {
                        expect(res).toEqual(content);
                        done();
                    });
                });
            });
        }
    };

});
