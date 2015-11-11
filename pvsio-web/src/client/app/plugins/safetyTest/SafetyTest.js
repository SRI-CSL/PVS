/**
 * Safety test plugin. Reads a list of predefined tests for a project and checks safety of device
 * @author Patrick Oladimeji
 * @date 9/24/14 11:01:13 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Promise, Handlebars, d3 */
define(function (require, exports, module) {
    "use strict";
    var	PrototypeBuilder	= require("plugins/prototypebuilder/PrototypeBuilder"),
        ProjectManager      = require("project/ProjectManager"),
        PVSioWebClient      = require("PVSioWebClient"),
        SampleTests         = require("plugins/safetyTest/SampleTest"),
        template            = require("text!plugins/safetyTest/templates/safetytest.handlebars"),
        testRowTemplate     = require("text!plugins/safetyTest/templates/testrow.handlebars"),
        ScriptPlayer        = require("util/ScriptPlayer");

    var canvas, instance;

    function SafetyTest() {

    }

    SafetyTest.prototype.initialise = function () {
        var that = this;
        canvas = PVSioWebClient.getInstance().createCollapsiblePanel({
            headerText: "Safety Test",
            owner: "SafetyTest"
        });
        var html = Handlebars.compile(template)(SampleTests);

        canvas.html(html);
        d3.select("#runTests").on("click", function () {
            that.runTests();
        });
        return Promise.resolve(true);
    };

    SafetyTest.prototype.unload = function () {
        PVSioWebClient.getInstance().removeCollapsiblePanel(canvas);
        return Promise.resolve(true);
    };

    /**

    */
    SafetyTest.prototype.appendResult = function (result) {
        var html = Handlebars.compile(testRowTemplate)(result);
        d3.select("#safetyTests tbody").append("tr").html(html);
    };

    SafetyTest.prototype.runTests = function () {
        var that = this;
        var pm = ProjectManager.getInstance();
        var project = pm.project();
        var testCases = SampleTests.slice(0);

        function test() {
            var testCase = testCases.shift();
            if (testCase) {
                return ScriptPlayer.runTest(testCase).then(function (res) {
                    if (res !== "done") {
                        var safe = res;
                        that.appendResult([testCase.init, testCase.keySequence, safe]);
                        test();
                    }
                });
            } else {
                return Promise.resolve("done");
            }
        }

        if (project.name() !== "defaultProject") {
            return test();
//		} else {
//
        }
    };

    SafetyTest.prototype.getDependencies = function () {
        return [PrototypeBuilder.getInstance()];
    };

    module.exports = {
        getInstance: function () {
            instance = instance || new SafetyTest();
            return instance;
        }
    };
});
