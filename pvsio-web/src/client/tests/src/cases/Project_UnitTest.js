/**
 * Test cases for project
 * @author Patrick Oladimeji
 * @date 5/1/14 13:25:33 PM
 */
/*jshint undef: true, unused:false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, describe, beforeEach, afterEach, expect, it, d3, jasmine, beforeAll */
define(function (require, exports, module) {
    "use strict";
    var Project = require("project/Project"),
		main = require("main"),
        PrototypeBuilder = require("plugins/prototypebuilder/PrototypeBuilder"),
        Descriptor = require("project/Descriptor"),
        baseProjectDir = "projects",
        sampleProject = "BBraunPerfusor";
    
	module.exports = {
		run: function () {
			describe("Project", function () {
				var pb = PrototypeBuilder.getInstance(),
					pm = pb.getProjectManager();
				beforeAll(function (done) {
					pm = pb.getProjectManager();
					main.start({noSplash: true}).then(function () {
                        done();
                    });
				});
                
				it("user interface is loaded and there is connection to the websocket server", function (done) {
					var websocketStatusOK = d3.select("#lblWebSocketStatus span").classed("glyphicon-ok");
					expect(websocketStatusOK).toEqual(true);
                    done();
				});
                
                it("default project name after initialisation should contain 'default project'", function (done) {
                    expect(pm.project().name()).toEqual("defaultProject");
                    console.log(pm.project().name());
                    done();
                });

                it("default project is not dirty", function (done) {
                    expect(pm.project()._dirty()).toBeFalsy();
                    done();
                });
                
				describe("Project innards works fine", function () {
                    //describe opening a project
					beforeAll(function (done) {
                        pm.openProject(sampleProject).then(function (project) {
                            done();
                        });
                    });
                    
					it("sample project should open project correctly", function (done) {
                        expect(sampleProject).toEqual(pm.project().name());
                        done();
                    });
					
					it(" and " + sampleProject + " has at least one pvs file on init", function (done) {
						expect(pm.project().pvsFilesList()).not.toBeFalsy();
						expect(pm.project().pvsFilesList().length).toBeGreaterThan(0);
                        done();
					});
				});
			});
		}
	};
   
    
});
