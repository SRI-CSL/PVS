/**
 * Test cases for Widget Manager
 * @author Paolo Masci
 * @date Dec 6, 2016
 */
/*jshint undef: true, unused:false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, describe, beforeEach, afterEach, expect, it, d3, jasmine, beforeAll */
define(function (require, exports, module) {
    "use strict";
    var Project = require("project/Project"),
        pm   = require("project/ProjectManager").getInstance(),
		main = require("main"),
        pb = require("plugins/prototypebuilder/PrototypeBuilder").getInstance(),
        Descriptor = require("project/Descriptor"),
        WidgetManager = require("pvsioweb/WidgetManager").getWidgetManager();

    var img = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==";

    var wd = {
         "widgetMaps": [
          {
           "id": "Display1",
           "type": "display",
           "displayKey": "left_display",
           "auditoryFeedback": "disabled",
           "visibleWhen": "true",
           "fontsize": "42",
           "fontColor": "white",
           "backgroundColor": "transparent"
          },
          {
           "id": "Button1",
           "type": "button",
           "evts": [
            "click"
           ],
           "recallRate": 250,
           "functionText": "UP",
           "customFunctionText": null,
           "boundFunctions": "click_UP",
           "buttonReadback": "",
           "keyCode": "33",
           "keyName": "page up",
           "visibleWhen": "true"
          },
          {
           "id": "Button2",
           "type": "button",
           "evts": [
            "click"
           ],
           "recallRate": 250,
           "functionText": "up",
           "customFunctionText": null,
           "boundFunctions": "click_up",
           "buttonReadback": "",
           "keyCode": "38",
           "keyName": "up arrow",
           "visibleWhen": "true"
          }
         ],
         "regionDefs": [
          {
           "class": "Display1357494252462",
           "shape": "rect",
           "coords": "255,92,365,139",
           "href": null
          },
          {
           "class": "Button1",
           "shape": "rect",
           "coords": "181,170,231,205",
           "href": null
          },
          {
           "class": "Button2",
           "shape": "rect",
           "coords": "235,168,281,206",
           "href": null
          }
         ]
     };

	module.exports = {
		run: function () {
			describe("Project", function () {
				beforeAll(function (done) {
					main.start({noSplash: true}).then(function () {
                        done();
                    });
				});

				it("widgets correctly restored", function (done) {
                    pb.changeImage("blank-image.gif", img).then(function (res) {
                        pb.updateImageAndLoadWidgets();
                        WidgetManager.restoreWidgetDefinitions(wd);
    					var widgets = d3.select("#imageDiv .image-map-layer");
                        wd.widgetsMap.forEach(function (widget) {
                            var theWidget = d3.select(widget.id.node());
                            if (theWidget.type === "display") {
                                expect(theWidget.id).toEqual("Display1");
                            }
                        });
                        done();
                    }).catch(function (err) {
                        console.log(err);
                    });
				});
			});
		}
	};


});
