/**
 * Widget Previewer
 * @author Paolo Masci
 * @date Sep 15, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, displayKeyCode*/
define(function (require, exports, module) {
    "use strict";
    var Button          = require("widgets/Button"),
        LED             = require("widgets/LED"),
        BasicDisplay    = require("widgets/BasicDisplay"),
        NumericDisplay  = require("widgets/NumericDisplay"),
        TouchscreenButton = require("widgets/TouchscreenButton"),
        TouchscreenDisplay = require("widgets/TouchscreenDisplay"),
        d3				= require("d3/d3");

    module.exports = {
        preview: function (widgetType, opt) {
            opt = opt || {};
            // remove old widget previews, if any
            d3.select("#theWidget").remove();
            d3.select("#navkeys").attr("style", "display: none");
            d3.select("#widgetPreview").attr("style", "display: none;");
            // render the widget passed as argument to the function
            var widgetPreview = null;
            var maxWidth = 210;
            if (widgetType === "button") {
                widgetPreview = new Button("theWidget",
                    { left: 16, top: 32, width: 64, height: 52 },
                    { keyCode: opt.keyCode, keyName: displayKeyCode(opt.keyCode),
                      evts: [],
                      buttonReadback: opt.buttonReadback,
                      prototypeMap: "widgetPreviewMap",
                      parent: "navkeys" });
                widgetPreview.render();
                d3.select("#navkeys").attr("style", "display: block;");
            } else if (widgetType === "display") {
                widgetPreview = new BasicDisplay("theWidget",
                    { width: maxWidth, height: 50 },
                    { backgroundColor: (opt.backgroundColor === "transparent") ? "lightgrey" : (opt.backgroundColor) ? opt.backgroundColor : "black",
                      fontColor: opt.fontColor || "white",
                      fontsize: opt.fontsize || 20,
                      auditoryFeedback: opt.auditoryFeedback,
                      position: "relative",
                      parent: "widgetPreview" });
                widgetPreview.render("Display 2.01");
                d3.select("#widgetPreview").attr("style", "display: block;");
            } else if (widgetType === "numericdisplay") {
                widgetPreview = new NumericDisplay("theWidget",
                    { width: maxWidth, height: 50 },
                    { backgroundColor: (opt.backgroundColor === "transparent") ? "lightgrey" : (opt.backgroundColor) ? opt.backgroundColor : "black",
                      fontColor: opt.fontColor || "white",
                      fontsize: opt.fontsize || 20,
                      auditoryFeedback: opt.auditoryFeedback,
                      displayKey: "d",
                      cursorName: "c",
                      position: "relative",
                      parent: "widgetPreview" });
                widgetPreview.render({ d:2.01, c:0 });
                d3.select("#widgetPreview").attr("style", "display: block;");
            } else if (widgetType === "touchscreendisplay") {
                widgetPreview = new TouchscreenDisplay("theWidget",
                    { width: maxWidth, height: 50 },
                    { backgroundColor: (opt.backgroundColor === "transparent") ? "lightgrey" : (opt.backgroundColor) ? opt.backgroundColor : "black",
                      fontColor: opt.fontColor || "white",
                      fontsize: opt.fontsize || 20,
                      auditoryFeedback: opt.auditoryFeedback,
                      displayMode: (opt.cursorName) ? "numeric" : "standard",
                      displayKey: "d",
                      cursorName: "c",
                      position: "relative",
                      parent: "widgetPreview" });
                if (opt.cursorName) {
                    widgetPreview.render({ d:"Display 2.01", c:0 });
                } else {
                    widgetPreview.render({ d:"Display 2.01" });
                }
                d3.select("#widgetPreview").attr("style", "display: block;");
            } else if (widgetType === "touchscreenbutton") {
                widgetPreview = new TouchscreenButton("theWidget",
                    { width: maxWidth, height: 50 },
                    { softLabel: "Touchscreen button",
                      buttonReadback: opt.buttonReadback,
                      backgroundColor: (opt.backgroundColor === "transparent") ? "lightgrey" : opt.backgroundColor,
                      fontColor: opt.fontColor,
                      fontsize: opt.fontsize || 20,
                      auditoryFeedback: false,
                      position: "relative",
                      parent: "widgetPreview" });
                widgetPreview.render();
                d3.select("#widgetPreview").attr("style", "display: block;");
            } else if (widgetType === "led") {
                var color = opt.color;
                widgetPreview = new LED("theWidget",
                    { width: 0, height: 0 },
                    { color: color,
                      position: "relative",
                      parent: "widgetPreview" });
                widgetPreview.render();
                d3.select("#widgetPreview").attr("style", "display: block;");
            }
        }
    };
});
