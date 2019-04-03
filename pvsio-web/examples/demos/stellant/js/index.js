/**
 *
 * @author Paolo Masci, Patrick Oladimeji
 * @date 27/03/15 20:30:33 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
require.config({
    baseUrl: "../../client/app",
    paths: {
        d3: "../lib/d3",
        "pvsioweb": "plugins/prototypebuilder",
        "imagemapper": "../lib/imagemapper",
        "text": "../lib/text",
        "lib": "../lib",
        "cm": "../lib/cm",
        stateParser: './util/PVSioStateParser'
    }
});

require([
        "widgets/TouchscreenButton",
        "widgets/BasicDisplay",
        "widgets/LED",
        "widgets/medical/TracingsDisplayEVO",
        "widgets/medical/PatientMonitorDisplay",
        "plugins/graphbuilder/GraphBuilder",
        "stateParser",
        "PVSioWebClient"],
    function (TouchscreenButton,
              BasicDisplay,
              LED,
              TracingsDisplay,
              PatientMonitorDisplay,
              GraphBuilder,
              stateParser,
              PVSioWebClient) {
        "use strict";
        var d3 = require("d3/d3");
        var client = PVSioWebClient.getInstance();

        // append displays
        var device = {};
        device.vol_saline = new SegmentDisplay("vol_saline");
        device.vol_saline.pattern = "###";
        device.vol_saline.displayAngle = 10;
        device.vol_saline.digitHeight = 3;
        device.vol_saline.digitWidth = 2;
        device.vol_saline.digitDistance = 1;
        device.vol_saline.segmentWidth = 0.3;
        device.vol_saline.segmentDistance = 0.1;
        device.vol_saline.segmentCount = 7;
        device.vol_saline.cornerType = 3;
        device.vol_saline.colorOn = "#001fff";
        device.vol_saline.colorOff = "#1b0033";
        device.vol_saline.draw();
        device.vol_saline.setValue("");

        device.vol_contrast = new SegmentDisplay("vol_contrast");
        device.vol_contrast.pattern = "###";
        device.vol_contrast.displayAngle = 10;
        device.vol_contrast.digitHeight = 3;
        device.vol_contrast.digitWidth = 2;
        device.vol_contrast.digitDistance = 1;
        device.vol_contrast.segmentWidth = 0.3;
        device.vol_contrast.segmentDistance = 0.1;
        device.vol_contrast.segmentCount = 7;
        device.vol_contrast.cornerType = 3;
        device.vol_contrast.colorOn = "#24ff22";
        device.vol_contrast.colorOff = "#082605";
        device.vol_contrast.draw();
        device.vol_contrast.setValue("");

        device.btn_inc_saline = new TouchscreenButton("inc_saline", {
            top: 212,
            left: 176,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_inc_contrast = new TouchscreenButton("inc_contrast", {
            top: 212,
            left: 261,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_dec_saline = new TouchscreenButton("dec_saline", {
            top: 252,
            left: 176,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_dec_contrast = new TouchscreenButton("dec_contrast", {
            top: 252,
            left: 261,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_auto = new TouchscreenButton("btn_auto", {
            top: 227,
            left: 212,
            width: 38,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent", // does this button light up?
            borderRadius: 20,
            opacity: 0.5
        });
        device.btn_fill_saline = new TouchscreenButton("btn_fill_saline", {
            top: 225,
            left: 110,
            width: 63,
            height: 36
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20,
            opacity: 0.5
        });
        device.btn_fill_contrast = new TouchscreenButton("btn_fill_contrast", {
            top: 227,
            left: 289,
            width: 63,
            height: 36
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20,
            opacity: 0.5
        });
        device.btn_prime = new TouchscreenButton("btn_prime", {
            top: 315,
            left: 211,
            width: 38,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent", // does this button light up?
            borderRadius: 20,
            opacity: 0.5
        });
        device.btn_confirm = new TouchscreenButton("btn_confirm", {
            top: 463,
            left: 210,
            width: 38,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent", // does this button light up?
            borderRadius: 20,
            opacity: 0.5
        });
        device.btn_engage = new TouchscreenButton("btn_engage", {
            top: 553,
            left: 209,
            width: 38,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent", // does this button light up?
            borderRadius: 20,
            opacity: 0.5
        });
        device.lock = new BasicDisplay("lock", {
            top: 513,
            left: 220,
            width: 24,
            height: 24
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            fontColor: "black",
            backgroundColor: "transparent" // does this button light up?
        });
        device.ready_LED = new LED("ready_LED", {
            top: 516,
            left: 221,
            width: 13,
            height: 13
        }, {
            parent: "stellant",
            visibleWhen: "mode = READY",
            callback: onMessageReceived,
            backgroundColor: "transparent" // does this button light up?
        });


        // utility function
        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            return str;
        }

        /**
         function to handle when an output has been received from the server after sending a guiAction
         if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
         */
        function onMessageReceived(err, event) {
            function prettyprint(v) {
                var x = parseInt(v);
                if (x < 10 ) {
                    return "00" + v;
                } else if (x < 100) {
                    return "0" + v;
                } else return v;
            }
            function render_saline_volume(res) {
                if (res.vol_saline_visible === "TRUE") {
                    device.vol_saline.setValue(prettyprint(res.vol_saline));
                } else {
                    device.vol_saline.setValue("");
                }
            }
            function render_contrast_volume(res) {
                if (res.vol_contrast_visible === "TRUE") {
                    device.vol_contrast.setValue(prettyprint(res.vol_contrast));
                } else {
                    device.vol_contrast.setValue("");
                }
            }
            function render_button(b, res) {
                if (res[b] === "BLINKING") {
                    device[b].render("", { blinking: true, backgroundColor: "cyan" });
                } else if (res[b] === "LIGHT") {
                    device[b].render("", { blinking: false, backgroundColor: "deepskyblue" });
                } else {
                    device[b].render();
                }
            }
            function render_lock(red) {
                if (res.mode === "READY") {
                    device.lock.renderGlyphicon("glyphicon-lock");
                } else {
                    device.lock.hide();
                }
            }
            if (!err) {
                client.getWebSocket().lastState(event.data);
                // rendering
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    res = stateParser.parse(event.data.toString());
                    if (res) {
                        render_saline_volume(res);
                        render_contrast_volume(res);
                        device.btn_inc_saline.render();
                        device.btn_inc_contrast.render();
                        device.btn_dec_saline.render();
                        device.btn_dec_contrast.render();
                        render_button("btn_fill_saline", res);
                        render_button("btn_fill_contrast", res);
                        render_button("btn_auto", res);
                        render_button("btn_prime", res);
                        render_button("btn_confirm", res);
                        render_button("btn_engage", res);
                        device.ready_LED.render(res);
                        render_lock(res);
                    }
                }
            } else {
                console.log(err);
            }
        }

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "stellant/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(0);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
