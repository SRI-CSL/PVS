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
        "widgets/medical/Syringe/Syringe",
        "widgets/LED",
        "widgets/LED2",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"],
    function (TouchscreenButton,
              BasicDisplay,
              Syringe,
              LED,
              LED2,
              ButtonActionsQueue,
              stateParser,
              PVSioWebClient) {
        "use strict";
        var d3 = require("d3/d3");
        var client = PVSioWebClient.getInstance();

        var tick;
        function start_tick() {
            if (!tick) {
                tick = setInterval(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
                }, 1000);
            }
        }
        function stop_tick() {
            if (tick) {
                clearInterval(tick);
                tick = null;
            }
        }

        // append displays
        var device = {};

        //TODO: transform these displays in proper pvsioweb widgets
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
            top: 612,
            left: 176,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_inc_contrast = new TouchscreenButton("inc_contrast", {
            top: 612,
            left: 261,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_dec_saline = new TouchscreenButton("dec_saline", {
            top: 652,
            left: 176,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.btn_dec_contrast = new TouchscreenButton("dec_contrast", {
            top: 652,
            left: 261,
            width: 25,
            height: 25
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });

        device.btn_auto_LED = new LED2("btn_auto_LED", {
            top: 631,
            left: 216,
            width: 30,
            height: 30
        }, {
            parent: "stellant"
        });
        device.btn_auto = new TouchscreenButton("btn_auto", {
            top: 627,
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

        device.btn_manual_LED = new LED2("btn_manual_LED", {
            top: 796,
            left: 214,
            width: 30,
            height: 30
        }, {
            parent: "stellant"
        });
        device.btn_manual = new TouchscreenButton("btn_manual", {
            top: 792,
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
        device.btn_fUP_saline = new TouchscreenButton("btn_fUP_saline", {
            top: 700,
            left: 118,
            width: 63,
            height: 78
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_fUP_contrast = new TouchscreenButton("btn_fUP_contrast", {
            top: 700,
            left: 277,
            width: 63,
            height: 78
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_sUP_saline = new TouchscreenButton("btn_sUP_saline", {
            top: 780,
            left: 120,
            width: 63,
            height: 60
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_sUP_contrast = new TouchscreenButton("btn_sUP_contrast", {
            top: 780,
            left: 275,
            width: 63,
            height: 60
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_sDOWN_saline = new TouchscreenButton("btn_sDOWN_saline", {
            top: 845,
            left: 120,
            width: 63,
            height: 60
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_sDOWN_contrast = new TouchscreenButton("btn_sDOWN_contrast", {
            top: 845,
            left: 275,
            width: 63,
            height: 60
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_fDOWN_saline = new TouchscreenButton("btn_fDOWN_saline", {
            top: 900,
            left: 118,
            width: 63,
            height: 78
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });
        device.btn_fDOWN_contrast = new TouchscreenButton("btn_fDOWN_contrast", {
            top: 900,
            left: 277,
            width: 63,
            height: 78
        }, {
            parent: "stellant",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20
        });


        device.btn_fill_saline = new TouchscreenButton("btn_fill_saline", {
            top: 625,
            left: 110,
            width: 63,
            height: 36
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20,
            opacity: 0.6
        });
        device.btn_fill_contrast = new TouchscreenButton("btn_fill_contrast", {
            top: 627,
            left: 289,
            width: 63,
            height: 36
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: 20,
            opacity: 0.6
        });

        device.btn_prime_LED = new LED2("btn_prime_LED", {
            top: 719,
            left: 215,
            width: 30,
            height: 30
        }, {
            parent: "stellant"
        });
        device.btn_prime = new TouchscreenButton("btn_prime", {
            top: 715,
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

        device.btn_confirm_LED = new LED2("btn_confirm_LED", {
            top: 869,
            left: 213,
            width: 30,
            height: 30
        }, {
            parent: "stellant"
        });
        device.btn_confirm = new TouchscreenButton("btn_confirm", {
            top: 863,
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

        device.btn_engage_LED = new LED2("btn_engage_LED", {
            top: 960,
            left: 211,
            width: 30,
            height: 30
        }, {
            parent: "stellant"
        });
        device.btn_engage = new TouchscreenButton("btn_engage", {
            top: 953,
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
        // device.lock = new BasicDisplay("lock", {
        //     top: 913,
        //     left: 220,
        //     width: 24,
        //     height: 24
        // }, {
        //     parent: "stellant",
        //     callback: onMessageReceived,
        //     fontColor: "black",
        //     backgroundColor: "transparent" // does this button light up?
        // });
        device.ready_LED = new LED("ready_LED", {
            top: 916,
            left: 221,
            width: 13,
            height: 13
        }, {
            parent: "stellant",
            visibleWhen: "mode = READY",
            callback: onMessageReceived,
            backgroundColor: "transparent" // does this button light up?
        });
        device.saline_syringe = new Syringe("saline_syringe", {
            top: 21,
            left: 110
        }, {
            needle_style: "blue",
            fluid_color: "aqua",
            fittings: "vial",
            large_syringe: true,
            automatic_plunger: true,
            parent: "stellant"
        });
        device.contrast_syringe = new Syringe("contrast_syringe", {
            top: 21,
            left: 270
        }, {
            needle_style: "green",
            fittings: "vial",
            large_syringe: true,
            automatic_plunger: true,
            parent: "stellant"
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
                device[b].render();
                if (b === "btn_fill_saline" || b === "btn_fill_contrast") {
                    if (res[b] === "BLINKING") {
                        if (b === "btn_fill_saline") {
                            device[b].render("", { blinking: true, backgroundColor: "cyan" });
                        } else {
                            device[b].render("", { blinking: true, backgroundColor: "#7FE817" }); // Bright Green
                        }
                    } else if (res[b] === "LIGHT") {
                        if (b === "btn_fill_saline") {
                            device[b].render("", { blinking: false, backgroundColor: "deepskyblue" });
                        } else {
                            device[b].render("", { blinking: false, backgroundColor: "#7FE817" });
                        }
                    } else {
                        device[b].render();
                    }
                }
                if (device[b + "_LED"]) {
                    if (res[b] === "BLINKING") {
                        device[b + "_LED"].blink();
                    } else if (res[b] === "LIGHT") {
                        device[b + "_LED"].on();
                    } else {
                        device[b + "_LED"].off();
                    }
                }
            }
            function render_lock(red) {
                // if (res.mode === "READY") {
                //     device.lock.renderGlyphicon("glyphicon-lock");
                // } else {
                //     device.lock.hide();
                // }
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
                        render_button("btn_manual", res);
                        render_button("btn_prime", res);
                        render_button("btn_confirm", res);
                        render_button("btn_engage", res);
                        device.ready_LED.render(res);
                        render_lock(res);
                        device.saline_syringe.render(res.plunger_saline);
                        device.contrast_syringe.render(res.plunger_contrast);
                        if (res.mode === "PULL_PLUNGER" || res.mode === "PRIMING") {
                            start_tick();
                        } else {
                            stop_tick();
                        }
                        device.btn_sUP_saline.render();
                        device.btn_sUP_contrast.render();
                        device.btn_fUP_saline.render();
                        device.btn_fUP_contrast.render();
                        device.btn_sDOWN_saline.render();
                        device.btn_sDOWN_contrast.render();
                        device.btn_fDOWN_saline.render();
                        device.btn_fDOWN_contrast.render();
                    }
                }
            } else {
                console.log(err);
            }
        }

        d3.select("#plug_saline_vial").on("click", function () {
            device.saline_syringe.plugVial();
        });
        d3.select("#remove_saline_fittings").on("click", function () {
            device.saline_syringe.removeFittings();
        });
        d3.select("#plug_saline_infusion_set").on("click", function () {
            device.saline_syringe.plugInfusionSet();
        });
        d3.select("#plug_contrast_vial").on("click", function () {
            device.contrast_syringe.plugVial();
        });
        d3.select("#remove_contrast_fittings").on("click", function () {
            device.contrast_syringe.removeFittings();
        });
        d3.select("#plug_contrast_infusion_set").on("click", function () {
            device.contrast_syringe.plugInfusionSet();
        });

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "stellantV1/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(0);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "inline-flex");
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
