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
        "widgets/med/Syringe/Syringe",
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

        var tick = {
            plunger: null,
            time: null
        };
        function start_tick_plunger() {
            if (!tick.plunger) {
                tick.plunger = setInterval(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
                }, 500);
            }
        }
        function start_tick_time() {
            if (!tick.time) {
                tick.time = setInterval(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
                }, 60 * 1000);
            }
        }
        function stop_tick_plunger() {
            if (tick.plunger) {
                clearInterval(tick.plunger);
                tick.plunger = null;
            }
        }
        function stop_tick_time() {
            if (tick.time) {
                clearInterval(tick.time);
                tick.time = null;
            }
        }
        function stop_tick() {
            stop_tick_time();
            stop_tick_plunger();
        }
        function getDate() {
            function format(x) {
                if (parseFloat(x) < 9) {
                    return "0" + x;
                }
                return x.toString();
            }
            var date = new Date();
            return date.getDate() + "/" + format(date.getMonth() + 1) + date.getFullYear()
                        + " " +  format(date.getHours()) + ":" + format(date.getMinutes());// + ":" + format(date.getSeconds());
        }

        var device = {};

        //TODO: transform these displays in proper pvsioweb widgets
        device.plunger_saline = new SegmentDisplay("vol_saline");
        device.plunger_saline.pattern = "###";
        device.plunger_saline.displayAngle = 10;
        device.plunger_saline.digitHeight = 3;
        device.plunger_saline.digitWidth = 2;
        device.plunger_saline.digitDistance = 1;
        device.plunger_saline.segmentWidth = 0.3;
        device.plunger_saline.segmentDistance = 0.1;
        device.plunger_saline.segmentCount = 7;
        device.plunger_saline.cornerType = 3;
        device.plunger_saline.colorOn = "#001fff";
        device.plunger_saline.colorOff = "#1b0033";
        device.plunger_saline.draw();
        device.plunger_saline.setValue("");

        device.plunger_contrast = new SegmentDisplay("vol_contrast");
        device.plunger_contrast.pattern = "###";
        device.plunger_contrast.displayAngle = 10;
        device.plunger_contrast.digitHeight = 3;
        device.plunger_contrast.digitWidth = 2;
        device.plunger_contrast.digitDistance = 1;
        device.plunger_contrast.segmentWidth = 0.3;
        device.plunger_contrast.segmentDistance = 0.1;
        device.plunger_contrast.segmentCount = 7;
        device.plunger_contrast.cornerType = 3;
        device.plunger_contrast.colorOn = "#24ff22";
        device.plunger_contrast.colorOff = "#082605";
        device.plunger_contrast.draw();
        device.plunger_contrast.setValue("");

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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px"
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
            borderRadius: "20px",
            opacity: 0.6,
            keyCode: 65 // A
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
            borderRadius: "20px",
            opacity: 0.6,
            keyCode: 83 // S
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
            borderRadius: "20px",
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
            borderRadius: "20px",
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
            borderRadius: "20px",
            opacity: 0.5
        });
        device.btn_stop = new TouchscreenButton("btn_stop", {
            top: 1022,
            left: 120,
            width: 85,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: "20px",
            opacity: 0.5
        });
        device.btn_start = new TouchscreenButton("btn_start", {
            top: 1022,
            left: 245,
            width: 85,
            height: 38
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: "20px",
            opacity: 0.5
        });
        device.lock_LED = new LED("lock_LED", {
            top: 916,
            left: 221,
            width: 13,
            height: 13
        }, {
            parent: "stellant",
            callback: onMessageReceived,
            backgroundColor: "transparent" // does this button light up?
        });
        device.syringe_saline = new Syringe("syringe_saline", {
            top: 21,
            left: 110
        }, {
            needle_style: "blue",
            fluid_color: "aqua",
            large_syringe: true,
            automatic_plunger: true,
            parent: "stellant"
        });
        device.syringe_contrast = new Syringe("syringe_contrast", {
            top: 21,
            left: 270
        }, {
            needle_style: "green",
            large_syringe: true,
            automatic_plunger: true,
            parent: "stellant"
        });
        device.console = {};
        device.console.btn_ACC = new TouchscreenButton("btn_ACC", {
            top: 1075,
            left: 884,
            width: 30,
            height: 32
        }, {
            parent: "stellant_console_base",
            label: "ACC",
            evts: ["press/release"],
            callback: onMessageReceived,
            backgroundColor: "transparent"
        });
        device.console.LED_ACC = new LED("console_LED_ACC", {
            top: 1060,
            left: 888,
            width: 20,
            height: 20
        }, {
            parent: "stellant_console_base",
            color: "orange",
            callback: onMessageReceived,
            backgroundColor: "transparent" // does this button light up?
        });
        // device.console.btn_START = new TouchscreenButton("btn_START", {
        //     top: 1075,
        //     left: 980,
        //     width: 110,
        //     height: 32
        // }, {
        //     parent: "stellant_console_base",
        //     label: "ACC",
        //     evts: ["press/release"],
        //     callback: onMessageReceived,
        //     backgroundColor: "transparent"
        // });
        device.console.btn_confirm_security = new TouchscreenButton("btn_confirm_security", {
            top: 920,
            left: 1100,
            width: 130,
            height: 38
        }, {
            parent: "security_screen",
            softLabel: "Continua",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "whitesmoke"
        });
        device.console.rate_contrast = new BasicDisplay("console_rate_contrast", {
            top: 505,
            left: 915,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "#7FE817", // bright green
            fontColor: "black"
        });
        device.console.vol_contrast = new BasicDisplay("console_vol_contrast", {
            top: 505,
            left: 1000,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "#7FE817", // bright green
            fontColor: "black"
        });
        device.console.time_contrast = new BasicDisplay("console_time_contrast", {
            top: 505,
            left: 1085,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "transparent",
            fontColor: "white",
            format: "mm:ss",
            fontsize: 16
        });
        device.console.rate_saline = new BasicDisplay("console_rate_saline", {
            top: 544,
            left: 915,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "cyan",
            fontColor: "black"
        });
        device.console.vol_saline = new BasicDisplay("console_vol_saline", {
            top: 544,
            left: 1000,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "cyan",
            fontColor: "black"
        });
        device.console.time_saline = new BasicDisplay("console_time_saline", {
            top: 544,
            left: 1085,
            width: 80,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "transparent",
            fontColor: "white",
            format: "mm:ss",
            fontsize: 16
        });
        device.console.protocol = new BasicDisplay("console_protocol", {
            top: 420,
            left: 740,
            width: 420,
            height: 30
        }, {
            parent: "protocol_screen",
            backgroundColor: "transparent",
            fontColor: "black",
            align: "left",
            fontsize: 26
        });
        device.console.btn_console_lock = new TouchscreenButton("btn_console_lock", {
            top: 945,
            left: 1180,
            width: 160,
            height: 38
        }, {
            parent: "protocol_screen",
            softLabel: "Blocca",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            borderRadius: "20px",
            backgroundColor: "gold",
            visibleWhen: "console_cmd = LOCK"
        });
        device.console.btn_console_engage = new TouchscreenButton("btn_console_engage", {
            top: 945,
            left: 1180,
            width: 160,
            height: 38
        }, {
            parent: "protocol_screen",
            softLabel: "Arma",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            borderRadius: "20px",
            backgroundColor: "gold",
            visibleWhen: "console_cmd = ENGAGE"
        });
        device.console.btn_console_disengage = new TouchscreenButton("btn_console_disengage", {
            top: 945,
            left: 1180,
            width: 160,
            height: 38
        }, {
            parent: "protocol_screen",
            softLabel: "Disarma",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            borderRadius: "20px",
            backgroundColor: "gold",
            visibleWhen: "console_cmd = DISENGAGE"
        });
        device.console.btn_console_manage_protocol = new TouchscreenButton("btn_console_manage_protocol", {
            top: 416,
            left: 1180,
            width: 160,
            height: 38
        }, {
            parent: "protocol_screen",
            softLabel: "Gestione protocollo",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "whitesmoke"
        });
        device.console.btn_confirm_air_check_ok = new TouchscreenButton("btn_confirm_air_check_ok", {
            top: 383,
            left: 366,
            width: 130,
            height: 38
        }, {
            parent: "confirm_air_check_dialog",
            softLabel: "Si",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "steelblue"
        });
        device.console.btn_confirm_air_check_fail = new TouchscreenButton("btn_confirm_air_check_fail", {
            top: 383,
            left: 537,
            width: 130,
            height: 38
        }, {
            parent: "confirm_air_check_dialog",
            softLabel: "No",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "steelblue"
        });
        device.console.click_btn_confirm_volume_warning_ok = new TouchscreenButton("btn_confirm_volume_warning_ok", {
            top: 470,
            left: 288,
            width: 130,
            height: 38
        }, {
            parent: "insufficient_volume_dialog",
            softLabel: "Si",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "steelblue"
        });
        device.console.click_btn_confirm_volume_warning_fail = new TouchscreenButton("btn_confirm_volume_warning_fail", {
            top: 470,
            left: 450,
            width: 130,
            height: 38
        }, {
            parent: "insufficient_volume_dialog",
            softLabel: "No",
            fontsize: "14",
            fontColor: "black",
            callback: onMessageReceived,
            backgroundColor: "steelblue"
        });
        device.console.btn_start = new TouchscreenButton("btn_console_start", {
            top: 1069,
            left: 1008,
            width: 115,
            height: 38
        }, {
            parent: "stellant_console_screen",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: "20px"
        });
        device.console.btn_stop = new TouchscreenButton("btn_console_stop", {
            top: 1069,
            left: 737,
            width: 115,
            height: 38
        }, {
            parent: "stellant_console_screen",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            borderRadius: "20px"
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
            d3.select(".demo-splash").style("display", "none");
            d3.select(".content").style("display", "inline-flex");

            function prettyprint(v) {
                var x = parseInt(v);
                if (x < 10 ) {
                    return "00" + v;
                } else if (x < 100) {
                    return "0" + v;
                } else return v;
            }
            function render_saline_volume(res) {
                if (res.display_saline === "MIRROR_PLUNGER_LEVEL") {
                    d3.select("#display_saline").node().setAttribute("class", "");
                    device.plunger_saline.setValue(prettyprint(res.plunger_saline));
                } else if (res.display_saline === "MIRROR_TARGET_VOLUME") {
                    d3.select("#display_saline").node().setAttribute("class", "");
                    device.plunger_saline.setValue(prettyprint(res.vol_saline));
                } else if (res.display_saline === "DISP_INIT") {
                    d3.select("#display_saline").node().setAttribute("class", "blink");
                    device.plunger_saline.setValue("---");
                } else {
                    device.plunger_saline.setValue("");
                }
            }
            function render_contrast_volume(res) {
                if (res.display_contrast === "MIRROR_PLUNGER_LEVEL") {
                    d3.select("#display_contrast").node().setAttribute("class", "");
                    device.plunger_contrast.setValue(prettyprint(res.plunger_contrast));
                } else if (res.display_contrast === "MIRROR_TARGET_VOLUME") {
                    d3.select("#display_contrast").node().setAttribute("class", "");
                    device.plunger_contrast.setValue(prettyprint(res.vol_contrast));
                } else if (res.display_contrast === "DISP_INIT") {
                    d3.select("#display_contrast").node().setAttribute("class", "blink");
                    device.plunger_contrast.setValue("---");
                } else {
                    device.plunger_contrast.setValue("");
                }
            }
            function render_button(b, res) {
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
                } else {
                    device[b].render();
                }
                if (device[b + "_LED"]) {
                    if (res[b] === "BLINKING") {
                        device[b + "_LED"].blink();
                    } else if (res[b] === "BLINK3") {
                        device[b + "_LED"].blink(3);
                    } else if (res[b] === "LIGHT") {
                        device[b + "_LED"].on();
                    } else {
                        device[b + "_LED"].off();
                    }
                }
            }
            function render_lock(res) {
                device.lock_LED.render();
                if (res.lock_LED === "DARK") {
                    device.lock_LED.off();
                } else if (res.lock_LED === "LIGHT") {
                    device.lock_LED.render();
                } else if (res.lock_LED === "BLINK3") {
                    device.lock_LED.blink(3);
                }
            }
            function render_protocol(res) {
                if (res.console_protocol === "TOTAL_BODY_BARG") {
                    device.console.protocol.render(res.console_protocol.replace(/_/g," ") + "*");
                }
            }
            function render_date() {
                d3.select("#date").html(getDate());
            }
            function render_syringes(res) {
                if (res.mode === "OFF") {
                    // reset simulation buttons
                    d3.select("#plug_syringe_contrast").attr("style", "opacity:0.1;");
                    d3.select("#spike_contrast_bag").attr("style", "opacity:0.1;");
                    d3.select("#plug_syringe_saline").attr("style", "opacity:0.1;");
                    d3.select("#spike_saline_bag").attr("style", "opacity:0.1;");
                    d3.select("#connect_infusion_set").attr("style", "opacity:0.1;");
                    // remove all fittings attached to syringes
                    device.syringe_saline.removeFittings();
                    device.syringe_contrast.removeFittings();
                } else if (res.console_screen === "CONSOLE_PROTOCOL") {
                    // adjust simulation buttons
                    if (res.syringe_contrast_present === "TRUE") {
                        d3.select("#plug_syringe_contrast").attr("style", "opacity:0.1;");
                        if (res.bag_contrast_present === "FALSE") {
                            if (res.plunger_contrast === "0" && res.mode === "INIT_COMPLETE") {
                                d3.select("#spike_contrast_bag").attr("style", "opacity:1;");
                            }
                        } else {
                            d3.select("#spike_contrast_bag").attr("style", "opacity:0.1;");
                            device.syringe_contrast.plugVial();
                        }
                    } else {
                        d3.select("#plug_syringe_contrast").attr("style", "opacity:1;");
                    }

                    if (res.syringe_saline_present === "TRUE") {
                        d3.select("#plug_syringe_saline").attr("style", "opacity:0.1;");
                        if (res.bag_saline_present === "FALSE") {
                            if (res.plunger_saline === "0" && res.mode === "INIT_COMPLETE") {
                                d3.select("#spike_saline_bag").attr("style", "opacity:1;");
                            }
                        } else {
                            d3.select("#spike_saline_bag").attr("style", "opacity:0.1;");
                            device.syringe_saline.plugVial();
                        }
                    } else {
                        d3.select("#plug_syringe_saline").attr("style", "opacity:1;");
                    }

                    if (res.mode === "READY_TO_PRIME" || res.mode === "CONFIRM_PRIME" ||
                            (res.mode === "MANUAL" && parseFloat(res.plunger_saline) > 0 && parseFloat(res.plunger_contrast) > 0)) {
                        if (res.infusion_set_present === "FALSE") {
                            d3.select("#connect_infusion_set").attr("style", "opacity:1;");
                        } else {
                            device.syringe_saline.plugInfusionSet();
                            device.syringe_contrast.plugLateralConnectionLine();
                            d3.select("#connect_infusion_set").attr("style", "opacity:0.1;");
                        }
                    }
                }
                if (res.syringe_saline_present === "TRUE") {
                    if (res.fluid_in_saline_syringe === "TRUE") {
                        device.syringe_saline.containsFluid();
                        device.syringe_saline.render(res.plunger_saline);
                    } else {
                        device.syringe_saline.noFluid();
                        device.syringe_saline.render(res.plunger_saline);
                    }
                } else { device.syringe_saline.hide(); }
                if (res.syringe_contrast_present === "TRUE") {
                    if (res.fluid_in_contrast_syringe === "TRUE") {
                        device.syringe_contrast.containsFluid();
                        device.syringe_contrast.render(res.plunger_contrast);
                    } else {
                        device.syringe_contrast.noFluid();
                        device.syringe_contrast.render(res.plunger_contrast);
                    }
                } else { device.syringe_contrast.hide(); }

                if (res.mode === "INIT_SYRINGE" || res.mode === "AUTO" || res.mode === "PRIMING"
                        || res.mode === "MANUAL" //|| res.mode === "CONFIRM_PRIME"
                        || res.mode === "INFUSING") {
                    stop_tick_time();
                    start_tick_plunger();
                } else if (res.prime_warning === "TRUE" || res.lock_warning === "TRUE") {
                    // this is an optimisation for handling case 'prime warning' in the PVS model (see tick function)
                    stop_tick();
                    window.setTimeout(function () {
                        ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived); // single tick after 3 seconds
                        console.log("tick");
                    }, 3000);
                } else if (res.console_screen === "CONSOLE_PROTOCOL") {
                    stop_tick_plunger();
                    start_tick_time();
                } else {
                    stop_tick();
                }
            }
            function render_infusion_LEDs(res) {
                if (res.infusion_contrast_LED === "LIGHT") {
                    d3.select("#infusion_contrast_LED").classed("blink", false).style("opacity", "0.7");
                } else if (res.infusion_contrast_LED === "BLINKING") {
                    d3.select("#infusion_contrast_LED").classed("blink", true).style("opacity", "0.7");
                } else {
                    d3.select("#infusion_contrast_LED").classed("blink", false).style("opacity", "0");
                }

                if (res.infusion_saline_LED === "LIGHT") {
                    d3.select("#infusion_saline_LED").classed("blink", false).style("opacity", "0.9");
                } else if (res.infusion_saline_LED === "BLINKING") {
                    d3.select("#infusion_saline_LED").classed("blink", true).style("opacity", "0.9");
                } else {
                    d3.select("#infusion_saline_LED").classed("blink", false).style("opacity", "0");
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
                        render_button("btn_manual", res);
                        render_button("btn_prime", res);
                        render_button("btn_confirm", res);
                        render_button("btn_engage", res);
                        render_button("btn_start", res);
                        render_button("btn_stop", res);

                        render_lock(res);
                        render_infusion_LEDs(res);

                        render_syringes(res);

                        device.btn_sUP_saline.render();
                        device.btn_sUP_contrast.render();
                        device.btn_fUP_saline.render();
                        device.btn_fUP_contrast.render();
                        device.btn_sDOWN_saline.render();
                        device.btn_sDOWN_contrast.render();
                        device.btn_fDOWN_saline.render();
                        device.btn_fDOWN_contrast.render();
                        //-- console buttons & LEDs
                        device.console.btn_ACC.render();
                        device.console.btn_start.render();
                        device.console.btn_stop.render();
                        if (res.console_LED_ACC === "GREEN") {
                            device.console.LED_ACC.render(res, { color: "#5cb85c" }); // bright green
                        } else {
                            device.console.LED_ACC.render(res, { color: "orange" });
                        }
                        if (res.console_screen === "CONSOLE_SECURITY") {
                            d3.select("#security_screen").attr("style", "display:block");
                            device.console.btn_confirm_security.render();
                        } else {
                            d3.select("#security_screen").attr("style", "display:none");
                            device.console.btn_confirm_security.hide();
                        }
                        if (res.console_screen === "CONSOLE_PROTOCOL") {
                            d3.select("#protocol_screen").attr("style", "display:block");
                            device.console.vol_contrast.render(res);
                            device.console.vol_saline.render(res);
                            device.console.rate_contrast.render(res);
                            device.console.rate_saline.render(res);
                            device.console.time_contrast.render(res);
                            device.console.time_saline.render(res);
                            device.console.btn_console_lock.render(res);
                            device.console.btn_console_engage.render(res);
                            device.console.btn_console_disengage.render(res);
                            device.console.btn_console_manage_protocol.render(res);
                            if (res.console_dlg === "ASK_CONFIRM_AIR_CHECK") {
                                d3.select("#confirm_air_check_dialog").attr("style", "display:block;")
                                device.console.btn_confirm_air_check_ok.render(res);
                                device.console.btn_confirm_air_check_fail.render(res);
                            } else {
                                d3.select("#confirm_air_check_dialog").attr("style", "display:none;")
                                device.console.btn_confirm_air_check_ok.hide();
                                device.console.btn_confirm_air_check_fail.hide();
                            }
                            if (res.console_dlg === "VOLUME_WARNING") {
                                d3.select("#insufficient_volume_dialog").attr("style", "display:block;")
                                device.console.click_btn_confirm_volume_warning_ok.render(res);
                                device.console.click_btn_confirm_volume_warning_fail.render(res);
                            } else {
                                d3.select("#insufficient_volume_dialog").attr("style", "display:none;")
                                device.console.click_btn_confirm_volume_warning_ok.hide();
                                device.console.click_btn_confirm_volume_warning_fail.hide();
                            }
                            if (res.console_dlg === "MSG_INFUSION_COMPLETE") {
                                d3.select("#infusion_complete_dialog").attr("style", "display:block;")
                            } else {
                                d3.select("#infusion_complete_dialog").attr("style", "display:none;")
                            }
                            render_protocol(res);
                            render_date();
                        } else {
                            d3.select("#protocol_screen").attr("style", "display:none");
                            d3.select("#insufficient_volume_dialog").attr("style", "display:none;")
                            d3.select("#infusion_complete_dialog").attr("style", "display:none;")
                            device.console.vol_contrast.hide();
                            device.console.vol_saline.hide();
                            device.console.rate_contrast.hide();
                            device.console.rate_saline.hide();
                            device.console.time_contrast.hide();
                            device.console.time_saline.hide();
                            device.console.protocol.hide();
                            device.console.btn_console_lock.hide();
                            device.console.btn_console_engage.hide();
                            device.console.btn_console_disengage.hide();
                            device.console.btn_console_manage_protocol.hide();
                            device.console.btn_confirm_air_check_ok.hide();
                            device.console.btn_confirm_air_check_fail.hide();
                        }
                        if (res.injector_rotated === "TRUE") {
                            d3.select("#stellant")
                                .style("transition-duration", '400ms').style("transform", "rotateZ(-180deg)translateY(-25px)");
                            //     .style("transition-duration", '200ms').style("transform", "rotateX(90deg)translateY(-25px)");
                            // window.setTimeout(function () {
                            //     d3.select("#stellant")
                            //         .style("transition-duration", '400ms').style("transform", "rotateX(180deg)translateY(-25px)rotateY(180deg)");
                            // }, 200);
                        } else {
                            d3.select("#stellant").style("transition-duration", '400ms').style("transform", "rotateX(0deg)rotateY(0deg)translateY(0px)");
                        }
                    }
                }
            } else {
                console.log(err);
            }
        }


        d3.select("#plug_syringe_saline").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("plug_syringe_saline", onMessageReceived);
        });
        d3.select("#spike_saline_bag").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("plug_bag_saline", onMessageReceived);
        });
        d3.select("#plug_syringe_contrast").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("plug_syringe_contrast", onMessageReceived);
        });
        d3.select("#spike_contrast_bag").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("plug_bag_contrast", onMessageReceived);
        });
        d3.select("#connect_infusion_set").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("connect_infusion_set", onMessageReceived);
        });
        d3.select("#restart_simulation").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("restart_simulation", onMessageReceived);
        });
        d3.select("#rotate_injector").on("click", function () {
            ButtonActionsQueue.getInstance().queueGUIAction("rotate_injector", onMessageReceived);
        });

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "stellantV2PISA/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init_precache;", onMessageReceived);
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
