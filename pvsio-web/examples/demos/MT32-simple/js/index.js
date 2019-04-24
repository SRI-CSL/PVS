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
        "widgets/Button",
        "widgets/TouchscreenButton",
        "widgets/TouchscreenDisplay",
        "widgets/BasicDisplay",
        "widgets/NumericDisplay",
        "widgets/SliderWidget",
        "widgets/ToggleButton",

        "util/playback/Player",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"
    ], function (
        Button,
        TouchscreenButton,
        TouchscreenDisplay,
        BasicDisplay,
        NumericDisplay,
        Slider,
        ToggleButton,

        Player,
        ButtonActionsQueue,
        stateParser,
        PVSioWebClient
    ) {
        "use strict";
        var client = PVSioWebClient.getInstance();

        // var tick;
        // function start_tick() {
        //     if (!tick) {
        //         tick = setInterval(function () {
        //             ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
        //         }, 1000);
        //     }
        // }
        // function stop_tick() {
        //     if (tick) {
        //         clearInterval(tick);
        //         tick = null;
        //     }
        // }
        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            var ans = (v < 100) ? v.toFixed(1).toString() : v.toFixed(0).toString();
            return parseFloat(ans);
        }

        // Function automatically invoked by PVSio-web when the back-end sends states updates
        function onMessageReceived(err, event) {
            if (!err) {
                // get new state
                client.getWebSocket().lastState(event.data);
                // parse and render new state
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    render(stateParser.parse(res));
                    console.log(res);
                }
            } else {
                console.log(err);
            }
        }

        var leftpanel = {};
        leftpanel.umts = new TouchscreenButton("umts", {
            width: 48,
            height: 89,
            top: 73,
            left: 505
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.wireless = new TouchscreenButton("wireless", {
            width: 48,
            height: 89,
            top: 163,
            left: 505
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.bluetooth = new TouchscreenButton("bluetooth", {
            width: 48,
            height: 89,
            top: 253,
            left: 505
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.battery = new TouchscreenButton("battery", {
            width: 48,
            height: 89,
            top: 343,
            left: 505
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.alerts = new TouchscreenButton("alerts", {
            width: 48,
            height: 91,
            top: 433,
            left: 505
        }, {
            softLabel: "!",
            backgroundColor: "crimson",
            opacity: "0.8",
            fontsize: 48,
            borderColor: "#000066",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.back = new TouchscreenButton("back", {
            width: 48,
            height: 94,
            top: 525,
            left: 505
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        var home = {};
        home.new_exam = new TouchscreenButton("new_exam", {
            width: 178,
            height: 89,
            top: 253,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "homeScreen",
            callback: onMessageReceived
        });
        home.server = new TouchscreenButton("central", {
            width: 180,
            height: 89,
            top: 343,
            left: 643
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "homeScreen",
            callback: onMessageReceived
        });
        home.settings = new TouchscreenButton("settings", {
            width: 180,
            height: 89,
            top: 433,
            left: 732
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "homeScreen",
            callback: onMessageReceived
        });
        var exams = {};
        exams.ecg12d = new TouchscreenButton("ecg12d", {
            width: 178,
            height: 89,
            top: 253,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "examsScreen",
            callback: onMessageReceived
        });
        exams.test = new TouchscreenButton("test_electrodes", {
            width: 180,
            height: 89,
            top: 433,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "examsScreen",
            callback: onMessageReceived
        });

        var central = {};
        central.download_updates = new TouchscreenButton("download_updates", {
            width: 178,
            height: 89,
            top: 253,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "centralScreen",
            callback: onMessageReceived
        });
        central.upload_results = new TouchscreenButton("upload_results", {
            width: 178,
            height: 89,
            top: 253,
            left: 733
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "centralScreen",
            callback: onMessageReceived
        });
        central.terminate = new TouchscreenButton("terminate", {
            width: 178,
            height: 89,
            top: 343,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "centralScreen",
            callback: onMessageReceived
        });

        var settings = {};
        settings.connection_settings = new TouchscreenButton("connection_settings", {
            width: 178,
            height: 89,
            top: 253,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "settingsScreen",
            callback: onMessageReceived
        });
        settings.ecg_settings = new TouchscreenButton("ecg_settings", {
            width: 178,
            height: 89,
            top: 253,
            left: 733
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "settingsScreen",
            callback: onMessageReceived
        });
        settings.security_settings = new TouchscreenButton("security_settings", {
            width: 178,
            height: 89,
            top: 343,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "settingsScreen",
            callback: onMessageReceived
        });
        settings.system_settings = new TouchscreenButton("system_settings", {
            width: 178,
            height: 89,
            top: 343,
            left: 733
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "settingsScreen",
            callback: onMessageReceived
        });
        settings.info = new TouchscreenButton("info", {
            width: 178,
            height: 89,
            top: 433,
            left: 555
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "settingsScreen",
            callback: onMessageReceived
        });
        var check = {};
        check.confirm = new TouchscreenButton("confirm", {
            width: 89,
            height: 89,
            top: 527,
            left: 827
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "checkPatientScreen",
            callback: onMessageReceived
        });

        var monitoring = {};
        monitoring.quit = new TouchscreenButton("quit", {
            width: 52,
            height: 104,
            top: 436,
            left: 506
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "monitoringScreen",
            callback: onMessageReceived
        });

        function hide_all_screens(res) {
            d3.select("#homeScreen").style("display", "none");
            d3.select("#examsScreen").style("display", "none");
            d3.select("#centralScreen").style("display", "none");
            d3.select("#settingsScreen").style("display", "none");
            d3.select("#testScreen").style("display", "none");
            d3.select("#checkPatientScreen").style("display", "none");
            d3.select("#monitoringScreen").style("display", "none");

            if (res.mode !== "MONITORING" && streaming) {
                reset_tracing_display();
            }

            d3.select("#leftPanel").style("display", "none");
        }

        function render(res) {
            hide_all_screens(res);

            if (res.mode === "HOME") {
                d3.select("#homeScreen").style("display", "block");
                home.new_exam.render();
                home.server.render();
                home.settings.render();
            } else if (res.mode === "EXAMS") {
                d3.select("#examsScreen").style("display", "block");
                exams.ecg12d.render();
                exams.test.render();
            } else if (res.mode === "CENTRAL") {
                d3.select("#centralScreen").style("display", "block");
                central.download_updates.render();
                central.upload_results.render();
                central.terminate.render();
            } else if (res.mode === "SETTINGS") {
                d3.select("#settingsScreen").style("display", "block");
                settings.connection_settings.render();
                settings.ecg_settings.render();
                settings.security_settings.render();
                settings.system_settings.render();
                settings.info.render();
            } else if (res.mode === "TEST") {
                d3.select("#testScreen").style("display", "block");
            } else if (res.mode === "CHECK_PATIENT") {
                d3.select("#checkPatientScreen").style("display", "block");
                check.confirm.render();
            } else if (res.mode === "MONITORING") {
                d3.select("#monitoringScreen").style("display", "block");
                monitoring.quit.render();
                if (!streaming) {
                    update_tracing_display();
                    streaming = setInterval(update_tracing_display, duration);
                }
            }

            // left panel
            if (res.mode === "HOME" || res.mode === "EXAMS" || res.mode === "CENTRAL"
                    || res.mode === "SETTINGS" || res.mode === "TEST" || res.mode === "CHECK_PATIENT"
                    || res.mode === "RESULTS") {
                d3.select("#leftPanel").style("display", "block");
                leftpanel.umts.render();
                leftpanel.wireless.render();
                leftpanel.bluetooth.render();
                leftpanel.battery.render();
                // leftpanel.alerts.render();
                if (res.mode !== "HOME") {
                    leftpanel.back.render();
                }
            }
        }

        var hour = d3.select("#div_hour");
        var date = d3.select("#div_date");
        var hhmmss = d3.select("#div_hhmmss");
        function set_clock() {
            var d = new Date();
            var hh = (d.getHours() < 10) ? "0" + d.getHours() : d.getHours();
            var mm = (d.getMinutes() < 10) ? "0" + d.getMinutes() : d.getMinutes();
            var ss = (d.getSeconds() < 10) ? "0" + d.getSeconds() : d.getSeconds();
            hour.text(hh + ":" + mm);
            date.text(d.toLocaleDateString());
            hhmmss.text(hh + ":" + mm + ":" + ss);
        }
        set_clock();
        var clock = setInterval(set_clock, 1000);

        var streaming = null;
        var run_trace_width = 0;
        var max_trace_width = 340;
        var run_trace_cursor_position = 0;
        var trace = "#div_run_trace";
        var run_trace = d3.select(trace);
        var run_trace_cursor = d3.select("#div_run_trace_cursor");
        var step = 20;
        var duration = 1000;
        run_trace.style("width", "0px");
        function reset_tracing_display() {
            run_trace_width = run_trace_cursor_position = 0;
            clearInterval(streaming);
            streaming = null;
            run_trace_cursor.transition().duration(0).style("margin-left", "0px");
            run_trace.transition().duration(0).style("width", "0px");
        }
        function update_tracing_display() {
            if (run_trace_cursor_position < max_trace_width) {
                run_trace_cursor_position += step;
                run_trace_width += step;
                run_trace.transition().duration(duration/2).style("width", run_trace_width + "px");
            } else {
                run_trace_width = 0;
                run_trace.style("z-index", "0");
                run_trace.transition().duration(duration).style("opacity", 0.4);
                trace = (trace === "#div_run_trace") ? "#div_run_trace_alt" : "#div_run_trace";
                run_trace = d3.select(trace);
                run_trace.style("z-index", "1").style("opacity", 1).style("width", "0px");
                run_trace_cursor_position = 0;
            }
            run_trace_cursor.transition().duration(duration/2).style("margin-left", run_trace_cursor_position + "px");
        }

        var demoFolder = "MT32-simple";

        function start_playback() {
            // var playlist = require("text!" + demoFolder + "/playback/ex1.json");
            var playlist = [
                { button: home.new_exam, timeStamp: 2000 },
                { button: exams.ecg12d, timeStamp: 4000 },
                { button: check.confirm, timeStamp: 6000 }
            ];
            var player = new Player();
            player.load(playlist);
            player.play();
        }

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket()
                .startPVSProcess({name: "MT32.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init;", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                // start playback
                // start_playback();
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();
    });
