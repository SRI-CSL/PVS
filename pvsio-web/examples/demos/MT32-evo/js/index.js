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
        "widgets/core/ButtonEVO",

        "util/playback/Player",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"
    ], function (
        TouchscreenButton,

        Player,
        ButtonActionsQueue,
        stateParser,
        PVSioWebClient
    ) {
        "use strict";
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

        var mt32_settings = {};
        mt32_settings.mt32_off = new TouchscreenButton("mt32_off", {
            width: 0,
            height: 0,
            top: 0,
            left: 0
        }, {
            softLabel: "",
            customFunctionText: "set_mt32_off",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "device",
            visibleWhen: "false",
            callback: onMessageReceived
        });
        mt32_settings.mt32_on_battery = new TouchscreenButton("mt32_on_battery", {
            width: 0,
            height: 0,
            top: 0,
            left: 0
        }, {
            softLabel: "",
            customFunctionText: "set_mt32_on_battery",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "device",
            visibleWhen: "false",
            callback: onMessageReceived
        });
        mt32_settings.mt32_charging = new TouchscreenButton("mt32_charging", {
            width: 0,
            height: 0,
            top: 0,
            left: 0
        }, {
            softLabel: "",
            customFunctionText: "set_mt32_charging",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "device",
            visibleWhen: "false",
            callback: onMessageReceived
        });
        mt32_settings.mt32_fully_charged = new TouchscreenButton("mt32_fully_charged", {
            width: 0,
            height: 0,
            top: 0,
            left: 0
        }, {
            softLabel: "",
            customFunctionText: "set_mt32_fully_charged",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "device",
            visibleWhen: "false",
            callback: onMessageReceived
        });
        mt32_settings.mt32_charging_error = new TouchscreenButton("mt32_charging_error", {
            width: 0,
            height: 0,
            top: 0,
            left: 0
        }, {
            softLabel: "",
            customFunctionText: "set_mt32_charging_error",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "blue",
            parent: "device",
            visibleWhen: "false",
            callback: onMessageReceived
        });


        var leftpanel = {};
        leftpanel.umts = new TouchscreenButton("umts", {
            width: 48,
            height: 89,
            top: 130,
            left: 226
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "white",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.wireless = new TouchscreenButton("wireless", {
            width: 48,
            height: 89,
            top: 220,
            left: 226
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "white",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.bluetooth = new TouchscreenButton("bluetooth", {
            width: 48,
            height: 89,
            top: 310,
            left: 226
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "white",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.battery = new TouchscreenButton("battery", {
            width: 48,
            height: 89,
            top: 400,
            left: 226
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "white",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        leftpanel.view_alerts = new TouchscreenButton("view_alerts", {
            width: 46,
            height: 85,
            top: 495,
            left: 228
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
            top: 582,
            left: 226
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "white",
            parent: "leftPanel",
            callback: onMessageReceived
        });
        var home = {};
        home.new_exam = new TouchscreenButton("new_exam", {
            width: 178,
            height: 89,
            top: 314,
            left: 277
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
            top: 404,
            left: 366
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "homeScreen",
            callback: onMessageReceived
        });
        home.settings = new TouchscreenButton("settings", {
            width: 177,
            height: 89,
            top: 494,
            left: 458
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
            top: 314,
            left: 277
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "examsScreen",
            callback: onMessageReceived
        });
        exams.holter = new TouchscreenButton("holter", {
            width: 178,
            height: 89,
            top: 314,
            left: 459
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "examsScreen",
            callback: onMessageReceived
        });
        exams.confirm = new TouchscreenButton("exams-confirm", {
            width: 131,
            height: 84,
            top: 586,
            left: 278
        }, {
            softLabel: "",
            customFunctionText: "click_confirm",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "confirmHolterScreen",
            callback: onMessageReceived
        });
        exams.test = new TouchscreenButton("test_electrodes", {
            width: 180,
            height: 89,
            top: 492,
            left: 277
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
            top: 314,
            left: 277
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
            top: 314,
            left: 458
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            borderColor: "#0066ff",
            opacity: "0.2",
            parent: "centralScreen",
            callback: onMessageReceived
        });
        central.terminate_operating_mode = new TouchscreenButton("terminate_operating_mode", {
            width: 178,
            height: 89,
            top: 402,
            left: 277
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
            top: 314,
            left: 275
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
            top: 314,
            left: 458
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
            top: 404,
            left: 275
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
            top: 404,
            left: 458
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
            top: 492,
            left: 275
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
            top: 585,
            left: 550
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
            top: 492,
            left: 230
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "monitoringScreen",
            callback: onMessageReceived
        });
        monitoring.rec = new TouchscreenButton("rec", {
            width: 52,
            height: 104,
            top: 389,
            left: 230
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "monitoringScreen",
            pushButton: true,
            callback: onMessageReceived
        });

        var results = {};
        results.repeat = new TouchscreenButton("repeat", {
            width: 342,
            height: 54,
            top: 224,
            left: 287
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "resultsScreen",
            pushButton: true,
            callback: onMessageReceived
        });
        results.interpretation = new TouchscreenButton("interpretation", {
            width: 342,
            height: 54,
            top: 305,
            left: 287
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "resultsScreen",
            pushButton: true,
            callback: onMessageReceived
        });
        results.physio = new TouchscreenButton("physio", {
            width: 342,
            height: 54,
            top: 382,
            left: 287
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderColor: "#000066",
            parent: "resultsScreen",
            pushButton: true,
            callback: onMessageReceived
        });

        var mt32 = {};
        mt32.power_btn = new TouchscreenButton("power_btn", {
            width: 33,
            height: 54,
            top: 709,
            left: 70
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            parent: "MT32-right",
            callback: onMessageReceived
        });
        mt32.confirm_poweroff = new TouchscreenButton("confirm_poweroff", {
            width: 132,
            height: 90,
            top: 588,
            left: 277
        }, {
            softLabel: "",
            customFunctionText: "click_confirm",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            parent: "confirmPowerOffScreen",
            callback: onMessageReceived
        });

        function viz(id, opt) {
            opt = opt || {};
            if (d3.select(id).node()) {
                if (opt.fade && d3.select(id).style("display") !== "block") {
                    d3.select(id).style("opacity", 0).transition().duration(300).style("opacity", 1).style("display", "block");
                } else {
                    d3.select(id).transition().duration(0).style("opacity", 1).style("display", "block");
                }
            }
        }
        function hide(id) {
            d3.select(id).style("display", "none");
        }
        function hide_all_screens(res) {
            // mt32
            hide("#homeScreen");
            hide("#examsScreen");
            hide("#centralScreen");
            hide("#settingsScreen");
            hide("#testScreen");
            hide("#checkPatientScreen");
            hide("#monitoringScreen");
            hide("#resultsScreen");
            hide("#confirmHolterScreen");
            if (res.mode !== "MONITORING" && streaming) {
                reset_tracing_display();
            }
            hide("#leftPanel");
            d3.selectAll(".led").style("display", "none");

            // ct64 -- OPTIMIZE-ME!
            // d3.selectAll(".CT64frame").style("display", "none");
            d3.selectAll(".CT64frame").style("opacity", 0);

            d3.selectAll(".CT64frame_inner").style("display", "none");
            d3.selectAll(".CT64Menu").style("display", "none");
            d3.selectAll(".monitorData").style("display", "none");
            d3.selectAll(".ptData").style("display", "none");
        }


        var ct64 = {};
        ct64.login = new TouchscreenButton("login", {
            width: 74,
            height: 36,
            top: 352,
            left: 905
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "ct64LoginScreen",
            callback: onMessageReceived
        });
        ct64.select_patient = new TouchscreenButton("select_patient", {
            width: 1324,
            height: 32,
            top: 284,
            left: 16
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            parent: "patientsScreen",
            toggleButton: true,
            visibleWhen: "ct64.known_patient = TRUE",
            callback: onMessageReceived
        });
        ct64.new_ecg = new TouchscreenButton("new_ecg", {
            width: 120,
            height: 36,
            top: 143,
            left: 1068
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "patientMGMScreen",
            callback: onMessageReceived
        });
        ct64.new_holter = new TouchscreenButton("new_holter", {
            width: 128,
            height: 36,
            top: 143,
            left: 1197
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "patientMGMScreen",
            callback: onMessageReceived
        });
        ct64.select_device = new TouchscreenButton("select_device", {
            width: 1229,
            height: 32,
            top: 431,
            left: 63
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            parent: "newMonitoringSessionScreen",
            toggleButton: true,
            callback: onMessageReceived
        });
        ct64.continue = new TouchscreenButton("continue", {
            width: 85,
            height: 34,
            top: 538,
            left: 1200
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "newMonitoringSessionScreen",
            callback: onMessageReceived
        });
        ct64.continue_holter_config = new TouchscreenButton("continue_holter_config", {
            width: 85,
            height: 34,
            top: 453,
            left: 1200
        }, {
            softLabel: "",
            customFunctionText: "click_continue",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "holterConfigScreen",
            callback: onMessageReceived
        });
        ct64.new_patient = new TouchscreenButton("new_patient", {
            width: 85,
            height: 36,
            top: 323,
            left: 1250
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            borderWidth: "4px",
            parent: "patientsScreen",
            callback: onMessageReceived
        });
        ct64.monitoring = new TouchscreenButton("monitoring", {
            width: 107,
            height: 46,
            top: 10,
            left: 355
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "patientsScreen",
            callback: onMessageReceived
        });
        ct64.upload_exams = new TouchscreenButton("upload_exams", {
            width: 124,
            height: 46,
            top: 10,
            left: 462
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "patientsScreen",
            callback: onMessageReceived
        });
        ct64.choose_exams_to_be_uploaded = new TouchscreenButton("choose_exams_to_be_uploaded", {
            width: 137,
            height: 38,
            top: 181,
            left: 32
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "uploadExamsScreen",
            callback: onMessageReceived
        });
        ct64.new_monitoring_session = new TouchscreenButton("new_monitoring_session", {
            width: 177,
            height: 36,
            top: 299,
            left: 1158
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ct64-monitoringScreen",
            callback: onMessageReceived
        });
        ct64.date_time_filters = new TouchscreenButton("date_time_filters", {
            width: 1324,
            height: 117,
            top: 78,
            left: 16
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ct64-monitoringScreen",
            callback: onMessageReceived
        });
        ct64.select_exam_data_hub = new TouchscreenButton("select_exam_data_hub", {
            width: 1324,
            height: 32,
            top: 260,
            left: 16
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ct64-monitoringScreen",
            callback: onMessageReceived
        });
        ct64.view_ecg = new TouchscreenButton("view_ecg", {
            width: 155,
            height: 44,
            top: 263,
            left: 33
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ecgMonitoringMenu",
            callback: onMessageReceived
        });
        ct64.HES = new TouchscreenButton("HES", {
            width: 84,
            height: 44,
            top: 263,
            left: 186
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ecgMonitoringMenu",
            callback: onMessageReceived
        });
        ct64.view_physio = new TouchscreenButton("view_physio", {
            width: 144,
            height: 44,
            top: 263,
            left: 268
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ecgMonitoringMenu",
            callback: onMessageReceived
        });
        ct64.write_report = new TouchscreenButton("write_report", {
            width: 134,
            height: 44,
            top: 263,
            left: 416
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ecgMonitoringMenu",
            callback: onMessageReceived
        });
        ct64.select_doctor = new TouchscreenButton("select_doctor", {
            width: 1017,
            height: 36,
            top: 390,
            left: 274
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "requestReportScreen",
            callback: onMessageReceived
        });
        ct64.select_exam_data_holter = new TouchscreenButton("select_exam_data_holter", {
            width: 1324,
            height: 32,
            top: 228,
            left: 16
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "ct64-monitoringScreen",
            callback: onMessageReceived
        });
        ct64.select_visit = new TouchscreenButton("select_visit", {
            width: 1019,
            height: 36,
            top: 220,
            left: 274
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "newMonSessionScreen",
            callback: onMessageReceived
        });
        ct64.holter_duration = new TouchscreenButton("holter_duration", {
            width: 993,
            height: 36,
            top: 275,
            left: 284
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "holterConfigScreen",
            callback: onMessageReceived
        });
        ct64.holter_derivation = new TouchscreenButton("holter_derivation", {
            width: 993,
            height: 36,
            top: 326,
            left: 284
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "holterConfigScreen",
            callback: onMessageReceived
        });
        ct64.holter_accelerometer = new TouchscreenButton("holter_accelerometer", {
            width: 28,
            height: 28,
            top: 386,
            left: 277
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "holterConfigScreen",
            callback: onMessageReceived
        });
        ct64.select_ecg2d_new_pt = new TouchscreenButton("select_ecg2d_new_pt", {
            width: 1019,
            height: 46,
            top: 48,
            left: 0
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "visits_menu",
            callback: onMessageReceived
        });
        ct64.continue_yes_no = new TouchscreenButton("continue_yes_no", {
            width: 82,
            height: 34,
            top: 0,
            left: 77
        }, {
            softLabel: "",
            customFunctionText: "click_continue",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "yes_no",
            callback: onMessageReceived
        });
        ct64.browse_medicaltech = new TouchscreenButton("browse_medicaltech", {
            width: 1314,
            height: 22,
            top: 45,
            left: 65
        }, {
            softLabel: "",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "browser-navbar",
            callback: onMessageReceived
        });
        ct64.select_report_status = new TouchscreenButton("select_report_status", {
            width: 1019,
            height: 36,
            top: 1160,
            left: 274
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "compileReportScreen",
            callback: onMessageReceived
        });
        ct64.save_report = new TouchscreenButton("save_report", {
            width: 62,
            height: 36,
            top: 1340,
            left: 1235
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "compileReportScreen",
            callback: onMessageReceived
        });
        ct64.view_patient_profile = new TouchscreenButton("view_patient_profile", {
            width: 144,
            height: 44,
            top: 145,
            left: 34
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "patientMenu",
            callback: onMessageReceived
        });
        ct64.view_medical_report = new TouchscreenButton("view_medical_report", {
            width: 118,
            height: 44,
            top: 145,
            left: 275
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "patientMenu",
            callback: onMessageReceived
        });
        ct64.view_archived_medical_reports = new TouchscreenButton("view_archived_medical_reports", {
            width: 172,
            height: 44,
            top: 145,
            left: 390
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "patientMenu",
            callback: onMessageReceived
        });
        ct64.ct64_back = new TouchscreenButton("ct64_back", {
            width: 156,
            height: 34,
            top: 145,
            left: 32
        }, {
            softLabel: "",
            backgroundColor: "transparent",
            opacity: "0",
            borderColor: "#000066",
            borderRadius: "6px",
            parent: "medicalReportScreen",
            callback: onMessageReceived
        });


        function render_ct64_widgets (res) {
            ct64.login.render(res);
            ct64.select_patient.render(res);
            ct64.new_ecg.render(res);
            ct64.new_holter.render(res);
            ct64.holter_duration.render(res);
            ct64.holter_derivation.render(res);
            ct64.holter_accelerometer.render(res);
            ct64.select_device.render(res);
            ct64.continue.render(res);
            ct64.continue_holter_config.render(res);
            ct64.monitoring.render(res);
            ct64.upload_exams.render(res);
            ct64.choose_exams_to_be_uploaded.render(res);
            ct64.new_patient.render(res);
            ct64.new_monitoring_session.render(res);
            ct64.date_time_filters.render(res);
            ct64.select_exam_data_hub.render(res);
            ct64.HES.render(res);
            ct64.view_ecg.render(res);
            ct64.view_physio.render(res);
            ct64.write_report.render(res);
            ct64.select_doctor.render(res);
            ct64.select_exam_data_holter.render(res);
            ct64.select_visit.render(res);
            ct64.select_ecg2d_new_pt.render(res);
            ct64.continue_yes_no.render(res);
            ct64.browse_medicaltech.render(res);
            ct64.select_report_status.render(res);
            ct64.save_report.render(res);
            ct64.view_medical_report.render(res);
            ct64.ct64_back.render(res);
            ct64.view_archived_medical_reports.render(res);
            ct64.view_patient_profile.render(res);
        }



        var Demo = { NONE: 0, ACCESS_HOME_PAGE: 1, HUB_NEW_PT: 2, HUB_KNOWN_PT: 3, HOLTER: 4, TERMINATE_HUB_MODE: 5, TERMINATE_HOLTER_MODE: 6,
                     NEW_EXAM_HUB_MODE: 7, TEST_ELECTRODES_HUB: 8, VIEW_EXAMS_HUB: 9, VIEW_INTERPRETATION_HUB: 10, VIEW_INTERPRETATION_HOLTER: 11,
                     REQUEST_REPORT: 12, WRITE_REPORT: 13, VIEW_MEDICAL_REPORT: 14, VIEW_ARCHIVED_MEDICAL_REPORTS: 15,
                     MT32_LED: 16, CREATE_NEW_PATIENT: 17, TRANSFER_DATA_MICROSD: 18, INTRO: 19, POWER_ON_MT32: 20, POWER_OFF_MT32: 21,
                     VIEW_ALERTS: 22, SEND_RESULTS: 23 };
        var demo = Demo.NONE;



        var player = new Player({ lang: "it-IT" });
        if (demo === Demo.ACCESS_HOME_PAGE) {
            //-- access home page
            player.load([
                { speak: "Instruzioni per eseguire l'accesso alla centrale CT64", timeStamp: 1000 },
                { speak: "Collégati al sito www punto médicàl téc punto it", timeStamp: 3000 },
                { input: "#ct64_address", value: "http://www.medicaltech.it", timeStamp: 5000 },
                { button: ct64.browse_medicaltech, timeStamp: 7500, borderColor: "transparent" },
                { speak: "Inserisci le credenziali di accesso fornite dall'amministratore", timeStamp: 8500 },
                { input: "#email", value: "mario.rossi@medicaltech.it", timeStamp: 10500 },
                { input: "#password", value: "mylongsecretpassword", timeStamp: 13000 },
                { speak: "Clicca accedi.", timeStamp: 15000 },
                { button: ct64.login, timeStamp: 17000, timeout: 1600, borderColor: "blue", classed: "blink" },
                { speak: "L'home page della centrale verrà visualizzata, ed è pronta all'utilizzo.", timeStamp: 17500 }
            ]).play();
        } else if (demo === Demo.HUB_NEW_PT) {
            //-- activate ECG12D - HUB with new patient
            player.load([
                { speak: "Instruzioni per attivare la modalità operativa Hub, sul dispositivo MT32 quando l'anagrafica del paziente in esame non è presente sulla centrale.", timeStamp: 1000 },
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale.", timeStamp: 10500 },
                { button: ct64.monitoring, timeStamp: 13000, timeout: 2000, borderColor: "black", classed: "blink" },
                { speak: "Clicca Nuovo Monitoraggio dalla lista delle azioni.", timeStamp: 15000 },
                { button: ct64.new_monitoring_session, timeStamp: 17500, timeout: 2000, borderColor: "black", classed: "blink" },
                { speak: "Scegli Ecg12D Senza Anagrafica come Tipo Visita.", timeStamp: 18500 },
                { button: ct64.select_visit, timeStamp: 19000, timeout: 2000, borderColor: "transparent"},
                { button: ct64.select_ecg2d_new_pt, timeStamp: 21000, timeour: 2000, borderColor: "black", classed: "blink" },
                { speak: "Poi, clicca Continua.", timeStamp: 22000 },
                { button: ct64.continue_yes_no, timeStamp: 24000, timeout: 2000, borderColor: "black", classed: "blink" },
                { speak: "Clicca il nome del dispositivo per selezionarlo.", timeStamp: 26000 },
                { button: ct64.select_device, timeStamp: 28000, timeout: 2000 },
                { speak: "Poi, clicca Continua.", timeStamp: 29000 },
                { button: ct64.continue, timeStamp: 31000, timeout: 2000, borderColor: "black", classed: "blink" },
                { speak: "Attendi che il dispositivo MT32 abbia ricevuto gli aggiornamenti.", timeStamp: 32000 },
                { speak: "A questo punto, il dispositivo MT32 è configurato con la modalità operativa Hub, ed è pronto all’utilizzo.", timeStamp: 38000 },
                { speak: "L'anagrafica del paziente verrà inserita dal medico direttamente sull'MT32, in fase di avvio esame.", timeStamp: 45000 }
            ]).play();
        } else if (demo === Demo.HUB_KNOWN_PT) {
            //-- activate ECG12D - HUB with known patient
            player.load([
                { speak: "Instruzioni per attivare la modalità operativa Hub, sul dispositivo MT32 quando l'anagrafica del paziente in esame è già presente sulla centrale.", timeStamp: 1000 },
                { speak: "Fai doppio click sull'anagrafica desiderata.", timeStamp: 10500 },
                { button: ct64.select_patient, timeStamp: 15000, timeout: 1500, borderColor: "blue", classed: "blink2" },
                { speak: "Clicca Nuovo ECG.", timeStamp: 16500 },
                { button: ct64.new_ecg, timeStamp: 19000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Clicca il nome del dispositivo per selezionarlo.", timeStamp: 19500 },
                { button: ct64.select_device, timeStamp: 21000, timeout: 3000 },
                { speak: "Poi, clicca Continua.", timeStamp: 21500 },
                { button: ct64.continue, timeStamp: 23000, timeout: 1500, borderColor: "blue", classed: "blink" },
                { speak: "Attendi che il dispositivo MT32 abbia ricevuto gli aggiornamenti.", timeStamp: 24000 },
                { speak: "A questo punto, il dispositivo MT32 è configurato con la modalità operativa Hub, ed è pronto all'utilizzo.", timeStamp: 30500 }
            ]).play();
        } else if (demo === Demo.HOLTER) {
            player.load([
                { speak: "Instruzioni per attivare la modalità operativa Hólter, sul dispositivo MT32.", timeStamp: 1000 },
                { speak: "Nell'home page della centrale, fai doppio click sull'anagrafe desiderata.", timeStamp: 8500 },
                { button: ct64.select_patient, timeStamp: 13500, timeout: 2500, borderColor: "blue", classed: "blink2" },
                { speak: "Poi, nella pagina Gestione Paziente, clicca Nuovo Hólter.", timeStamp: 14500 },
                { button: ct64.new_holter, timeStamp: 18000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Seleziona Durata Esame e Derivazione Precordiale che intendi utilizzare.", timeStamp: 19500 },
                { button: ct64.holter_duration, timeStamp: 20000, timeout: 1000, borderColor: "blue", classed: "blink2" },
                { button: ct64.holter_derivation, timeStamp: 21000, timeout: 1000, borderColor: "blue", classed: "blink2" },
                { speak: "Inotre, seleziona Attiva Accelerómetro se intendi attivare l'acquisizione dei dati acceleromètrici.", timeStamp: 24500 },
                { button: ct64.holter_accelerometer, timeStamp: 25500, timeout: 1000, borderColor: "blue", classed: "blink2" },
                { speak: "Poi, clicca Continua.", timeStamp: 33000 },
                { button: ct64.continue_holter_config, timeStamp: 34500, timeout: 1500, borderColor: "blue", classed: "blink" },
                { speak: "Clicca il nome del dispositivo per selezionarlo.", timeStamp: 37000 },
                { button: ct64.select_device, timeStamp: 38500, timeout: 3000 },
                { speak: "Poi, clicca Continua.", timeStamp: 40000 },
                { button: ct64.continue, timeStamp: 42000, timeout: 1500, borderColor: "blue", classed: "blink" },
                { speak: "Attendi che il dispositivo MT32 abbia ricevuto gli aggiornamenti.", timeStamp: 43000 },
                { speak: "A questo punto, il dispositivo MT32 è stato configurato in modalità operativa Hólter, ed è pronto all'utilizzo.", timeStamp: 49000 }
            ]).play();
        } else if (demo === Demo.TERMINATE_HUB_MODE) {
            player.load([
                { speak: "Instruzioni per terminare la modalità operativa Hub, sul dispositivo MT32.", timeStamp: 2000 },
                { speak: "Clicca Centrale.", timeStamp: 8500 },
                { button: home.server, timeStamp: 10000},
                { speak: "Poi, clicca Termina modalità.", timeStamp: 11500 },
                { button: central.terminate_operating_mode, timeStamp: 12500 },
                { speak: "Clicca indietro per tornare all'home page.", timeStamp: 14000},
                { button: leftpanel.back, timeStamp: 17000 },
                { speak: "Potrai verificare che la modalità operativa non è piu presente sul dispositivo MT32.", timeStamp: 18500},
                { speak: "Applica la stessa procedura per terminare la modalità Hólter.", timeStamp: 24500}
            ]).play();
        } else if (demo === Demo.NEW_EXAM_HUB_MODE) {
            player.load([
                { speak: "Istruzioni per creare un nuovo esame in modalità operativa Hub.", timeStamp: 1000 },
                { speak: "Verifica che il dispositivo MT32 sia in modalità EGC12D Hub.", timeStamp: 6400 },
                { speak: "Poi, Clicca Nuovo Esame.", timeStamp: 12000 },
                { button: home.new_exam, timeStamp: 14000, borderColor: "white" },
                { speak: "Clicca ECG12D.", timeStamp: 15000, borderColor: "white" },
                { button: exams.ecg12d, timeStamp: 16500, borderColor: "white" },
                { speak: "Verifica i dati del paziente.", timeStamp: 18000 },
                { speak: "Poi, clicca Conferma.", timeStamp: 19500 },
                { button: check.confirm, timeStamp: 21000, borderColor: "white" },
                { speak: "Aspetta qualche secondo che il filtro sia a regime.", timeStamp: 23000 },
                { speak: "Poi, clicca rec per acquisire il segnale elettrocardiografico.", timeStamp: 29000 },
                { button: monitoring.rec, timeStamp: 30000, timeout: 1000, classed: "blink2", borderColor: "white" },
                { speak: "Al termine dell'acquisizione si passa automaticamente alla pagina Risultato.", timeStamp: 35800 },
                { speak: "Dove puoi scegliere se ripetere l'acquisizione. Vedere l'esito interpretativo dell'acquisizione. O inserire i dati fisiologici.", timeStamp: 40000 },
                { button: results.repeat, timeStamp: 41000, timeout: 1000, classed: "blink2", borderColor: "white", stutter: true },
                { button: results.interpretation, timeStamp: 42500, timeout: 1000, classed: "blink2", borderColor: "white", stutter: true },
                { button: results.physio, timeStamp: 45000, timeout: 1000, classed: "blink2", borderColor: "white", stutter: true }
            ]).play();
        } else if (demo === Demo.NEW_EXAM_HOLTER_MODE) {
            player.load([
                { speak: "Istruzioni per creare un nuovo esame in modalità operativa Hólter.", timeStamp: 1000 },
                { speak: "Verifica che il dispositivo MT32 sia in modalità Hólter.", timeStamp: 6400 },
                { speak: "Poi, Clicca Nuovo Esame.", timeStamp: 12000 },
                { button: home.new_exam, timeStamp: 14000, borderColor: "white" },
                { speak: "Clicca Hólter.", timeStamp: 15000, borderColor: "white" },
                { button: exams.holter, timeStamp: 16500, borderColor: "white" },
                { speak: "Verifica i dati del paziente.", timeStamp: 17500 },
                { speak: "Poi, clicca Conferma.", timeStamp: 19500 },
                { button: check.confirm, timeStamp: 21000, timeout: 2000, borderColor: "white" },
                { speak: "Aspetta qualche secondo che il filtro sia a regime.", timeStamp: 23000 },
                { speak: "Poi, clicca rec.", timeStamp: 28000 },
                { button: monitoring.rec, timeStamp: 30000, timeout: 1000, classed: "blink2", borderColor: "white" },
                { speak: "Comparirà una finestra di conferma.", timeStamp: 30000 },
                { speak: "Clicca conferma.", timeStamp: 31000 },
                { button: exams.confirm, timeStamp: 34000, timeout: 1000, borderColor: "white" },
                { speak: "La registrazione Hólter verrà effettuata per la durata impostata in fase di configurazione dell'esame.", timeStamp: 36000 }
            ]).play();
        } else if (demo === Demo.TEST_ELECTRODES_HUB) {
            setTimeout(function () {
                d3.select("#defective_electrode").style("display", "block");
            }, 36000);
            player.load([
                { speak: "Per ottenere ECG di buona qualità, è molto importante la corretta applicazione degli elettrodi.", timeStamp: 1000 },
                { speak: "Per verificare la corretta applicazione degli elettrodi, clicca Nuovo Esame.", timeStamp: 8000 },
                { button: home.new_exam, timeStamp: 12000, timeout: 1500, borderColor: "white" },
                { speak: "Poi, Clicca Test Elettrodi.", timeStamp: 13000 },
                { button: exams.test, timeStamp: 15500, timeout:1500, borderColor: "white" },
                { speak: "Nella pagina viene suggerito il posizionamento degli elettrodi e controllata la qualità di connessione.", timeStamp: 16000 },
                { speak: "Gli elettrodi sono identificati da pallini colorati.", timeStamp: 22000 },
                { speak: "Pallini stabili indicano una buona connessione.", timeStamp: 26000 },
                { speak: "Se un pallino è intermittente, c'è un elettrodo con una cattiva connessione che va controllato.", timeStamp: 30000 }
            ]).play();
        } else if (demo === Demo.VIEW_EXAMS_HUB) {
            player.load([
                { speak: "Instruzioni per vedere tutti gli esami svolti in un determinato periodo di tempo.", timeStamp: 1000 },
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale CT64.", timeStamp: 7000 },
                { button: ct64.monitoring, timeStamp: 12000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "La tabella al centro della schermata contiene l'elenco degli esami effettuati.", timeStamp: 14000 },
                { button: ct64.select_exam_data_hub, timeStamp: 16000, timeout: 2000, borderColor: "blue", classed: "blink2", stutter: true },
                { button: ct64.select_exam_data_holter, timeStamp: 16000, timeout: 2000, borderColor: "blue", classed: "blink2", stutter: true },
                { speak: "Puoi selezionare le date di interesse attraverso i filtri disponibili nella parte alta della pagina.", timeStamp: 19000 },
                { button: ct64.date_time_filters, timeStamp: 22000, timeout: 2000, borderColor: "blue", classed: "blink2", stutter: true }
            ]).play();
        } else if (demo === Demo.VIEW_INTERPRETATION_HUB) {
            player.load([
                { speak: "Instruzioni per visualizzare l'analisi di un esame ECG12D.", timeStamp: 1000 },
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale CT64.", timeStamp: 6000 },
                { button: ct64.monitoring, timeStamp: 10000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Clicca sull'esame ECG di interesse.", timeStamp: 14000 },
                { button: ct64.select_exam_data_hub, timeStamp: 16000, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Apparirà la schermata di analisi esame ECG, dove puoi selezionare i parametri di interesse da visualizzare.", timeStamp: 19000 },
                { speak: "Cliccando su H E S, puoi visualizzare il risultato interpretativo.", timeStamp: 27000 },
                { button: ct64.HES, timeStamp: 28000, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Cliccando su Dati Fisiologici, puoi vedere i dati fisiologici del paziente raccolti precedentemente.", timeStamp: 34000 },
                { button: ct64.view_physio, timeStamp: 36000, timeout: 2000, borderColor: "blue", classed: "blink2" }
            ]).play();
        } else if (demo === Demo.VIEW_INTERPRETATION_HOLTER) {
            player.load([
                { speak: "Instruzioni per visualizzare l'analisi di un esame Hólter.", timeStamp: 1000 },
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale CT64.", timeStamp: 6000 },
                { button: ct64.monitoring, timeStamp: 10000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Clicca sull'esame Hólter di interesse.", timeStamp: 14000 },
                { button: ct64.select_exam_data_holter, timeStamp: 16000, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Apparirà la schermata di analisi esame Hólter, dove puoi selezionare i vari parametri di interesse.", timeStamp: 19000 }
            ]).play();
        } else if (demo === Demo.REQUEST_REPORT) {
            player.load([
                { speak: "Istruzioni per richiedere la refertazione di un esame ECG.", timeStamp: 1000 },
                { speak: "Questa procedura va utilizzata quando l'operatore non ha l'autorizzazione per refertare.", timeStamp: 4500},
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale CT64.", timeStamp: 12000 },
                { button: ct64.monitoring, timeStamp: 16000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Clicca sull'esame ECG di interesse.", timeStamp: 20000 },
                { button: ct64.select_exam_data_hub, timeStamp: 22000, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Poi, clicca Refertazione.", timeStamp: 24000 },
                { button: ct64.write_report, timeStamp: 26000, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Infine, seleziona il medico al quale vuoi richiedere la refertazione.", timeStamp: 28000 },
                { button: ct64.select_doctor, timeStamp: 32000, timeout: 2000, borderColor: "blue", classed: "blink" }
            ]).play();
        } else if (demo === Demo.WRITE_REPORT) {
            player.load([
                { speak: "Istruzioni per effettuare la refertazione di un esame ECG.", timeStamp: 1000 },
                { speak: "Clicca Monitoraggi nel Pannello di Navigazione della centrale CT64.", timeStamp: 5500 },
                { button: ct64.monitoring, timeStamp: 8500, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Clicca sull'esame ECG di interesse.", timeStamp: 12500 },
                { button: ct64.select_exam_data_hub, timeStamp: 14500, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Poi, clicca Refertazione.", timeStamp: 16500 },
                { button: ct64.write_report, timeStamp: 18500, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Compila la diagnosi del paziente.", timeStamp: 20500 },
                { input: "#diagnosis", value: "Nella norma", timeStamp: 21000 },
                { scroll: "#MONITORING_SCREENS", timeStamp: 23500, offset: 300 },
                { speak: "Indica, la terapia consigliata.", timeStamp: 24500 },
                { input: "#therapy", value: "Nessuna", timeStamp: 27000 },
                { scroll: "#MONITORING_SCREENS", timeStamp: 28000, offset: 700 },
                { speak: "Indica, gli esami consigliati.", timeStamp: 28500 },
                { input: "#recommended_exams", value: "Nessuno", timeStamp: 29000 },
                { speak: "Seleziona lo stato appropriato del referto dal menu a tendina.", timeStamp: 30000 },
                { button: ct64.select_report_status, timeStamp: 32000, timeout: 2500, borderColor: "blue", classed: "blink2" },
                { speak: "In fine, clicca su Salva per salvare il referto.", timeStamp: 35000 },
                { button: ct64.save_report, timeStamp: 37000, timeout: 2000, borderColor: "blue", classed: "blink" },
                { speak: "Comparirà una schermàta riassuntiva che mostra il referto salvato.", timeStamp: 40000 },
                { scroll: "#MONITORING_SCREENS", timeStamp: 40500, offset: 0 }
            ]).play();
        } else if (demo === Demo.VIEW_MEDICAL_REPORT) {
            player.load([
                { speak: "Istruzioni per consultare l'anamnesi del paziente.", timeStamp: 1000 },
                { speak: "Nell'home page della centrale, fai doppio click sull'anagrafe desiderata.", timeStamp: 5000 },
                { button: ct64.select_patient, timeStamp: 10500, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Poi, nella pagina Gestione Paziente, clicca Anamnesi.", timeStamp: 12500 },
                { button: ct64.view_medical_report, timeStamp: 15000, timeout: 2500, borderColor: "blue", classed: "blink2" },
                { speak: "Verrà visualizzata l'anamnesi del paziente selezionato.", timeStamp: 18000 }
            ]).play();
        } else if (demo === Demo.VIEW_ARCHIVED_MEDICAL_REPORTS) {
            player.load([
                { speak: "Istruzioni per consultare l'archivio di anamnesi del paziente.", timeStamp: 1000 },
                { speak: "Nell'home page della centrale, fai doppio click sull'anagrafe desiderata.", timeStamp: 6000 },
                { button: ct64.select_patient, timeStamp: 10500, timeout: 2000, borderColor: "blue", classed: "blink2" },
                { speak: "Clicca Archivio Anamnesi.", timeStamp: 13000 },
                { button: ct64.view_archived_medical_reports, timeStamp: 15000, timeout: 2500, borderColor: "blue", classed: "blink2" },
                { speak: "Verrà visualizzato l'elenco delle anamnesi archiviate.", timeStamp: 18000 }
            ]).play();
        } else if (demo === Demo.MT32_LED) {
            player.load([
                { speak: "Istruzioni per comprendere le segnalazioni fornite dal dispositivo MT32 attraverso il led frontale.", timeStamp: 1000 },
                { speak: "Se il led frontale è blu lampeggiante, il dispositivo MT32 è acceso.", timeStamp: 8000 },
                { button: mt32_settings.mt32_charging, timeStamp: 18000 },
                { speak: "Led arancione fisso, indica che il dispositivo MT32 è in carica.", timeStamp: 18500 },
                { button: mt32_settings.mt32_fully_charged, timeStamp: 27000 },
                { speak: "Led verde fisso, indica che il dispositivo MT32 ha concluso con esito positivo il processo di ricarica.", timeStamp: 27500 },
                { button: mt32_settings.mt32_charging_error, timeStamp: 36000 },
                { speak: "Led arancione lampeggiante indica una anomalia nel processo di ricarica.", timeStamp: 36500 },
                { speak: "L'anomalia può essere dovuta al superamento del tempo massimo previsto per la ricarica.", timeStamp: 40000 },
                { speak: "Oppure ad un malfunzionamento dell'hardware del dispositivo, ad esempio un guasto all'alimentatore o alla batteria interna.", timeStamp: 46000 },
                { speak: "Per escludere che l'anomalia sia dovuta a problemi hardware, si consiglia  di scollegare l'alimentatore per qualche minuto", timeStamp: 50000 },
                { speak: "E successivamente ripetere l'intero ciclo di carica a dispositivo spento, monitorando lo stato della batteria.", timeStamp: 56000 },
                { speak: "Se l'anomalia persiste, è necessario contattare l'assistenza.", timeStamp: 63000 }
            ]).play();
        } else if (demo === Demo.CREATE_NEW_PATIENT) {
            player.load([
                { speak: "Istruzioni per creare il profilo di un nuovo paziente nella centrale CT64.", timeStamp: 1000 },
                { speak: "Nella pagina Pazienti, fai click su Nuovo.", timeStamp: 7000 },
                { button: ct64.new_patient, timeStamp: 10000, timeout: 2500, borderColor: "blue", classed: "blink2" },
                { speak: "Verrà visualizzata una schermata che consente l'inserimento dei dati del nuovo paziente.", timeStamp: 11000 }
            ]).play();
        } else if (demo === Demo.TRANSFER_DATA_MICROSD) {
            player.load([
                { speak: "Istruzioni per caricare gli esami ECG tramite la centrale CT64.", timeStamp: 1000 },
                { speak: "Estrai la scheda micro-SD dal dispositivo MT32.", timeStamp: 7000 },
                { speak: "Inserisci la scheda nel lettore SD del computer usato per connetterti alla centrale CT64.", timeStamp: 12000 },
                { speak: "Connettiti alla centrale CT64 con le tue credenziali.", timeStamp: 20000 },
                { input: "#email", value: "mario.rossi@medicaltech.it", timeStamp: 20500 },
                { input: "#password", value: "mylongsecretpassword", timeStamp: 24000 },
                { button: ct64.login, timeStamp: 26000, timeout: 1600, borderColor: "blue", classed: "blink" },
                { speak: "Clicca Upload Esami nel pannello di navigazione.", timeStamp: 29000 },
                { button: ct64.upload_exams, timeStamp: 32000, timeout: 3000, borderColor: "blue", classed: "blink" },
                { speak: "In fine, clicca il pulsante Upload Esami.", timeStamp: 35000 },
                { button: ct64.choose_exams_to_be_uploaded, timeStamp: 36000, timeout: 3000, borderColor: "blue", classed: "blink" },
                { speak: "Comparirà una schermata che ti permetterà di selezionare gli esami che vuoi caricare sulla centrale CT64.", timeStamp: 39000 }
            ]).play();
        } else if (demo === Demo.INTRO) {
            player.load([
                { speak: "Il Sistema Diagnostico Integrato Multifunzione médicàl téc; è uno strumento all'avanguardia nel panorama del telemonitoraggio cardiologico.", timeStamp: 1000 },
                { speak: "Il sistema è articolato in due componenti principali.", timeStamp: 10500 },
                { speak: "Il Terminale Paziente MT32 multi.", timeStamp: 13500 },
                { speak: "E la Centrale di Telemedicina CT64.", timeStamp: 17500 },
                { input: "#ct64_address", value: "http://www.medicaltech.it", timeStamp: 17800 },
                { button: ct64.browse_medicaltech, timeStamp: 19500, borderColor: "transparent" },

                { speak: "Il Terminale Paziente MT32 multi è un registratore di segnali elettro-cardio-gràfici portatile", timeStamp: 23500 },
                { button: home.new_exam, timeStamp: 25000, borderColor: "white" },
                { button: exams.ecg12d, timeStamp: 26000, borderColor: "white" },
                { button: check.confirm, timeStamp: 27000, borderColor: "white" },
                { speak: "Che fornisce una funzione elettro-cardiografo diagnostico a 12 derivazioni ECG", timeStamp: 26500 },
                { speak: "una funzione Hólter da 24 a 120 ore.", timeStamp: 30000 },
                { speak: "E una funzione Hub per l'acquisizione di parametri fisiologici da altri dispositivi.", timeStamp: 32000 },
                { button: monitoring.rec, timeStamp: 36000, timeout: 1000, classed: "blink2", borderColor: "white" },
                // { button: results.physio, timeStamp: 45000, timeout: 1000, classed: "blink2", borderColor: "white", stutter: true }

                { speak: "La Centrale di Telemedicina CT64 fornisce applicativi software per personalizzare e configurare il Terminale Paziente MT32.", timeStamp: 50000 },
                { input: "#email", value: "mario.rossi@medicaltech.it", timeStamp: 50500 },
                { input: "#password", value: "mylongsecretpassword", timeStamp: 53000 },
                { button: ct64.login, timeStamp: 55000, timeout: 1000, borderColor: "blue", classed: "blink" },

                { speak: "La centrale fornisce anche funzioni per la lettura, l'analisi, e l'archiviazione di esami cardiaci; e di altri parametri fisiologici registrati con il Terminale Paziente.", timeStamp: 61000 },
                { button: ct64.monitoring, timeStamp: 64000, timeout: 1000, borderColor: "blue", classed: "blink" },
                { button: ct64.select_exam_data_hub, timeStamp: 70000, timeout: 1000, borderColor: "blue", classed: "blink2" },

                { speak: "Gli esami e profili dei pazienti sono memorizzati in modo sicuro in una base di dati.", timeStamp: 74500 },
                { button: ct64.HES, timeStamp: 78000, timeout: 1000, borderColor: "blue", classed: "blink2" },
                { button: ct64.view_physio, timeStamp: 82000, timeout: 1000, borderColor: "blue", classed: "blink2" },

                { speak: "Utilizzando la centrale CT64, il medico specialista e il medico curante possono prontamente accedere agli esami ed ai profili dei pazienti.", timeStamp: 79000 },
                { speak: "Questo facilita quindi diagnosi e refertazione accurata dei pazienti.", timeStamp: 88000 },
                { button: ct64.write_report, timeStamp: 91000, timeout: 1000, borderColor: "blue", classed: "blink2" },
                { input: "#diagnosis", value: "Nella norma.", timeStamp: 94000 }
            ]).play();
        } else if (demo === Demo.POWER_ON_MT32) {
            player.load([
                { speak: "Istruzioni per accendere il dispositivo MT32.", timeStamp: 1000 },
                { speak: "Premi il pulsante power per almeno un secondo.", timeStamp: 7000 },
                { button: mt32.power_btn, timeStamp: 9000, timeout: 1500, borderColor: "blue" },
                { speak: "Comparirà la pagina principale ed il dispositivo è pronto all'uso.", timeStamp: 11000 },
            ]).play();
        } else if (demo === Demo.POWER_OFF_MT32) {
            player.load([
                { speak: "Istruzioni per spegnere il dispositivo MT32.", timeStamp: 1000 },
                { speak: "Premi il pulsante power per almeno un secondo.", timeStamp: 7000 },
                { button: mt32.power_btn, timeStamp: 9000, timeout: 1500, borderColor: "black" },
                { speak: "Comparirà una pagina per confermare l'operazione di spegnimento.", timeStamp: 11000 },
                { speak: "Clicca Conferma per confermare lo spegnimento del dispositivo.", timeStamp: 17000 },
                { button: mt32.confirm_poweroff, timeStamp: 19000, timeout: 2000, borderColor: "white" }
            ]).play();
        } else if (demo === Demo.VIEW_ALERTS) {
            player.load([
                { speak: "Se il display del dispositivo MT32 mostra un pulsante rosso di allerta, vuol dire che sono state riscontrate alcune anomalie nel funzionamento del dispositivo.", timeStamp: 1000 },
                { speak: "Clicca il pulsante rosso di allerta per verificare il tipo di anomalie riscontrate.", timeStamp: 13000 },
                { button: leftpanel.view_alerts, timeStamp: 15000, timeout: 2000, borderColor: "white" },
                { speak: "Comparirà una pagina con la lista delle anomalie.", timeStamp: 17000 },
                { speak: "Se le anomalie persistono, è necessario contattare l'assistenza.", timeStamp: 22000 }
            ]).play();
        } else if (demo === Demo.SEND_RESULTS) {
            player.load([
                { speak: "Istruzioni per caricare gli esami ECG tramite il dispositivo MT32.", timeStamp: 1000 },
                { speak: "Clicca Centrale.", timeStamp: 8500 },
                { button: home.server, timeStamp: 10000},
                { speak: "Poi, clicca Invio Risultati.", timeStamp: 11500 },
                { button: central.upload_results, timeStamp: 12500 },
                { speak: "Il dispositivo farà partire una procedura automatica che prevede il collegamento con la CT64, e l'invio di tutte le registrazioni.", timeStamp: 15000 },
                { input: "#send_1", value: "- Accensione modulo cellulare", timeStamp: 15500, lineFeed: true },
                { input: "#send_2", value: "- Attesa registrazione SIM", timeStamp: 16000, lineFeed: true },
                { input: "#send_3", value: "- Avvio connessione dati", timeStamp: 16500, lineFeed: true },
                { input: "#send_4", value: "- Invio esami", timeStamp: 17500, lineFeed: true },
                { input: "#send_5", value: "- 8828285a-c395-148d-e55c-abc170102009", timeStamp: 18000, lineFeed: true },
                { input: "#send_6", value: "- 8828285a-c395-148d-e55c-abc170102009.log", timeStamp: 18300, lineFeed: true },
                { input: "#send_7", value: "- 8828285a-c395-148d-e55c-abc170102009-00000732.hecg", timeStamp: 18600, lineFeed: true },
                { input: "#send_8", value: "- 8828285a-c395-148d-e55c-abc170102009-00000295.hecg", timeStamp: 18900, lineFeed: true },
                { input: "#send_9", value: "- 8828285a-c395-148d-e55c-abc170102009-00000732.hacc", timeStamp: 19200, lineFeed: true }
            ]).play();
        }




        function render(res) {
            hide_all_screens(res);
            render_ct64_widgets(res);
            if (res.ct64.mode === "WAITING_RESULTS" || res.mt32.mode === "POWERING_OFF") {
                start_tick(res);
            } else { stop_tick(res); }
            if (res.mt32.mode === "RECORDING" && res.mt32.mo === "HUB") {
                setTimeout(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("click_mt32_results_ready", onMessageReceived);
                }, 9000);
            }

            //-- mt32
            mt32.power_btn.render(res);
            mt32_settings.mt32_off.render(res);
            mt32_settings.mt32_on_battery.render(res);
            mt32_settings.mt32_charging.render(res);
            mt32_settings.mt32_fully_charged.render(res);
            mt32_settings.mt32_charging_error.render(res);

            if (res.mt32.mode === "OFF") {
                viz("#offScreen");
            } else {
                hide("#offScreen");
            }
            if (res.mt32.mode === "HOME") {
                viz("#homeScreen");
                home.new_exam.render();
                home.server.render();
                home.settings.render();
            } else if (res.mt32.mode === "EXAMS") {
                viz("#examsScreen");
                if (res.mt32.mo === "HUB") {
                    hide("#examsDISABLED");
                    viz("#examsHUB");
                    viz("#div_ecg12d");
                    hide("#examsHOLTER");
                    hide("#div_holter");
                    exams.ecg12d.render();
                    exams.test.render();
                } else if (res.mt32.mo === "HOLTER") {
                    hide("#examsDISABLED");
                    hide("#examsHUB");
                    hide("#div_ecg12d");
                    viz("#examsHOLTER");
                    viz("#div_holter");
                    exams.holter.render();
                    exams.test.render();
                } else {
                    // mode:none
                    viz("#examsDISABLED");
                    hide("#examsHUB");
                    hide("#div_ecg12d");
                    hide("#examsHOLTER");
                    hide("#div_holter");
                }
            } else if (res.mt32.mode === "CENTRAL") {
                viz("#centralScreen");
                central.download_updates.render();
                central.upload_results.render();
                central.terminate_operating_mode.render();
            } else if (res.mt32.mode === "SETTINGS") {
                viz("#settingsScreen");
                settings.connection_settings.render();
                settings.ecg_settings.render();
                settings.security_settings.render();
                settings.system_settings.render();
                settings.info.render();
            } else if (res.mt32.mode === "TEST") {
                viz("#testScreen");
                if (res.mt32.mo === "HUB") {
                    viz("#testHUB");
                    viz("#div_electrodes_status");
                    hide("#testHOLTER");
                    hide("#div_electrodes_status_holter");
                } else if (res.mt32.mo === "HOLTER") {
                    hide("#testHUB");
                    hide("#div_electrodes_status");
                    viz("#testHOLTER");
                    viz("#div_electrodes_status_holter");
                }
            } else if (res.mt32.mode === "CHECK_PATIENT") {
                viz("#checkPatientScreen");
                check.confirm.render();
            } else if (res.mt32.mode === "MONITORING" || res.mt32.mode === "RECORDING") {
                viz("#monitoringScreen");
                monitoring.quit.render();
                monitoring.rec.render();
                if (!streaming) {
                    update_tracing_display();
                    streaming = setInterval(update_tracing_display, duration);
                }
                if (res.mt32.mode === "RECORDING") {
                    monitoring.rec.select();
                } else {
                    monitoring.rec.deselect();
                }
            } else if (res.mt32.mode === "RESULTS") {
                viz("#resultsScreen");
                results.repeat.render();
                results.interpretation.render();
                results.physio.render();
            } else if (res.mt32.mode === "CONFIRM_REC") {
                viz("#confirmHolterScreen");
                exams.confirm.render();
            } else if (res.mt32.mode === "CONFIRM_POWER_OFF") {
                viz("#confirmPowerOffScreen");
                mt32.confirm_poweroff.render();
            } else if (res.mt32.mode === "POWERING_OFF") {
                viz("#poweringOffScreen");
            } else if (res.mt32.mode === "VIEW_ALERTS") {
                viz("#viewAlertsScreen");
            } else if (res.mt32.mode === "SENDING_RESULTS") {
                viz("#sendingResultsScreen");
            }

            // render mt32 LED
            render_mt32_led(res);
            // render mt32 mode
            if (res.demo === "TERMINATE_HUB_MODE" || res.demo === "TERMINATE_HOLTER_MODE") {
                render_mt32_mode(res, { flash: true });
            } else if (res.demo === "NEW_EXAM_HUB_MODE" ) {
                render_mt32_mode(res);
                setTimeout(function () {
                    render_mt32_mode(res, { flash: true });
                }, 6500);
            } else if (res.mt32.mode === "HOME" && res.demo === "NEW_EXAM_HUB_MODE") {
                render_mt32_mode(res);
                setTimeout(function () {
                    render_mt32_mode(res, { flash: true });
                }, 4000);
            } else if (res.mt32.mode === "HOME" && res.demo === "NEW_EXAM_HOLTER_MODE") {
                render_mt32_mode(res);
                setTimeout(function () {
                    render_mt32_mode(res, { flash: true });
                }, 6000);
            } else {
                render_mt32_mode(res);
            }
            // left panel
            if (res.mt32.mode === "HOME" || res.mt32.mode === "EXAMS" || res.mt32.mode === "CENTRAL"
                    || res.mt32.mode === "SETTINGS" || res.mt32.mode === "TEST" || res.mt32.mode === "CHECK_PATIENT"
                    || res.mt32.mode === "RESULTS") {
                d3.select("#leftPanel").style("display", "block");
                leftpanel.umts.render();
                leftpanel.wireless.render();
                leftpanel.bluetooth.render();
                leftpanel.battery.render();
                if (res.mt32.mode !== "HOME" && res.mt32.mode !== "RESULTS") {
                    leftpanel.back.render();
                }
            }
            if (res.mt32.alerts === "TRUE") {
                leftpanel.view_alerts.render();
            }

            //-- render ct64, based on selected demo mode
            // if (res.demo === "ACCESS_HOME_PAGE") {
                viz("#ct64AccessHomePage");
            // }
                if (res.ct64.mode === "google") {
                    viz("#ct64google", { fade: true });
                } else if (res.ct64.mode === "LOGIN") {
                    viz("#ct64LoginScreen", { fade: true });
                } else if (res.ct64.mode === "PATIENTS_SCREEN") {
                    viz("#ct64homePage", { fade: true });
                }
            // else if (res.demo === "HUB_KNOWN_PT" || res.demo === "HOLTER" || res.demo === "HUB_NEW_PT") {
                if (demo !== Demo.INTRO && demo !== Demo.ACCESS_HOME_PAGE) {
                    d3.select("#ct64_address").attr("value", "http://www.medicaltech.it");
                }

                if (res.ct64.mode === "PATIENTS_SCREEN") {
                    viz("#PATIENT_SCREENS");
                    viz("#patientsScreen");
                    if (res.ct64.known_patient === "TRUE") {
                        d3.selectAll(".ptData").style("display", "block");
                    } else { d3.selectAll(".ptData").style("display", "none"); }
                } else if (res.ct64.mode === "PATIENT_MANAGEMENT") {
                    viz("#PATIENT_SCREENS");
                    viz("#patientMenu");
                    if (res.ct64.patient_management_tab === "ARCHIVED_MEDICAL_REPORTS") {
                        viz("#archiveMedicalReportsScreen");
                    } else {
                        viz("#patientMGMScreen");
                    }
                } else if (res.ct64.mode === "MONITORING") {
                    stop_tick();
                    viz("#ct64-monitoringScreen");
                } else if (res.ct64.mode === "NEW_MONITORING_SESSION") {
                    viz("#newMonitoringSessionScreen", { flash: true });
                    if (res.ct64.known_patient === "TRUE") {
                       viz("#knownPT");
                    } else { viz("#newPT"); }
                    if (res.ct64.holter_mode === "TRUE") { viz("#holter_mode") }
                    if (res.ct64.hub_mode === "TRUE") { viz("#hub_mode") }
                    viz("#mariabianchi");
                    if (res.ct64.device_selected === "TRUE") {
                       ct64.select_device.select();
                    } else {
                       ct64.select_device.deselect();
                    }
                } else if (res.ct64.mode === "SHOW_MENU_VISITS") {
                    viz("#newMonSessionScreen");
                    viz("#visits_menu");
                    if (res.ct64.visit === "VISIT_ECG2D_NEW_PT") {
                        viz("#the_visit");
                    } else {
                        hide("#the_visit");
                    }
                } else if (res.ct64.mode === "NEW_MONITORING_SESSION_ECG2D_NEW_PT") {
                    viz("#newMonSessionScreen");
                    hide("#visits_menu");
                    if (res.ct64.visit === "VISIT_ECG2D_NEW_PT") {
                        viz("#the_visit");
                    } else {
                        hide("#the_visit");
                    }
                } else if (res.ct64.mode === "HOLTER_CONFIG") {
                    viz("#holterConfigScreen");
                } else if (res.ct64.mode === "SELECT_HOLTER_DEVICE") {
                    viz("#newMonitoringSessionScreen", { flash: true });
                    viz("#holterSession");
                    hide("#hubSession");
                    if (res.ct64.device_selected === "TRUE") {
                        ct64.select_device.select();
                    } else {
                        ct64.select_device.deselect();
                    }
                } else if (res.ct64.mode === "WAITING_RESULTS" || res.ct64.mode === "MT32_HOLTER_MODE" || res.ct64.mode === "MT32_HUB_MODE") {
                    viz("#waitingResultsScreen");
                    if (res.ct64.known_patient === "TRUE") {
                        viz("#knownPT");
                    } else { viz("#newPT"); }
                    if (res.ct64.holter_mode === "TRUE") { viz("#holter_mode") }
                    if (res.ct64.hub_mode === "TRUE") { viz("#hub_mode") }
                    viz("#mariabianchi");
                    if (res.ct64.mode === "WAITING_RESULTS") {
                        viz("#waiting_device");
                    } else {
                        viz("#deviceID");
                        hide("#waiting_device");
                    }
                } else if (res.ct64.mode === "ECG_ANALYSIS_RESULTS") {
                    viz("#MONITORING_SCREENS")
                    viz("#ecgAnalysisResultsScreen")
                    viz("#ecgMonitoringMenu");
                } else if (res.ct64.mode === "HOLTER_ANALYSIS_RESULTS") {
                    viz("#MONITORING_SCREENS")
                    viz("#holterAnalysisResultsScreen")
                } else if (res.ct64.mode === "ECG_RESULTS_INTERPRETATION") {
                    viz("#MONITORING_SCREENS")
                    viz("#ecgResultsInterpretationScreen")
                    viz("#ecgMonitoringMenu");
                } else if (res.ct64.mode === "ECG_PHYSIO") {
                    viz("#MONITORING_SCREENS")
                    viz("#ecgResultsPhysioScreen")
                    viz("#ecgMonitoringMenu");
                } else if (res.ct64.mode === "REPORT") {
                    viz("#MONITORING_SCREENS")
                    if (res.ct64.report_auth === "TRUE") {
                        if (res.ct64.ecg_report_saved === "TRUE") {
                            viz("#reportSavedScreen")
                        } else {
                            viz("#compileReportScreen");
                        }
                    } else {
                        viz("#requestReportScreen");
                    }
                    viz("#ecgMonitoringMenu");
                } else if (res.ct64.mode === "MEDICAL_REPORT") {
                    viz("#PATIENT_SCREENS");
                    viz("#medicalReportScreen");
                } else if (res.ct64.mode === "CREATE_NEW_PATIENT") {
                    viz("#PATIENT_SCREENS");
                    viz("#createNewPatientScreen");
                } else if (res.ct64.mode === "UPLOAD_EXAMS") {
                    viz("#PATIENT_SCREENS");
                    viz("#uploadExamsScreen");
                }
            // }
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
        var step = 40;
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
        function render_mt32_led(res) {
            viz("#mt32_off");
            if (res.mt32.mode !== "OFF") {
                if (res.mt32.battery_status === "ON_BATTERY") {
                    viz("#mt32_on");
                } else if (res.mt32.battery_status === "CHARGING") {
                    viz("#mt32_charging");
                } else if (res.mt32.battery_status === "FULLY_CHARGED") {
                    viz("#mt32_fully_charged");
                } else if (res.mt32.battery_status === "CHARGING_ERROR") {
                    viz("#mt32_charging_error");
                }
            }
        }
        function render_mt32_mode(res, opt) {
            opt = opt || {};
            if (res.mt32.mo === "HUB") {// && res.ct64.mode === "MT32_HUB_MODE") {
                setTimeout(function () {
                    hide("#mode_none");
                    viz("#mode_hub", { fade: true });
                    hide("#mode_holter");
                    if (opt.flash) {
                        d3.select("#mode_hub").classed("animated flash", true);
                    } else {
                        d3.select("#mode_hub").classed("animated flash", false);
                    }
                }, 250);
            } else if (res.mt32.mo === "HOLTER") {// && res.ct64.mode === "MT32_HOLTER_MODE") {
                setTimeout(function () {
                    hide("#mode_none");
                    hide("#mode_hub");
                    viz("#mode_holter", { fade: true });
                    if (opt.flash) {
                        d3.select("#mode_holter").classed("animated flash", true);
                    } else {
                        d3.select("#mode_holter").classed("animated flash", false);
                    }
                }, 250);
            } else {
                setTimeout(function () {
                    viz("#mode_none", { fade: true });
                    if (opt.flash) {
                        d3.select("#mode_none").classed("animated flash", true);
                    } else {
                        d3.select("#mode_none").classed("animated flash", false);
                    }
                    hide("#mode_hub");
                    hide("#mode_holter");
                }, 250);
            }
        }

        function fill(id, val, opt) {
            opt = opt || {};
            if (val && typeof val === "string") {
                var current_value = d3.select(id).attr("value");
                var i = 1;
                var elapse = opt.delay || 250;
                val.split("").forEach(function (c) {
                    setTimeout(function () {
                        d3.select(id).attr("value", current_value + c);
                        console.log(current_value);
                        current_value = d3.select(id).attr("value");
                    }, elapse);
                    elapse += (c === "@")? 400 : (Math.random() * (150 - 200) + 100);
                });
            }
        }

        var demoFolder = "MT32-evo";

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket()
                .startPVSProcess({name: "main.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
                    if (demo === Demo.ACCESS_HOME_PAGE) {
                        client.getWebSocket().sendGuiAction("init(ACCESS_HOME_PAGE);", onMessageReceived);
                    } else if (demo === Demo.HUB_NEW_PT) {
                        client.getWebSocket().sendGuiAction("init(HUB_NEW_PT);", onMessageReceived);
                    } else if (demo === Demo.HUB_KNOWN_PT) {
                        client.getWebSocket().sendGuiAction("init(HUB_KNOWN_PT);", onMessageReceived);
                    } else if (demo === Demo.HOLTER) {
                        client.getWebSocket().sendGuiAction("init(HOLTER);", onMessageReceived);
                    } else if (demo === Demo.TERMINATE_HUB_MODE) {
                        client.getWebSocket().sendGuiAction("init(TERMINATE_HUB_MODE);", onMessageReceived);
                    } else if (demo === Demo.TERMINATE_HOLTER_MODE) {
                        client.getWebSocket().sendGuiAction("init(TERMINATE_HOLTER_MODE);", onMessageReceived);
                    } else if (demo === Demo.NEW_EXAM_HUB_MODE) {
                        client.getWebSocket().sendGuiAction("init(NEW_EXAM_HUB_MODE);", onMessageReceived);
                    } else if (demo === Demo.NEW_EXAM_HOLTER_MODE) {
                        client.getWebSocket().sendGuiAction("init(NEW_EXAM_HOLTER_MODE);", onMessageReceived);
                    } else if (demo === Demo.TEST_ELECTRODES_HUB) {
                        client.getWebSocket().sendGuiAction("init(TEST_ELECTRODES_HUB);", onMessageReceived);
                    } else if (demo === Demo.VIEW_EXAMS_HUB) {
                        client.getWebSocket().sendGuiAction("init(VIEW_EXAMS_HUB);", onMessageReceived);
                    } else if (demo === Demo.VIEW_INTERPRETATION_HUB) {
                        client.getWebSocket().sendGuiAction("init(VIEW_INTERPRETATION_HUB);", onMessageReceived);
                    } else if (demo === Demo.VIEW_INTERPRETATION_HOLTER) {
                        client.getWebSocket().sendGuiAction("init(VIEW_INTERPRETATION_HOLTER);", onMessageReceived);
                    } else if (demo === Demo.REQUEST_REPORT) {
                        client.getWebSocket().sendGuiAction("init(REQUEST_REPORT);", onMessageReceived);
                    } else if (demo === Demo.WRITE_REPORT) {
                        client.getWebSocket().sendGuiAction("init(WRITE_REPORT);", onMessageReceived);
                    } else if (demo === Demo.VIEW_MEDICAL_REPORT) {
                        client.getWebSocket().sendGuiAction("init(VIEW_MEDICAL_REPORT);", onMessageReceived);
                    } else if (demo === Demo.VIEW_ARCHIVED_MEDICAL_REPORTS) {
                        client.getWebSocket().sendGuiAction("init(VIEW_ARCHIVED_MEDICAL_REPORTS);", onMessageReceived);
                    } else if (demo === Demo.MT32_LED) {
                        client.getWebSocket().sendGuiAction("init(MT32_LED);", onMessageReceived);
                        setTimeout(function () {
                            d3.select("#device").style("transition-duration", "1000ms").style("transform", "translate(1200px,-1300px)scale(2,2)");
                        }, 9000);
                    } else if (demo === Demo.CREATE_NEW_PATIENT) {
                        client.getWebSocket().sendGuiAction("init(CREATE_NEW_PATIENT);", onMessageReceived);
                    } else if (demo === Demo.TRANSFER_DATA_MICROSD) {
                        client.getWebSocket().sendGuiAction("init(TRANSFER_DATA_MICROSD);", onMessageReceived);

                        // slide everything to the right
                        setTimeout(function () {
                            d3.select("#content").style("transition-duration", "1000ms").style("margin-left", "200px");
                        }, 5000);

                        // rotate MT32 on the side
                        setTimeout(function () {
                            d3.select("#MT32-screens").style("transition-duration", "1600ms")
                                                        .style("transform", "rotateY(22deg)translateZ(-423px)translateY(212px)translateX(140px)skewY(10deg)");
                            d3.select("#MT32-sdcard").style("transition-duration", "1600ms")
                                                        .style("transform", "rotateY(22deg)translateZ(-36px)translateY(15px)translateX(-2px)skewY(10deg)");
                            d3.selectAll(".MT32-case-leftside")
                                .style("transition-duration", "1600ms")
                                .style("transform", "rotateY(0deg)translateZ(0px)translateY(8px)translateX(0px)skewY(0deg)scaleY(1.01)");
                            d3.select("#MT32-sdcard-cover")
                                .style("transition-duration", "1600ms")
                                .style("transform", "rotateY(0deg)translateZ(0px)translateY(0px)translateX(0px)skewY(0deg)scaleY(1.04)");

                        }, 6000);

                        // remove SD panel
                        setTimeout(function () {
                            d3.select("#MT32-sdcard-cover").transition().duration(1000).style("opacity", "0");
                        }, 7000);

                        // extract SD card
                        setTimeout(function () {
                            d3.select("#MT32-sdcard").style("display", "block");
                            setTimeout(function () {
                                d3.select("#MT32-sdcard").style("display", "block");
                                d3.select("#MT32-sdcard").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(22deg)translateZ(-170px)translateY(170px)translateX(-200px)skewY(10deg)");
                            }, 1000);
                        }, 7500);

                        // show SD card reader
                        setTimeout(function () {
                            d3.selectAll(".card-reader").style("display", "block").style("opacity", 0).transition().duration(1000).style("opacity", 0.8);
                            d3.select("#MT32-sdcard").style("z-index", 2).style("transition-duration", "2600ms")
                                    .style("transform", "translateX(980px)translateY(360px)rotateX(17deg)rotateZ(-108deg)skewX(-13deg)scale(0.3,0.5)");
                        }, 10000);

                        // insert card in the reader
                        setTimeout(function () {
                            d3.select("#MT32-sdcard").style("z-index", 2).style("transition-duration", "1600ms")
                                    .style("transform", "translateX(1132px) translateY(421px) rotateX(26deg) rotateZ(-104deg) skewX(-13deg) translateZ(-300px) scale(0.2, 0.5)");
                        }, 12000);
                    } else if (demo === Demo.INTRO) {
                        client.getWebSocket().sendGuiAction("init(INTRO);", onMessageReceived);
                        d3.select("#device").style("transform", "translate(-400px,200px)scale(0.5)");
                        d3.select("#CT64-screens").style("opacity", 0);

                        // present MT32
                        setTimeout(function () {
                            d3.select("#device").style("transition-duration", "1600ms")
                                .style("transform", "translate(0px,0px)scale(1)");
                        }, 14000);

                        // ct64
                        setTimeout(function () {
                            d3.select("#CT64-screens").transition().duration(1600).style("opacity", 1);
                        }, 18000);

                        // fad out mt32
                        setTimeout(function () {
                            d3.select("#device").style("transition-duration", "1600ms")
                                .style("transform", "translate(-400px,200px)scale(0.5)");
                        }, 46500);

                    } else if (demo === Demo.POWER_ON_MT32) {
                        client.getWebSocket().sendGuiAction("init(POWER_ON_MT32);", onMessageReceived);
                        d3.selectAll(".MT32-case-righside").style("transition-duration", "1600ms")
                                .style("transform", "rotateY(22deg)translateZ(-10px)translateY(-12px)skewY(10deg)scaleY(0.9)");
                        d3.select("#MT32-right").style("display", "block");

                        setTimeout(function () {

                            d3.select("#MT32-screens").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(-22deg)translateZ(-183px)translateY(-112px)translateX(140px)skewY(-10deg)scaleY(1.4)");
                            d3.selectAll(".MT32-case-righside").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(0deg)translateZ(0px)translateY(0px)skewY(0deg)scaleY(1)");
                        }, 4000);

                    } else if (demo === Demo.POWER_OFF_MT32) {
                        client.getWebSocket().sendGuiAction("init(POWER_OFF_MT32);", onMessageReceived);
                        d3.selectAll(".MT32-case-righside").style("transition-duration", "1600ms")
                                .style("transform", "rotateY(22deg)translateZ(-10px)translateY(-12px)skewY(10deg)scaleY(0.9)");
                        d3.select("#MT32-right").style("display", "block");

                        setTimeout(function () {
                            d3.select("#MT32-screens").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(-22deg)translateZ(-183px)translateY(-112px)translateX(140px)skewY(-10deg)scaleY(1.4)");
                            d3.selectAll(".MT32-case-righside").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(0deg)translateZ(0px)translateY(0px)skewY(0deg)scaleY(1)");
                        }, 4000);

                        // confirmation screen
                        setTimeout(function () {
                            d3.select("#MT32-screens").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(0deg)translateZ(0px)translateY(0px)translateX(0px)skewY(0deg)scaleY(1)");
                            d3.selectAll(".MT32-case-righside").style("transition-duration", "1600ms")
                                    .style("transform", "rotateY(22deg)translateZ(-10px)translateY(-12px)skewY(10deg)scaleY(0.9)");
                        }, 12000);

                    } else if (demo === Demo.VIEW_ALERTS) {
                        client.getWebSocket().sendGuiAction("init(VIEW_ALERTS);", onMessageReceived);
                        d3.select("#home_battery_empty").style("display", "block");
                    } else if (demo === Demo.SEND_RESULTS) {
                        client.getWebSocket().sendGuiAction("init(SEND_RESULTS);", onMessageReceived);
                    }
                    else {
                        client.getWebSocket().sendGuiAction("init(STD);", onMessageReceived);
                    }
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
