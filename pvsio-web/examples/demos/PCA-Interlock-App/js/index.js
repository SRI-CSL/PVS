/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, browser:true*/
/*global require*/

require.config({
    baseUrl: '../../client/app',
    paths: {
        d3: '../lib/d3',
        stateParser: './util/PVSioStateParser',
        NCDevice: 'plugins/networkController/NCDevice',
        NCMonitorCore: 'plugins/networkController/NCMonitorCore'
    }
});
/**
 * Loading the module PVSioWebClient.
 */
require([
    'PVSioWebClient',
    'stateParser',
    'NCDevice',
    'NCMonitorCore',
    "widgets/med/PatientMonitorDisplay",
    "widgets/med/PumpMonitorDisplay",
    "widgets/TripleDisplay",
    "widgets/SingleDisplay",
    "widgets/ButtonActionsQueue",
    "widgets/TouchScreenButton"
], function (PVSioWebClient, stateParser, NCDevice, NCMonitorCore, PatientMonitorDisplay, PumpMonitorDisplay, TripleDisplay, SingleDisplay, ButtonActionsQueue, TouchScreenButton) {

    var deviceID = "Supervisor";
    var deviceType = "Supervisor";
    var d3 = require("d3/d3");
    //var deviceDescription = "Supervisor";

    var client;
    var tick = null;

    //-- messages from the PVSio-web Network Controller
    function parseNCUpdate(event) {
        if (event.from === "Alaris") {
            ButtonActionsQueue.getInstance().queueGUIAction("update_pump(" + event.message + ")", onMessageReceived);
        } else if (event.from === "Radical") {
            ButtonActionsQueue.getInstance().queueGUIAction("update_monitor(" + event.message + ")", onMessageReceived);
        }
    }

    /**
     * NCMonitorCore preferences
     */
    var error_mode = false;
    var debugging_mode_backend = false;
    var debugging_mode_frontend = false;
    var extended_mode = false;

    function errorMessage(event) {
        if (error_mode) {
            console.log("!!! " + event.message);
        }
    }

    function notifyMessage(event) {
        if (debugging_mode_frontend) {
            console.log(">>> " + event.message);
        }
    }

    function debugMessage(event) {
        if (debugging_mode_backend) {
            console.log(">!> " + event.message);
        }
    }

    /**
     * Callback DeviceID connected
     * @param event
     */
    function onConnect(event) {
        ButtonActionsQueue.getInstance()
            .queueGUIAction("on_connect_supervisor", onMessageReceived);
    }

    /**
     * Callback DeviceID connected
     * @param event
     */
    function onDisconnect(event) {
        ButtonActionsQueue.getInstance()
            .queueGUIAction("on_disconnect_supervisor", onMessageReceived);
    }


    /**
     * Parsing of different actions coming from the Network Controller Monitor
     * @param event
     */
    function parseNCActions(event) {
        var kind = event.kind;

        switch (kind) {
            case "connected":
                var deviceConnected = event.data.deviceID;
                switch (deviceConnected) {
                    case "Alaris":
                        ButtonActionsQueue.getInstance()
                            .queueGUIAction("on_connect_pump", onMessageReceived);
                        break;
                    case "Radical":
                        ButtonActionsQueue.getInstance()
                            .queueGUIAction("on_connect_monitor", onMessageReceived);
                        break;
                    default:
                        break;
                }
                break;

            case "disconnected":
                var deviceDisconnected = event.data.deviceID;
                switch (deviceDisconnected) {
                    case "Alaris":
                        ButtonActionsQueue.getInstance()
                            .queueGUIAction("on_disconnect_pump", onMessageReceived);
                        break;
                    case "Radical":
                        ButtonActionsQueue.getInstance()
                            .queueGUIAction("on_disconnect_monitor", onMessageReceived);
                        break;
                    default:
                        break;
                }
                break;

            default:
                break;
        }
    }

    $('#toggle_front_debugging').click(function () {
        debugging_mode_frontend = !debugging_mode_frontend;
    });

    $('#toggle_back_debugging').click(function () {
        if (debugging_mode_backend) {
            debugging_mode_backend = false;
            ncMonitorCore.deactivateDebug();
        }
        else {
            debugging_mode_backend = true;
            ncMonitorCore.activateDebug();
        }
    });

    $('#toggle_errors').click(function () {
        error_mode = !error_mode;
    });

    var urlMonitor = window.location.origin.split(":").slice(0, 2).join(":") + ":8080/NetworkController/monitor";
    urlMonitor = urlMonitor.replace("http://", "ws://");

    var ncMonitorCore = new NCMonitorCore({
        extended: extended_mode,
        debugging: debugging_mode_backend,
        url: urlMonitor
    });

    ncMonitorCore.addListener("error", errorMessage);
    ncMonitorCore.addListener("notify", notifyMessage);
    ncMonitorCore.addListener("debug_backend", debugMessage);
    ncMonitorCore.addListener("action", parseNCActions);

    var url = window.location.origin.split(":").slice(0, 2).join(":") + ":8080/NetworkController/devices";
    url = url.replace("http://", "ws://");
    var ncDevice = new NCDevice({id: deviceID, type: deviceType}, {url: url});
    ncDevice.addListener("update", parseNCUpdate);
    ncDevice.addListener("error", errorMessage);
    ncDevice.addListener("notify", notifyMessage);
    ncDevice.addListener("connected", onConnect);
    ncDevice.addListener("disconnected", onDisconnect);


    //-- UI of the ICE Supervisor
    function start_tick() {
        if (!tick) {
            tick = setInterval(function () {
                ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
            }, 4000);
        }
    }

//    function stop_tick() {
//        if (tick) {
//            clearInterval(tick);
//            tick = null;
//        }
//    }

    var verboseLogging = false;

    function logOnDiv(msg, logger) {
        if (verboseLogging) {
            d3.select("#monitor").style("display", "block");
            var p = $("<p>", {class: "console_element"});
            p.html(msg);
            $("#" + logger)
                .append(p)
                .animate({scrollTop: $("#" + logger)[0].scrollHeight}, 500);
        } else {
            d3.select("#monitor").style("display", "none");
        }
    }

    /**
     function to handle when an output has been received from the server after sending a guiAction
     if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
     */
    function onMessageReceived(err, event) {
        d3.select(".demo-splash").style("display", "none");
        if (!err && event && event.data) {
            logOnDiv(event.data.toString(), "monitor");
            var res = event.data.toString();
            if (res && res.indexOf("(#") === 0) { // checking that the state is well-formed
                res = stateParser.parse(res);
                if (res.supervisor_connected.trim() === "TRUE" &&
                    res.monitor_connected.trim() === "TRUE" &&
                    res.pump_connected.trim() === "TRUE") {
                    // safety interlock
                    if (res.monitor.isOn.trim() === "TRUE" && res.pump.isOn.trim() === "TRUE") {
                        if (res.pumpcmd.trim() === "pause") {
                            console.log("pause pump");
                            ncDevice.sendControlData("Alaris", "click_btn_pause");
                        }
                    }
                }
                // rendering
                render_patient_monitor(res);
                render_pump_monitor(res);
                start_tick();
            }
        } else {
            console.log(err);
        }
    }


    /*
     * Get client instance and the websocket it uses to communicate with the server
     */
    client = PVSioWebClient.getInstance();
    var imageHolder = client.createCollapsiblePanel({
        parent: "#content",
        headerText: "Simulation of ICE Supervisor with PCA Interlock App",
        showContent: true,
        isDemo: true
    }).style("position", "relative").style("width", "1200px");
    //insert the html into the panel (note that this could have used templates or whatever)
    imageHolder.html('<img src="ICE-Supervisor.png" usemap="#prototypeMap"/>').attr("id", "prototype");

    var app = {monitor: {spo2_display: null, rra_display: null}, pump: {rate: null}};
    app.monitor.spo2_display = new PatientMonitorDisplay("spo2_display",
        {top: 250, left: 140, height: 40, width: 180},
        {parent: "prototype", font: "Times", label: "%SpO2"});
    app.monitor.rra_display = new PatientMonitorDisplay("rra_display",
        {top: 300, left: 140, height: 40, width: 180},
        {parent: "prototype", font: "Times", label: "RRa", fontColor: "aqua"});
    app.monitor.searching = new SingleDisplay("searching_patient_monitor",
        {top: 280, left: 140, height: 24, width: 190},
        {parent: "prototype"});

    app.alarm = new SingleDisplay("alarm",
        {top: 34, left: 133, height: 16, width: 16},
        {parent: "prototype", blinking: true, fontColor: "#FF0033"}); // bright red
    app.pump.topline = new SingleDisplay("topline",
        {top: 70, left: 160, height: 12, width: 130},
        {parent: "prototype"});
    app.pump.topline_back = new SingleDisplay("topline_back",
        {top: 70, left: 140, height: 16, width: 16},
        {parent: "prototype", touchscreen: {callback: onMessageReceived}});
    app.pump.rate = new TripleDisplay("rate",
        {top: 100, left: 140, height: 24, width: 190},
        {
            left_display: {height: 14, width: 56, align: "left", left: +2, top: +8},
            center_display: {width: 80, align: "right"},
            right_display: {height: 14, width: 44, align: "right", top: +10},
            backgroundColor: "black",
            touchscreen: {callback: onMessageReceived},
            parent: "prototype"
        });
    app.pump.vtbi = new TripleDisplay("vtbi",
        {top: 135, left: 140, height: 24, width: 190},
        {
            left_display: {height: 14, width: 56, align: "left", left: +2, top: +8},
            center_display: {width: 80, align: "right"},
            right_display: {height: 14, width: 44, align: "right", top: +10},
            backgroundColor: "black",
            touchscreen: {callback: onMessageReceived},
            parent: "prototype"
        });
    app.pump.volume = new TripleDisplay("volume",
        {top: 170, left: 140, height: 24, width: 190},
        {
            left_display: {height: 14, width: 56, align: "left", left: +2, top: +8},
            center_display: {width: 80, align: "right"},
            right_display: {height: 14, width: 44, align: "right", top: +10},
            backgroundColor: "black",
            touchscreen: {callback: onMessageReceived},
            parent: "prototype"
        });
    app.pump.time = new SingleDisplay("time",
        {top: 205, left: 140, height: 20, width: 190},
        {
            backgroundColor: "black",
            parent: "prototype"
        });
    app.pump.tracings = new PumpMonitorDisplay("tracings",
        {top: 112, left: 140, height: 34, width: 180},
        {parent: "prototype", range: {max: 1200, min: 0}});
    app.pump.searching = new SingleDisplay("searching_pump",
        {top: 112, left: 140, height: 24, width: 190},
        {parent: "prototype"});


    // utility function
    function evaluate(str) {
        var v = +str;
        if (str.indexOf("/") >= 0) {
            var args = str.split("/");
            v = +args[0] / +args[1];
        }
        return (v <= 0) ? "--" : ((v < 10) ? v.toFixed(1).toString() : v.toFixed(0).toString());
    }

    // monitor
    function render_patient_monitor(res) {
        // spo2
        function render_spo2(monitor) {
            app.monitor.spo2_display.set_alarm({min: parseFloat(monitor.spo2_min), max: parseFloat(monitor.spo2_max)});
            app.monitor.spo2_display.set_range({min: 0, max: 100});
            if (monitor.spo2_fail === "FALSE") {
                if (monitor.spo2_alarm === "off") {
                    app.monitor.spo2_display.render(evaluate(monitor.spo2));
                } else {
                    app.monitor.spo2_display.render(evaluate(monitor.spo2), {fontColor: "red"});
                }
            } else {
                app.monitor.spo2_display.fail("FAIL");
            }
        }

        // RRa
        function render_rra(monitor) {
            app.monitor.rra_display.set_alarm({min: parseFloat(monitor.rra_min), max: parseFloat(monitor.rra_max)});
            app.monitor.rra_display.set_range({min: 0, max: 70});
            if (monitor.rra_fail === "FALSE") {
                if (monitor.rra_alarm === "off") {
                    app.monitor.rra_display.render(evaluate(monitor.rra));
                } else {
                    app.monitor.rra_display.render(evaluate(monitor.rra), {fontColor: "red"});
                }
            } else {
                app.monitor.rra_display.fail("FAIL");
            }
        }

        // alarms
        function render_alarms(monitor) {
            switch (monitor.spo2_alarm.trim()) {
                case "off"   :
                    app.monitor.spo2_display.alarm("off");
                    break;
                case "alarm" :
                    app.monitor.spo2_display.alarm("glyphicon-bell");
                    app.alarm.renderGlyphicon("glyphicon-bell");
                    break;
                case "mute"  :
                    app.monitor.spo2_display.alarm("glyphicon-mute");
                    app.alarm.renderGlyphicon("glyphicon-mute");
                    break;
            }
            switch (monitor.rra_alarm) {
                case "off"   :
                    app.monitor.rra_display.alarm("off");
                    break;
                case "alarm" :
                    app.monitor.rra_display.alarm("glyphicon-bell");
                    app.alarm.renderGlyphicon("glyphicon-bell");
                    break;
                case "mute"  :
                    app.monitor.rra_display.alarm("glyphicon-mute");
                    app.alarm.renderGlyphicon("glyphicon-mute");
                    break;
            }
            if ((monitor.rra_alarm.trim() === "alarm" || monitor.rra_alarm.trim() === "mute") &&
                (monitor.spo2_alarm.trim() === "alarm" || monitor.spo2_alarm.trim() === "mute")) {
                app.alarm.renderGlyphicon("glyphicon-bell");
            } else {
                app.alarm.hide();
            }
        }

        if (res.monitor_connected === "TRUE") {
            if (res.monitor.isOn.trim() === "TRUE") {
                render_spo2(res.monitor);
                render_rra(res.monitor);
                render_alarms(res.monitor);
                app.monitor.searching.hide();
            } else {
                app.alarm.hide();
                app.monitor.spo2_display.hide();
                app.monitor.rra_display.hide();
                app.monitor.spo2_display.hide();
                app.monitor.rra_display.hide();
                app.monitor.searching.renderMultiline(["ICE-Compatible monitor detected:", res.monitor.id]);
            }
        } else {
            app.alarm.hide();
            app.monitor.spo2_display.hide();
            app.monitor.rra_display.hide();
            app.monitor.spo2_display.hide();
            app.monitor.rra_display.hide();
            app.monitor.searching.renderMultiline(["ICE-compatible monitors", "Searching..."], {blinking: true});
        }
    }


    // pump
    function render_pump_monitor(res) {
        // topline
        function render_topline(res) {
            function topline2string(msg) {
                msg = msg.toUpperCase();
                switch (msg) {
                    case "DISPVTBI" :
                        return "VTBI";
                    case "DISPKVO"  :
                        return "KVO";
                    case "HOLDING"  :
                        return "ON HOLD";
                    case "SETRATE"  :
                        return "ON HOLD - SET RATE";
                    case "DISPBLANK":
                        return "";
                    default :
                        return msg;
                }
                return msg;
            }

            function topline2options(msg) {
                msg = msg.toUpperCase();
                if (msg === "HOLDING" || msg === "SETRATE" || msg === "ATTENTION") {
                    return {blinking: true};
                }
                return {};
            }

            app.pump.topline.render(topline2string(res.topline.trim()), topline2options(res.topline.trim()));
        }

        function render_pump_data(res) {
            function evaluate(str) {
                var v = +str;
                if (str.indexOf("/") >= 0) {
                    var args = str.split("/");
                    v = +args[0] / +args[1];
                }
                return parseInt(v * 100) / 100; // number truncated at the 2nd fractional digit
            }

            function evaluateTime(str) {
                var x = evaluate(str);
                var hour = parseInt(x);
                x = (x - hour) * 60;
                var min = parseInt(x);
                x = (x - min) * 60;
                var sec = parseInt(x);
                return hour + "h " + min + "m " + sec + "s";
            }

            render_topline(res);
            app.pump.topline_back.hide();
            app.pump.rate.renderLabel("RATE");
            app.pump.rate.renderValue(evaluate(res.rate));
            app.pump.rate.renderUnits("mL/h");
            app.pump.vtbi.renderLabel("VTBI");
            app.pump.vtbi.renderValue(evaluate(res.vtbi));
            app.pump.vtbi.renderUnits("mL");
            app.pump.volume.renderLabel("VOLUME");
            app.pump.volume.renderValue(evaluate(res.volume));
            app.pump.volume.renderUnits("mL");
            app.pump.time.render(evaluateTime(res.time));
            app.pump.tracings.hide();
        }

        function render_pump_tracings(param, res) {
            function evaluate(str) {
                var v = +str;
                if (str.indexOf("/") >= 0) {
                    var args = str.split("/");
                    v = +args[0] / +args[1];
                }
                return v;
            }

            param = param.replace("show_", "");
            app.pump.rate.hide();
            app.pump.vtbi.hide();
            app.pump.volume.hide();
            app.pump.time.hide();
            render_topline(res);
            app.pump.topline_back.renderGlyphicon("glyphicon-list");
            var units = (param === "rate") ? "mL/h" : (param === "vtbi" || param === "volume") ? "mL" : "??";
            var range = (param === "rate") ? {max: 1200} : (param === "vtbi" || param === "volume") ? {max: 3000} : {max: 100};
            app.pump.tracings.render(evaluate(res[param]), {
                label: param.toString().toUpperCase(),
                units: units,
                range: range
            });
        }

        if (res.pump_connected.trim() === "TRUE") {
            if (res.pump.isOn.trim() === "TRUE") {
                if (res.mode.trim() === "monitoring") {
                    render_pump_data(res.pump);
                } else if (res.mode.indexOf("show_") === 0) {
                    render_pump_tracings(res.mode, res.pump);
                }
                app.pump.searching.hide();
            } else {
                app.pump.searching.renderMultiline(["ICE-Compatible pump detected:", res.pump.id]);
                app.pump.rate.hide();
                app.pump.vtbi.hide();
                app.pump.volume.hide();
                app.pump.time.hide();
                app.pump.topline.hide();
                app.pump.tracings.hide();
                app.pump.topline_back.hide();
            }
        } else {
            app.pump.rate.hide();
            app.pump.vtbi.hide();
            app.pump.volume.hide();
            app.pump.time.hide();
            app.pump.topline.hide();
            app.pump.tracings.hide();
            app.pump.topline_back.hide();
            app.pump.searching.renderMultiline(["ICE-compatible pumps", "Searching..."], {blinking: true});
        }
    }

    /*
     * Register event listener for websocket connection to the server.
     */
    client.addListener('WebSocketConnectionOpened', function () {
        console.log('web socket connected');
        logOnDiv('PVSio Web Socket connected', 'monitor');
        /*
         * Start the PVS Process for the pacemaker
         */
        client.getWebSocket().startPVSProcess({
            name: 'main.pvs',
            demoName: 'PCA-Interlock-App/pvs'
        }, function (err) {
            if (!err) {
                logOnDiv('PVS Process started', 'monitor');
                //-- start ICE Network Controller (NCEE) & connect ICE supervisor to it
                ncMonitorCore.start().then(function (res) {
                    ncDevice.start(ncDevice).then(function (res) {
                        ncDevice.connect();
                        start_tick();
                        d3.select(".content").style("display", "block");
                    }).catch(function (err) {
                        d3.select(".error").style("display", "block");
                    });
                }).catch(function (err) {
                    console.log(err);
                    d3.select(".error_monitor").style("display", "block");

                });
            } else {
                console.log(err);
            }
        });
    }).addListener('WebSocketConnectionClosed', function () {
        logOnDiv('PVS Process closed', 'monitor');
        console.log('web socket closed');
    }).addListener('processExited', function () {
        var msg = 'Warning!!!\r\nServer process exited. See console for details.';
        console.log(msg);
    });
    /*
     * Initiate connection to the server
     */
    logOnDiv('Connecting to the PVSio server...', 'monitor');
    client.connectToServer();


});