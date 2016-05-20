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
        stateParser: './util/PVSioStateParser',
        NCDevice: 'plugins/networkController/NCDevice'
    }
});

require([
        "widgets/Button",
        "widgets/SingleDisplay",
        "widgets/DoubleDisplay",
        "widgets/TripleDisplay",
        "widgets/LED",
        "widgets/TracingsDisplay",
        "widgets/med/PatientMonitorDisplay",
        "plugins/graphbuilder/GraphBuilder",
        "stateParser",
        "PVSioWebClient",
        "NCDevice"],
    function (Button,
              SingleDisplay,
              DoubleDisplay,
              TripleDisplay,
              LED,
              TracingsDisplay,
              PatientMonitorDisplay,
              GraphBuilder,
              stateParser,
              PVSioWebClient,
              NCDevice) {
        "use strict";


        var deviceID = "Radical";
        var deviceType = "SpO2 Monitor";


        function parseNCUpdate(event) {

            var from = event.from;

            if (from === "Radical") {

                var res = stateParser.parse(event.message);
                client.getWebSocket()
                    .sendGuiAction("update_spo2(" + res.spo2 + ")" +
                    "(" + client.getWebSocket().lastState() + ");",
                    onMessageReceived);
            }
        }

        function parseNCControl(event) {
//            var from = event.from;
        }

        function errorMessage(event) {
            console.log("!!! " + event.message);
        }

        function notifyMessage(event) {
            console.log(">>> " + event.message);
        }

        function onConnect(event) {
            console.log(">>> CONNECTED");
        }

        function onDisconnect(event) {
            console.log(">>> DISCONNECTED");
        }


        var url = window.location.href.split(":").slice(0,2).join(":") + ":8080/NetworkController/devices";
        url = url.replace("http://", "ws://");
        var ncDevice = new NCDevice({id: deviceID, type: deviceType}, { url: url });

        ncDevice.addListener("update", parseNCUpdate);
        ncDevice.addListener("control", parseNCControl);
        ncDevice.addListener("error", errorMessage);
        ncDevice.addListener("notify", notifyMessage);
        ncDevice.addListener("connected", onConnect);
        ncDevice.addListener("disconnected", onDisconnect);


        var d3 = require("d3/d3");
        var serverLogs = [], maxLogSize = 40;

        var client = PVSioWebClient.getInstance();
        //create a collapsible panel using the pvsiowebclient instance
        var imageHolder = client.createCollapsiblePanel({
            parent: "#content",
            headerText: "Simulation of Masimo Radical 7 PulseOximeter",
            showContent: true,
            isDemo: true
        }).style("position", "relative").style("width", "1200px");
        //insert the html into the panel (note that this could have used templates or whatever)
        imageHolder.html('<img src="radical-7.png" usemap="#prototypeMap"/>').attr("id", "prototype");

        // append control panel
        // FIXME: create a library with APIs to create SAPERE control panels -- i.e., something similar to client.createCollapsiblePanel
        var content = imageHolder.append("div").style("width", "600px").style("float", "right");
        content.append("div").attr("style", "margin-bottom: 10px;").append("input").attr("type", "button")
            .attr("id", "btnShowPanel").attr("value", "Show Advanced Controls");
        var controlPanel = content.append("div").attr("id", "controlPanel").style("display", "none");
        // sapere
        var sapereControl = controlPanel.append("div").attr("class", "sapere");
        sapereControl.append("div").attr("class", "sapere_control_panel")
            .append("input").attr("type", "button").attr("class", "btnAddDevice")
            .attr("value", "Add New Radical7");
        sapereControl.append("div").attr("id", "sapere_response_log").attr("class", "console_log");
        sapereControl.append("input").attr("type", "text").attr("name", "address")
            .attr("placeholder", "Please type a message")
            .attr("id", "updateMessage");
        sapereControl.append("input").attr("type", "button").attr("class", "btnUpdateDevice")
            .attr("value", "Send");
        // pvsio
        controlPanel.append("div").attr("class", "pvsio")
            .text("Device State").append("div").attr("class", "dbg").attr("id", "dbg")
            .style("position", "absolute").style("top", "20px")
            .attr("style", "height: 100%; width: 100%; height: 600px;");

        d3.select("#btnShowPanel").on("click", function toggleDebug() {
            if (document.getElementById("controlPanel").style.display === "none") {
                document.getElementById("controlPanel").style.display = "block";
                document.getElementById("btnShowPanel").value = "Hide Advanced Controls";
            } else {
                document.getElementById("controlPanel").style.display = "none";
                document.getElementById("btnShowPanel").value = "Show Advanced Controls";
            }
        });


        // append a div that will contain the canvas elements
        var tick = null;
        var start_tick = null, stop_tick = null;


        // append displays
        var radical = {};
        radical.spo2_display = new PatientMonitorDisplay("spo2_display",
            {top: 54, left: 150, height: 34, width: 160},
            {parent: "prototype", font: "Times", label: "%SpO2"});
        radical.rra_display = new PatientMonitorDisplay("rra_display",
            {top: 100, left: 150, height: 34, width: 160},
            {parent: "prototype", font: "Times", label: "RRa", fontColor: "aqua"});

        // utility function
        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            return (v <= 0) ? "--" : ((v < 10) ? v.toFixed(1).toString() : v.toFixed(0).toString());
        }

        // spo2
        function render_spo2(res) {
            if (res.isOn === "TRUE") {
                radical.spo2_display.set_alarm({min: parseFloat(res.spo2_min), max: parseFloat(res.spo2_max)});
                radical.spo2_display.set_range({min: 0, max: 100});
                if (res.spo2_fail === "FALSE") {
                    if (res.spo2_alarm === "off") {
                        radical.spo2_display.render(evaluate(res.spo2));
                    } else {
                        radical.spo2_display.render(evaluate(res.spo2), {fontColor: "red"});
                    }
                } else {
                    radical.spo2_display.fail("FAIL");
                }
                start_tick();
            } else {
                radical.spo2_display.hide();
                stop_tick();
            }
        }

        // RRa
        function render_rra(res) {
            if (res.isOn === "TRUE") {
                radical.rra_display.set_alarm({min: parseFloat(res.rra_min), max: parseFloat(res.rra_max)});
                radical.rra_display.set_range({min: 0, max: 70});
                if (res.rra_fail === "FALSE") {
                    if (res.rra_alarm === "off") {
                        radical.rra_display.render(evaluate(res.rra));
                    } else {
                        radical.rra_display.render(evaluate(res.rra), {fontColor: "red"});
                    }
                } else {
                    radical.rra_display.fail("FAIL");
                }
                start_tick();
            } else {
                radical.rra_display.hide();
                stop_tick();
            }
        }

        // alarms
        function render_alarms(res) {
            if (res.isOn === "TRUE") {
                if (res.spo2_alarm === "off") {
                    radical.spo2_display.alarm("off");
                } else if (res.spo2_alarm === "alarm") {
                    radical.spo2_display.alarm("glyphicon-bell");
                } else if (res.spo2_alarm === "mute") {
                    radical.spo2_display.alarm("glyphicon-mute");
                }
                if (res.rra_alarm === "off") {
                    radical.rra_display.alarm("off");
                } else if (res.rra_alarm === "alarm") {
                    radical.rra_display.alarm("glyphicon-bell");
                } else if (res.rra_alarm === "mute") {
                    radical.rra_display.alarm("glyphicon-mute");
                }
            } else {
                radical.spo2_display.hide();
                radical.rra_display.hide();
            }
        }

        /**
         function to handle when an output has been received from the server after sending a guiAction
         if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
         */
        function onMessageReceived(err, event) {
            function prettyprintState(str) {
                var state = stateParser.parse(str);
                state.spo2_label = state.spo2_label.replace(/"/g, "");
                return JSON.stringify(state, null, " ");
            }

            if (!err) {
                client.getWebSocket().lastState(event.data);
                var dbg = prettyprintState(event.data.toString());

                // logging
                var date = new Date();
                serverLogs.push({data: dbg, date: date, id: event.id, type: "dbg"});
                if (serverLogs.length > maxLogSize) {
                    serverLogs = serverLogs.slice(-maxLogSize);
                }
                var logLines = d3.select(".dbg").selectAll("textarea").data(serverLogs, function (d, i) {
                    return d.id;
                });
                logLines.enter()
                    .insert("textarea", "textarea").html(function (d) {
                        return d.date.toString() + "\n" + d.data;
                    }).style("width", "100%")
                    .attr("readonly", true)
                    .attr("rows", function (d) {
                        return d.data.split("\n").length + 1;
                    }).attr("class", function (d) {
                        return d.type;
                    });
                logLines.exit().remove();

                ncDevice.sendDataUpdate(event.data.toString());

                // rendering
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    res = stateParser.parse(event.data.toString());
                    if (res) {
                        render_spo2(res);
                        render_rra(res);
                        render_alarms(res);
                    }
                }
            } else {
                console.log(err);
            }
        }


        //--- tick function -------------------
        start_tick = function () {
            if (!tick) {
                tick = setInterval(function () {
                    client.getWebSocket()
                        .sendGuiAction("tick(" + client.getWebSocket().lastState() + ");", onMessageReceived);
                }, 2000);
            }
        };

        stop_tick = function () {
            if (tick) {
                clearInterval(tick);
                tick = null;
            }
        };


        d3.select(".btn_on").on("click", function () {
            stop_tick();
            client.getWebSocket()
                .sendGuiAction("click_btn_on(" + client.getWebSocket().lastState() + ");", onMessageReceived);
            start_tick();
        });

        d3.select("#submit_spo2_sensor_data").on("click", function () {
            var data = d3.select("#spo2_sensor_data").node().value;
            if (data) {
                data = (isNaN(parseFloat(data))) ? -1 : parseFloat(data);
                stop_tick();
                client.getWebSocket()
                    .sendGuiAction("spo2_sensor_data(" + data + ")(" + client.getWebSocket().lastState() + ");", onMessageReceived);
                start_tick();
            }
        });

        d3.select("#submit_rra_sensor_data").on("click", function () {
            var data = d3.select("#rra_sensor_data").node().value;
            if (data) {
                data = (isNaN(parseFloat(data))) ? -1 : parseFloat(data);
                stop_tick();
                client.getWebSocket()
                    .sendGuiAction("rra_sensor_data(" + data + ")(" + client.getWebSocket().lastState() + ");", onMessageReceived);
                start_tick();
            }
        });


        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "Radical7/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(0);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                ncDevice.start().then(
                    function (res) {
                        ncDevice.connect();
                    }).catch(function(err){
                        console.log(err);
                    });
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
