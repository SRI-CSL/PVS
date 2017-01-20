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
        "widgets/LED",
        "plugins/graphbuilder/GraphBuilder",
        "stateParser",
        "PVSioWebClient"
    ], function (
        Button,
        TouchscreenButton,
        TouchscreenDisplay,
        BasicDisplay,
        NumericDisplay,
        LED,
        GraphBuilder,
        stateParser,
        PVSioWebClient
    ) {
        "use strict";
        var d3 = require("d3/d3");
        var serverLogs = [], maxLogSize = 40;

        var client = PVSioWebClient.getInstance();
        //create a collapsible panel using the pvsiowebclient instance
        var imageHolder = client.createCollapsiblePanel({
            parent: "#content",
            headerText: "Simulation of the FCU Software",
            showContent: true,
            isDemo: true,
            width: 1230
        }).style("position", "relative").style("width", "1230px");
        //insert the html into the panel (note that this could have used templates or whatever)
        imageHolder.html('<img src="FCU-Software-ext.png" usemap="#prototypeMap"/>')
                    .attr("id", "prototype").attr("class", "fcusoftware").style("float", "left")
                    .append("map").attr("id", "prototypeMap").attr("name", "prototypeMap");
        //d3.select(".collapsible-panel-parent").append("div").attr("id", "kccu").attr("class", "kccu").html('<img src="KCCU.png" usemap="#prototypeMap"/>').style("float", "left");

        /**
         function to handle when an output has been received from the server after sending a guiAction
         if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
         */
        function onMessageReceived(err, event) {
            function prettyprintState(str) {
                var state = stateParser.parse(str);
                state.data_entry.display = state.data_entry.display.replace(/"/g, "");
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

                // rendering
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    res = stateParser.parse(event.data.toString());
                    if (res) {
                        render(res);
                    }
                }
            } else {
                console.log(err);
            }
        }

        var tick = null;
        var start_tick = null, stop_tick = null;

        var fcu = {};
        // append FCU widgets
        fcu.cursorOverlay = new BasicDisplay("cursorOverlay",
                            {top: 0, left: 0, height: 800, width: 624 },
                            {
                                backgroundColor: "transparent",
                                //visibleWhen: "current_state != EDIT_PRESSURE",
                                parent: "prototype",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.display_val = new BasicDisplay("display_val",
                            { top: 333, left: 16, height: 28, width: 100 },
                            {
                                displayKey: "data_entry.display",
                                visibleWhen: "current_state = STD",
                                fontsize: 16,
                                fontColor: "white",
                                backgroundColor: "dimgray",
                                parent: "prototype",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.display_units = new BasicDisplay("FCU_display_units",
                            { top: 333, left: 112, height: 28, width: 36 },
                            {
                              displayKey: "data_entry.units",
                              visibleWhen: "current_state = STD",
                              fontsize: 14,
                              fontColor: "white",
                              backgroundColor: "dimgray",
                              parent: "prototype",
                              cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.touchscreen_display = new TouchscreenDisplay("FCU_display",
                            { top: 333, left: 16, height: 28, width: 100 },
                            {
                              displayKey: "data_entry.display",
                              visibleWhen: "current_state = QNH",
                              functionText: "editbox_pressure",
                              fontColor: "cyan",
                              backgroundColor: "black",
                              callback: onMessageReceived,
                              fontsize: 16,
                              displayMode: "standard",
                              parent: "prototype",
                              cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.notouchs_display_units = new BasicDisplay("FCU_notouch_display_units",
                            { top: 333, left: 112, height: 28, width: 36 },
                            {
                              displayKey: "data_entry.units",
                              visibleWhen: "current_state = QNH",
                              fontColor: "steelblue",
                              backgroundColor: "black",
                              fontsize: 14,
                              parent: "prototype",
                              cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.data_entry_display = new BasicDisplay("FCU_data_entry_display",
                            { top: 333, left: 16, height: 28, width: 134 },
                            {
                              displayKey: "data_entry.display",
                              visibleWhen: "current_state = EDIT_PRESSURE",
                              backgroundColor: "black",
                              fontColor: "cyan",
                              fontsize: 16,
                              parent: "prototype",
                              cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.btn_toHPA = new TouchscreenButton("tohPa",
                            { top: 380, left: 12, height: 36, width: 140 },
                            {
                                softLabel: "INHG -> HPA",
                                functionText: "hPa",
                                visibleWhen: "data_entry.units = inHg",
                                backgroundColor: "dimgray",
                                fontsize: 16,
                                callback: onMessageReceived,
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto",
                                parent: "prototype"
                            });
        fcu.btn_toINHG = new TouchscreenButton("toinHg",
                            { top: 380, left: 12, height: 36, width: 140 },
                            {
                                softLabel: "HPA -> INHG",
                                functionText: "inHg",
                                visibleWhen: "data_entry.units = hPa",
                                backgroundColor: "dimgray",
                                fontsize: 16,
                                callback: onMessageReceived,
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto",
                                parent: "prototype"
                            });
        fcu.btn_CLEAR = new TouchscreenButton("CLR_INFO",
                            { top: 752, left: 4, height: 42, width: 82 },
                            {
                                softLabel: "CLR INFO",
                                functionText: "CLR",
                                backgroundColor: "dimgray",
                                fontsize: 16,
                                callback: onMessageReceived,
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto",
                                parent: "prototype"
                            });
        fcu.STD_LED = new LED("STD_LED", {top: 243, left: 10, height: 32, width: 32},
                            {
                                radius: 9,
                                visibleWhen: "current_state = STD",
                                parent: "prototype",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.btn_STD_RADIO = new TouchscreenButton("STD_RADIO",
                            { top: 243, left: 10, height: 32, width: 72 },
                            {
                                callback: onMessageReceived,
                                softLabel: "",
                                backgroundColor: "transparent",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto",
                                parent: "prototype"
                            });
        fcu.QNH_LED = new LED("QNH_LED", {top: 275, left: 10, height: 32, width: 32},
                            {
                                radius: 9,
                                visibleWhen: "current_state != STD",
                                parent: "prototype",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto"
                            });
        fcu.btn_QNH_RADIO = new TouchscreenButton("QNH_RADIO",
                            { top: 275, left: 10, height: 32, width: 72 },
                            {
                                callback: onMessageReceived,
                                softLabel: "",
                                backgroundColor: "transparent",
                                cursor: "url('./css/pilot_cursor.cur') 32 32, auto",
                                parent: "prototype"
                            });

        fcu.btn_key1 = new Button("digit_1", {left: 1032, top: 294}, {callback: onMessageReceived, keyCode:49, keyName:"key 1"});
        fcu.btn_key2 = new Button("digit_2", {left: 1080, top: 294}, {callback: onMessageReceived, keyCode:50, keyName:"key 2"});
        fcu.btn_key3 = new Button("digit_3", {left: 1126, top: 294}, {callback: onMessageReceived, keyCode:51, keyName:"key 3"});
        fcu.btn_key4 = new Button("digit_4", {left: 1032, top: 340}, {callback: onMessageReceived, keyCode:52, keyName:"key 4"});
        fcu.btn_key5 = new Button("digit_5", {left: 1080, top: 340}, {callback: onMessageReceived, keyCode:53, keyName:"key 5"});
        fcu.btn_key6 = new Button("digit_6", {left: 1126, top: 340}, {callback: onMessageReceived, keyCode:54, keyName:"key 6"});
        fcu.btn_key7 = new Button("digit_7", {left: 1032, top: 383}, {callback: onMessageReceived, keyCode:55, keyName:"key 7"});
        fcu.btn_key8 = new Button("digit_8", {left: 1080, top: 383}, {callback: onMessageReceived, keyCode:56, keyName:"key 8"});
        fcu.btn_key9 = new Button("digit_9", {left: 1126, top: 383}, {callback: onMessageReceived, keyCode:57, keyName:"key 9"});
        fcu.btn_point = new Button("point",  {left: 1032, top: 428}, {callback: onMessageReceived, keyCode:190, keyName:"."});
        fcu.btn_key0 = new Button("digit_0", {left: 1080, top: 428}, {callback: onMessageReceived, keyCode:48, keyName:"key 0"});
        fcu.btn_CLR = new Button("CLR", {left: 672, top: 82}, {callback: onMessageReceived, keyCode:46, keyName:"delete"});
        fcu.btn_ESC = new Button("ESC", {left: 680, top: 34}, {callback: onMessageReceived, keyCode:27, keyName:"esc"});
        fcu.btn_OK = new Button("OK", {left: 1110, top: 234}, {callback: onMessageReceived, keyCode:13, keyName:"enter"});

        // render function
        function render(res) {
            fcu.touchscreen_display.render(res);
            fcu.display_units.render(res);
            fcu.data_entry_display.render(res);
            fcu.display_val.render(res);
            fcu.notouchs_display_units.render(res);
            fcu.btn_toHPA.render(res);
            fcu.btn_toINHG.render(res);
            fcu.btn_CLEAR.render(res);
            fcu.btn_QNH_RADIO.render(res);
            fcu.btn_STD_RADIO.render(res);
            fcu.STD_LED.render(res);
            fcu.QNH_LED.render(res);
            fcu.cursorOverlay.render(res);
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


        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "emucharts_FCU_th.pvs", demoName: "fcusoftware/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(0);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                speak("ready");
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
