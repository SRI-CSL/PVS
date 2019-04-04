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

        "widgets/car/Gauge",

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

        Gauge,

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

        var sys = {};
        sys.pressureGauge = new Gauge("pressure_gauge", {
            width: 200,
            height: 200,
            top: 78,
            left: 668
        },{
            label: "bar",
            max: 340,
            min: 0,
            majorTicks: 6,
            redZones: [ { from: 270, to: 340 } ],
            rotation: -45,
            style: "classic",
            position: "absolute"
        });
        sys.flowGauge = new Gauge("flow_gauge", {
            width: 200,
            height: 200,
            top: 78,
            left: 967
        },{
            label: "L/min",
            max: 20,
            min: 0,
            majorTicks: 6,
            redZones: [ { from: 16, to: 20 } ],
            rotation: -45,
            style: "classic",
            position: "absolute"
        });
        sys.pressureDisplay = new BasicDisplay("pressure_display", {
            top: 3,
            left: 658,
            width: 200,
            height: 76
        }, {
            fontsize: 24,
            backgroundColor: "red",
            parent: "content"
        });
        sys.flowDisplay = new BasicDisplay("flow_display", {
            top: 3,
            left: 955,
            width: 200,
            height: 76
        }, {
            fontsize: 24,
            backgroundColor: "green",
            parent: "content"
        });
        sys.slider = new Slider("set_engine_power", {
            top: 280,
            left: 115,
            width: 40,
            height: 200
        }, {
            max: 340,
            min: 0,
            orientation: "horizontal",
            tooltipPosition: "inner",
            handleWidth: 40,
            handleHeight: 60,
            backgroundColor: "transparent",
            track: {
                color: "transparent",
            },
            init: 300 // initial value selected by the slider
        });
        sys.tempDisplay = new BasicDisplay("temperature_display", {
            top: 874,
            left: 898,
            width: 200,
            height: 76
        }, {
            fontsize: 24,
            backgroundColor: "green",
            parent: "content"
        });
        sys.togglePower = new ToggleButton("toggle_power", {
            top: 245,
            left: 260,
            height: 32,
            width: 104
        });

        function render(res) {
            sys.pressureGauge.render(evaluate(res.pressure));
            sys.pressureDisplay.render(evaluate(res.pressure) + " bar", {
                backgroundColor: (evaluate(res.pressure) > 272) ? "red" : "green"
            });
            // you can tweak the visual appearance of the widgets directly here, e.g., to change the border style
            d3.select("#pressure_display").select(".pressure_display_canvas").style("border", "solid 4px black");

            sys.flowGauge.render(evaluate(res.flow));
            sys.flowDisplay.render(evaluate(res.flow) + " L/min");
            sys.tempDisplay.render(evaluate(res.temperature) + " °C");
            sys.slider.render();
            sys.togglePower.render();
            if (res.isOn === "TRUE") {
                sys.togglePower.on();
                d3.select("#pump").classed("pulse_bak infinite", true);
            } else {
                sys.togglePower.off();
                d3.select("#pump").classed("pulse_bak infinite", false);
            }
        }

        var demoFolder = "taste";
        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket()
                .startPVSProcess({name: "main.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(300);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                // start the simulation
                start_tick();
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();
    });
