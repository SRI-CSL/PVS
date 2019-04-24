/**
 * SVG Speedometer widget demo
 *
 * @author Henrique Pacheco
 * @date 02/07/17
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
        "widgets/car/GaugeSport",
        "widgets/car/Gearbox",
        "widgets/car/Clock",
        "widgets/car/SVGWidget",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"
    ], function (
        Button,
        GaugeSport,
        Gearbox,
        Clock,
        SVGWidget,
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
                }
            } else {
                console.log(err);
            }
        }

        var widgets = {

            // Speedometer idgets
            speedometer1: new GaugeSport(
                'svg-speedometer',
                { top: 90, left: 30 },
                { style: 'speedometer', parent: 'speedometer-container' }
            ),
            speedometer2: new GaugeSport(
                'svg-speedometer2',
                { top: 90, left: 330 },
                { style: 'speedometer2', parent: 'speedometer-container' }
            ),
            speedometer3: new GaugeSport(
                'svg-speedometer3',
                { top: 90, left: 630 },
                { style: 'speedometer3', parent: 'speedometer-container' }
            ),
            speedometer4: new GaugeSport(
                'svg-speedometer4',
                { top: 90, left: 930 },
                { style: 'speedometer4', parent: 'speedometer-container' }
            ),
            speedometer5: new GaugeSport(
                'svg-speedometer5',
                { top: 390, left: 30 },
                { style: 'speedometer5', parent: 'speedometer-container' }
            ),
            speedometer6: new GaugeSport(
                'svg-speedometer6',
                { top: 390, left: 330 },
                { style: 'speedometer6', parent: 'speedometer-container' }
            ),
            speedometer7: new GaugeSport(
                'svg-speedometer7',
                { top: 390, left: 630 },
                { style: 'speedometer7', parent: 'speedometer-container', pointer: { top: 107, left: 112, width: 36 }}
            ),
            speedometer8: new GaugeSport(
                'svg-speedometer8',
                { top: 390, left: 930 },
                { style: 'speedometer8', parent: 'speedometer-container' }
            ),


            // Tachometer widgets
            tachometer1: new GaugeSport(
                'svg-tachometer1',
                { top: 90, left: 30 },
                { style: 'tachometer', parent: 'tachometer-container' }
            ),
            tachometer2: new GaugeSport(
                'svg-tachometer2',
                { top: 90, left: 330 },
                { style: 'tachometer2', parent: 'tachometer-container' }
            ),
            tachometer3: new GaugeSport(
                'svg-tachometer3',
                { top: 90, left: 630 },
                { style: 'tachometer3', parent: 'tachometer-container' }
            ),
            tachometer4: new GaugeSport(
                'svg-tachometer4',
                { top: 90, left: 930 },
                { style: 'tachometer4', parent: 'tachometer-container' }
            ),


            // Clock widgets
            clock1: new Clock(
                'svg-clock1',
                { top: 90, left: 30 },
                { style: 'clock', parent: 'clock-container' }
            ),
            clock2: new Clock(
                'svg-clock2',
                { top: 90, left: 330 },
                { style: 'clock2', parent: 'clock-container' }
            ),
            clock3: new Clock(
                'svg-clock3',
                { top: 90, left: 630 },
                { style: 'clock3', parent: 'clock-container' }
            ),
            clock4: new Clock(
                'svg-clock4',
                { top: 90, left: 930 },
                { style: 'clock4', parent: 'clock-container' }
            ),


            // Fuel widgets
            fuel1: new GaugeSport(
                'svg-fuel1',
                { top: 30, left: 915 },
                { style: 'fuel', parent: 'fuel-temp-pressure-container' }
            ),
            fuel2: new GaugeSport(
                'svg-fuel2',
                { top: 30, left: 615 },
                { style: 'fuel2', parent: 'fuel-temp-pressure-container' }
            ),
            fuel3: new GaugeSport(
                'svg-fuel3',
                { top: 30, left: 15 },
                { style: 'fuel3', parent: 'fuel-temp-pressure-container' }
            ),
            fuel4: new GaugeSport(
                'svg-fuel4',
                { top: 30, left: 315 },
                { style: 'fuel4', parent: 'fuel-temp-pressure-container' }
            ),


            // Pressure widgets
            pressure1: new GaugeSport(
                'svg-pressure1',
                { top: 330, left: 15 },
                { style: 'pressure', parent: 'fuel-temp-pressure-container' }
            ),


            // Temperature widgets
            temperature1: new GaugeSport(
                'svg-thermometer1',
                { top: 330, left: 315 },
                { style: 'thermometer', parent: 'fuel-temp-pressure-container' }
            ),


            // Compass widgets
            compass1: new GaugeSport(
                'svg-compass1',
                { top: 30, left: 15 },
                { style: 'compass', parent: 'compass-container' }
            ),
            compass2: new GaugeSport(
                'svg-compass2',
                { top: 30, left: 315 },
                { style: 'compass2', parent: 'compass-container' }
            ),


            // Gearbox widgets
            gearbox1: new Gearbox(
                'svg-gearbox',
                { top: 15, left: 15, width: 250, height: 250 },
                { parent: 'gearbox-container', style: 'auto' }
            ),
            gearbox2: new Gearbox(
                'svg-gearbox2',
                { top: 15, left: 315, width: 250, height: 250 },
                { parent: 'gearbox-container', style: 'manual' }
            ),
            gearbox3: new Gearbox(
                'svg-gearbox3',
                { top: 15, left: 615, width: 250, height: 250 },
                { parent: 'gearbox-container', style: 'manual2' }
            ),
            gearbox4: new Gearbox(
                'svg-gearbox4',
                { top: 15, left: 915, width: 250, height: 250 },
                { parent: 'gearbox-container', style: 'manual3' }
            ),


            // Car controls
            accelerate: new Button("accelerate", { width: 0, height: 0 }, {
                callback: onMessageReceived,
                evts: ['press/release'],
                keyCode: 38 // key up
            }),
            brake: new Button("brake", { width: 0, height: 0 }, {
                callback: onMessageReceived,
                evts: ['press/release'],
                keyCode: 40 // key down
            })
        };

        // Render widgets
        function render(res) {

            // TODO check this out -- some of the gauges use mph, so the conversion is
            // being made here, but it should be provided from the pvs file
            var kph = evaluate(res.speed.val);
            var mph = kph / 1.609344;

            // Spedometer render methods
            widgets.speedometer1.render(mph);
            widgets.speedometer2.render(mph);
            widgets.speedometer3.render(kph);
            widgets.speedometer4.render(mph);
            widgets.speedometer5.render(kph);
            widgets.speedometer6.render(mph);
            widgets.speedometer7.render(mph);
            widgets.speedometer8.render(kph);

            // Tachometer render methods
            widgets.tachometer1.render(evaluate(res.rpm));
            widgets.tachometer2.render(evaluate(res.rpm));
            widgets.tachometer3.render(evaluate(res.rpm));
            widgets.tachometer4.render(evaluate(res.rpm));

            // Clock render methods
            widgets.clock1.render();
            widgets.clock2.render();
            widgets.clock3.render();
            widgets.clock4.render();
        }

        // Fuel event handlers
        $('.fuel-change').click(function () {
            var val = $(this).data('fuel');
            widgets.fuel1.render(val);
            widgets.fuel2.render({ fuel: val });
            widgets.fuel3.render(val);
            widgets.fuel4.render(val);
        });
        document.getElementById("perc-input").addEventListener("change", function(e) {
            var val = document.getElementById("perc-input").value;
            widgets.fuel1.render(val);
            widgets.fuel2.render({ fuel: val });
            widgets.fuel3.render(val);
            widgets.fuel4.render(val);
        });


        // Temperature event handlers
        $('.temp-change').click(function () {
            var val = $(this).data('temp');
            widgets.fuel2.render({ temperature: val });
            widgets.temperature1.render(val);
        });
        document.getElementById("temp-input").addEventListener("change", function(e) {
            var val = document.getElementById("temp-input").value;
            widgets.fuel2.render({ temperature: val });
            widgets.temperature1.render(val);
        });

        // Pressure event handlers
        $('.pressure-change').click(function () {
            var val = $(this).data('pressure');
            widgets.pressure1.render(val);
        });
        document.getElementById("pressure-input").addEventListener("change", function(e) {
            widgets.pressure1.render(document.getElementById("pressure-input").value);
        });


        // Compass event handlers
        $('.compass-change').click(function () {
            var angle = $(this).data('angle');
            widgets.compass1.render(angle);
            widgets.compass2.render(angle);
        });


        // Gearbox event handlers
        $(".change-gear-button").click(function() {
            var gear = $(this).data('gear');
            widgets.gearbox1.render(gear);
            widgets.gearbox2.render(gear);
            widgets.gearbox3.render(gear);
            widgets.gearbox4.render(gear);
        });


        document.getElementById("color-change").addEventListener("change", function(e) {
            // Get selected color
            var val = document.getElementById("color-change").value;

            // Call the color change method for every widget in the page
            $.each(widgets, function(key, value) {
                if(SVGWidget.prototype.isPrototypeOf(value)) {
                    value.changeColor(val);
                }
            });
        });


        var demoFolder = "gauge-widgets-library";
        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            //start pvs process
            client.getWebSocket()
                .startPVSProcess({name: "main.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init(0);", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select("#content").style("display", "block");
                // start the simulation
                start_tick();
            });
        }).addListener("WebSocketConnectionClosed", function (e) {

        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
        });

        client.connectToServer();
    });
