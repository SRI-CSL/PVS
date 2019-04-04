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
        "widgets/SliderWidget",
        "widgets/SliderWidgetWithButtons",

        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"
    ], function (
        Slider,
        SliderWithButtons,

        ButtonActionsQueue,
        stateParser,
        PVSioWebClient
    ) {
        "use strict";
        var client = PVSioWebClient.getInstance();

        var sys = {};
        sys.SliderWithButtonsH = new SliderWithButtons("SliderWithButtonsH", {
            top: 0,
            left: 0,
            width: 55, // note that the default orientation of the slider is vertical, this is why I have swapped width and height
            height: 433
        }, {
            min: 0,
            max: 100,
            style: "modern",
            orientation: "horizontal",
            backgroundColor: "#121e2b",
            labelFormat: function (value) {
                return value + "%";
            },
            buttons: {
                backgroundColor: "#121e2b",
                borderColor: "#8aa1b6",
                fontColor: "#cad7e4",
                opacity: 1
            },
            handle: {
                backgroundColor: "#121e2b",
                borderColor: "#9bb5cc",
                fontColor: "#cad7e4",
                opacity: 0.9,
                zIndex: 2
            },
            innerImage: {
                type: "triangle"
            },
            parent: "volumeControlWindow",
            init: 340 // initial value selected by the slider
        });


        //--------------- the following code takes care of rendering state updates sent by the PVSio-web back-end
        function render(res) {
            for (var key in sys) {
                sys[key].render(res);
            }
        }
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

        //--------------- the following code takes care of connecting the JavaScript front-end to the PVSio-web back-end
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({
                    name: "main.pvs", demoName: "slider/pvs"
                }, function (err, event) {
                    client.getWebSocket().sendGuiAction("init(300);", onMessageReceived);
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
