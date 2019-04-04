/**
 * @description Echo tutorial: demonstrates the use of button widgets. The display of the device prototytpe toggles on/off by pressing the device button.
 * @author Paolo Masci
 * @date 09/01/2018
 */
/* jslint esnext:true */
require.config({
    baseUrl: "../../client/app",
    paths: {
        "d3": "../lib/d3",
        "text": "../lib/text"
    }
});

require([
    "widgets/core/BasicDisplayEVO",
    "PVSioWebClient"
], function (BasicDisplay, PVSioWebClient) {
    "use strict";

    // callback function
    function onMessageReceived(err, res) {
        if (err) {
            console.error(err);
        } else {
            sys.disp.render("Echo message received!", { backgroundColor: "olive" });
            console.log("Echo message received!");
        }
    }

    // widgets
    var sys = {};
    sys.disp = new BasicDisplay("disp", {
        top: 62, left: 44, height: 410, width: 294
    }, {
        fontSize: 12,
        backgroundColor: "steelblue",
        opacity: 0.8,
        parent: "screen"
    });

    // web socket client
    const client = PVSioWebClient.getInstance();
    client.serverUrl("ws://localhost");
    client.port("8082");
    client.addListener("WebSocketConnectionOpened", function (res) {
        init();
        console.log("Connection opened!");
    }).addListener("WebSocketConnectionClosed", function (evt) {
        console.log("Connection closed :((");
    }).addListener("processExited", function (evt) {
        console.error(evt);
    });
    client.connectToServer();

    // init function
    function init () {
        client.getWebSocket().sendGuiAction("<ping>", onMessageReceived);
        sys.disp.render("Waiting echo...");
        console.log("Waiting echo...");
    }
});
