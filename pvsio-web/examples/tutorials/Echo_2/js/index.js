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

require([ "widgets/core/BasicDisplayEVO" ], function (BasicDisplay) {
    "use strict";

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
    const client = new WebSocket("ws://localhost:8082");
    client.onopen = function () {
        var msg = { type: "ping" };
        client.send(JSON.stringify(msg));
        sys.disp.render("Waiting echo...");
        console.log("Connection opened!");
    };
    client.onclose = function (evt) {
        console.log("Connection closed :((");
    };
    client.onerror = function (evt) {
        console.error(evt);
    };
    client.onmessage = function (evt) {
        sys.disp.render("Echo message received!", { backgroundColor: "olive" });
        console.log("Echo message received!");
    };
});
