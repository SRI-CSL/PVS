/**
 *
 * @author Patrick Oladimeji
 * @date 11/25/13 23:10:09 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, document, PVSioWebClient */
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
    "widgets/core/NumericDisplayEVO",
    "widgets/core/ButtonEVO",
    "stateParser",
    "PVSioWebClient"
],function (
        NumericDisplay,
        Button,
        stateParser,
        PVSioWebClient) {
    "use strict";
    var device = {};

    device.disp = new NumericDisplay("display", {
        left: 88, top: 234, width: 220, height: 56
    }, {
        displayKey: "d",
        cursorName: "c",
        fontsize: 28,
        maxIntegerDigits: 6,
        maxFractionalDigits: 2
    });
    device.up = new Button("up", {
        top: 240, left: 382, width: 38, height: 38
    }, {
        keyCode: 38, //arrow up
        callback: onMessageReceived
    });
    device.dn = new Button("dn", {
        top: 288, left: 382, width: 38, height: 38
    }, {
        keyCode: 40, //arrow down
        callback: onMessageReceived
    });
    device.lf = new Button("lf", {
        top: 264, left: 344, width: 38, height: 38
    }, {
        keyCode: 37, //arrow left
        callback: onMessageReceived
    });
    device.rt = new Button("rt", {
        top: 264, left: 420, width: 38, height: 38
    }, {
        keyCode: 39, //arrow right
        callback: onMessageReceived
    });
    device.clear = new Button("clear", {
        top: 240, left: 463, width: 38, height: 38
    }, {
        keyCode: 46, //clear button
        callback: onMessageReceived
    });

    //--------------- the following functions take care of rendering state updates sent by the PVSio-web back-end
    function render(res) {
        for (var key in device) {
            device[key].render(res);
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
        }
    }

    //--------------- the following code takes care of connecting the JavaScript front-end to the PVSio-web back-end
    var demoFolder = "BBraun/";
    var client = PVSioWebClient.getInstance();
    //register event listener for websocket connection from the client
    client.addListener('WebSocketConnectionOpened', function (e) {
        console.log("web socket connected");
        //start pvs process
        client.getWebSocket()
        .startPVSProcess({name: "bbraun_space.pvs", demoName: demoFolder + "pvs"}, function (err, event) {
            // first thing, initialise the pvs model
            client.getWebSocket().sendGuiAction("init;", onMessageReceived);
        });
    }).addListener("WebSocketConnectionClosed", function (e) {
        console.log("web socket closed");
    }).addListener("processExited", function (e) {
        var msg = "Warning!!!\r\nServer process exited. See console for details.";
        console.log(msg);
    });
    client.connectToServer();

});
