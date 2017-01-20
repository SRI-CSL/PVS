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
    "widgets/NumericDisplay",
    "widgets/BasicDisplay",
    "plugins/graphbuilder/GraphBuilder",
    "stateParser",
    "PVSioWebClient"
],function (
        NumericDisplay,
        BasicDisplay,
        GraphBuilder,
        stateParser,
        PVSioWebClient) {
    "use strict";
    var d3 = require("d3/d3");

    var w = 228, h = 64;
	var client = PVSioWebClient.getInstance();
    //create a collapsible panel using the pvsiowebclient instance
    var imageHolder = client.createCollapsiblePanel({
        parent: "#content",
        headerText: "Simulation of the data entry system of a medical infusion pump. Please use the navigation keys of the medical device to interact with the device (greyed out keys are disabled keys).",
        showContent: true,
        isDemo: true
    }).style("position", "relative");
    //insert the html into the panel (note that this could have used templates or whatever)
    imageHolder.html('<img src="image.png" usemap="#prototypeMap"/>');
	//append display
    var disp = new NumericDisplay("display", {
        left: 90, top: 272, width: 220, height: 56
    }, {
        displayKey: "d",
        cursorName: "c",
        fontsize: 32
    });
	//register the graph builder plugin -- so we can visualise the interaction
    var gb = GraphBuilder.getInstance();
//    var gb = client.registerPlugin(GraphBuilder);

    /**
        parse the raw state string from pvsio process into key value pairs
    */
    function parseState(str) {
        var args = str.split(","), res = {};
        args.forEach(function (d) {
            var t = d.split(":=");
            res[t[0].replace("(#", "").trim()] = t[1].trim();
        });
        return res;
    }

    /**
        using this (instead of encouraging the use of eval()) to evaluate the rational numbers send from the pvsio process.
    */
    function evaluate(str) {
        if (str.indexOf("/") < 0) { return str; }
        var args = str.split("/");
        return +args[0] / +args[1];
    }

    /**
        function to handle when an output has been received from the server after sending a guiAction
        if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
    */
    function onMessageReceived(err, event) {
        if (!err) {
            var res = event.data.toString();
            if (res.indexOf("(#") === 0) {
                res = stateParser.parse(event.data.toString());
                if (res) {
                    disp.render(res);
                }
            }
        } else {
             console.log(err);
        }
	}

    //bind ui elements with functions for sending messages to the server
	d3.select('.btnUp').on("click", function () {
		client.getWebSocket().sendGuiAction("click_up(" + client.getWebSocket().lastState() + ");", onMessageReceived);
	});

	d3.select(".btnDn").on("click", function () {
		client.getWebSocket().sendGuiAction("click_dn(" + client.getWebSocket().lastState() + ");", onMessageReceived);
	});

    d3.select(".btnLeft").on("click", function () {
		client.getWebSocket().sendGuiAction("click_lf(" + client.getWebSocket().lastState() + ");", onMessageReceived);
    });

    d3.select(".btnRight").on("click", function () {
		client.getWebSocket().sendGuiAction("click_rt(" + client.getWebSocket().lastState() + ");", onMessageReceived);
    });

    d3.select(".btnClear").on("click", function () {
		client.getWebSocket().sendGuiAction("click_clear(" + client.getWebSocket().lastState() + ");", onMessageReceived);
    });

    //register event listener for websocket connection from the client
	client.addListener('WebSocketConnectionOpened', function (e) {
		console.log("web socket connected");
		//start pvs process
		client.getWebSocket().startPVSProcess({name: "bbraun_space.pvs", demoName: "BBraun/pvs"}, function (err, event) {
			client.getWebSocket().sendGuiAction("click_clear(" + client.getWebSocket().lastState() + ");", onMessageReceived);
		});
	}).addListener("WebSocketConnectionClosed", function (e) {
		console.log("web socket closed");
	}).addListener("processExited", function (e) {
		var msg = "Warning!!!\r\nServer process exited. See console for details.";
		console.log(msg);
	});

	client.connectToServer();

});
