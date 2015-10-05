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
        "cm": "../lib/cm"
    }
});

require(["widgets/CursoredDisplay", "plugins/graphbuilder/GraphBuilder", "PVSioWebClient"], function (CursoredDisplay, GraphBuilder, PVSioWebClient) {
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
    //append a div that will contain the canvas element
    var content = imageHolder.append("div").style("position", "absolute").style("top", "232px").style("left", "84px")
        .style("height", h + "px").style("width", w + "px");
    //append the canvas element
	var display = content.append("canvas").attr("width", w).attr("height", h).attr("id", "display");
	//instantiate the cursored display using the id of the element we just created
    var disp = new CursoredDisplay("display", w, h);
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
            client.getWebSocket().lastState(event.data);
            var res = event.data.toString();
            // FIXME: event.type === commandResult when pvsio-web was not able to evaluate the expression (e.g., because of missing pvs function)
            if (res.indexOf("(#") === 0) {
                res = parseState(event.data.toString());
                disp.renderNumber(evaluate(res.d).toString(), +res.c);
            }
        } else { console.log(err); }
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
			d3.selectAll("button").style("display", null);
		});
	}).addListener("WebSocketConnectionClosed", function (e) {
		console.log("web socket closed");
	}).addListener("processExited", function (e) {
		var msg = "Warning!!!\r\nServer process exited. See console for details.";
		console.log(msg);
	});
	
	client.connectToServer();
	
});
