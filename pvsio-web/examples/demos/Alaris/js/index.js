/**
 * 
 * @author Patrick Oladimeji
 * @date 11/21/13 17:11:09 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, window, PVSioWebClient */
(function () {
	"use strict";
	var content = d3.select("body").append("div");
	var client = new PVSioWebClient();
	var display = content.append("div");
	var logDiv = content.append("div");
	
	function log(msg) {
		logDiv.append("p").html(msg);
	}
	
	function show(msg) {
		display.html(msg);
	}
	
	function onMessageReceived(err, event) {
		client.getWebSocket().lastState(event.data);
		show(event.data.toString());
	}
	
	content.append("button").html("UP").on("click", function () {
		client.getWebSocket().sendGuiAction("click_UP(" + client.getWebSocket().lastState() + ");", onMessageReceived);
	});
	
	content.append("button").html("DN").on("click", function () {
		client.getWebSocket().sendGuiAction("click_DN(" + client.getWebSocket().lastState() + ");", onMessageReceived);
	});
	d3.selectAll("button").style("display", "none");
	
	client.addListener('WebSocketConnectionOpened', function (e) {
		log("web socket connected");
		//start pvs process
		client.getWebSocket().startPVSProcess({fileName: "alarisGP.pvs", demoName: "Alaris/pvs"}, function (err, event) {
			d3.selectAll("button").style("display", null);
		});
	}).addListener("WebSocketConnectionClosed", function (e) {
		log("web socket closed");
	}).addListener("processExited", function (e) {
		var msg = "Warning!!!\r\nServer process exited. See console for details.";
		log(msg);
	});
	
	client.connectToServer();
	
}());