/**
 * 
 * @author Patrick Oladimeji
 * @date 3/17/14 13:56:33 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, window, MouseEvent */
require.config({
    baseUrl: "../../client/app",
    paths: {
        d3: "../lib/d3"
    }
});

require(["PVSioWebClient", "util/PVSioStateParser",
         "d3/d3", "../../demos/TCAS/scripts/TCASView"], function (PVSioWebClient, StateParser, d3, TCASView) {
    "use strict";

    var timer, t = 500;
    var client = PVSioWebClient.getInstance();
    var ws = client.getWebSocket();
    var responses = [];
           
    function showRange() {
        responses = responses.filter(function (d) {
            return d.si && d.so;
        });
        var last = responses[responses.length - 1];
        var irangex = d3.extent(responses, function (d) { return +d.si.x; }),
            irangey = d3.extent(responses, function (d) {return +d.si.y; }),
            orangex = d3.extent(responses, function (d) {return +d.so.x; }),
            orangey = d3.extent(responses, function (d) {return +d.so.y; }),
            xdelta = +last.so.x - +last.si.x,
            ydelta = +last.so.y - +last.si.y;
        d3.select("#range").append("p").html(JSON.stringify([irangex, irangey, orangex, orangey, xdelta, ydelta].map(function (d) {
            return Array.isArray(d) ? d.map(function (a) { return a.toFixed(2); }) : d.toFixed(2);
        })));
    }
             
    function tick() {
        ws.sendGuiAction("tick(" + ws.lastState() + ");", function (err, res) {
            var pvsState = res.data.join("").replace(/d0/g, "");
            
            pvsState = pvsState.substring(pvsState.indexOf("(#"));
            
            ws.lastState(pvsState);
            var state = StateParser.parse(pvsState);
            console.log(pvsState);
            responses.push(state);
            TCASView.render(state);
        });
    }
        
    client.addListener("WebSocketConnectionOpened", function (e) {
        console.log("connected");
        ws.startPVSProcess({name: "top.pvs", demoName: "TCAS/pvs"}, function (err, event) {
            d3.select("#loading").style("display", "none");
            timer = setInterval(tick, t);
            d3.select("#pause").on("click", function () {
                clearInterval(timer);
            });
            
            d3.select("#resume").on("click", function () {
                timer = setInterval(tick, t);
            });
        });
    }).addListener("WebSocketConnectionClosed", function (e) {
        clearInterval(timer);
    }).addListener("processExited", function (e) {
        clearInterval(timer);
    });
    
    client.connectToServer();
});