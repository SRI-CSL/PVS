/**
 * @module PVSioWebClient
 * PVSio WebClient is the core component of pvsioweb. It creates a websocket connection to a nodejs server running
 * on localhost on port 8082
 * @author Patrick Oladimeji
 * @date 4/19/13 17:23:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/

define(function (require, exports, module) {
    "use strict";
    var pvsws                   = require("websockets/pvs/pvsWSClient"),
		eventDispatcher			= require("util/eventDispatcher"),
		d3						= require("d3/d3"),
		property				= require("util/property"),
		ws,
		_port = (window.location.href.indexOf("pvsioweb.herokuapp.com") >= 0 ||
                   window.location.href.indexOf("pvsioweb.org") >= 0) ? 0 : 8082,
		url = window.location.href.indexOf("file") === 0 ?
				("ws://localhost") : ("ws://" + window.location.hostname),
        instance;
	
	/**
	 * Creates a new PVSioWeb client object. This object is an event emitter and emits the following events:
	 * @constructor
	 */
	function PVSioWeb() {
		eventDispatcher(this);
		var _pvsioweb = this;
		//create pvs websocket connection
		//add listeners for pvs process events
		ws = pvsws()
			.serverUrl(url)
			.addListener('ConnectionOpened', function (e) {
                e.type = "WebSocketConnectionOpened";
				_pvsioweb.isWebSocketConnected(true).fire(e);
			}).addListener("ConnectionClosed", function (e) {
                e.type = "WebSocketConnectionClosed";
				_pvsioweb.isWebSocketConnected(false).fire(e);
			}).addListener("pvsoutput", function (e) {
				_pvsioweb.fire(e);
			}).addListener("processExited", function (e) {
				_pvsioweb.isPVSProcessConnected(false).fire(e);
			});
	}
    /**
     Get or set whether the client is connected to the server websocket
    */
    PVSioWeb.prototype.isWebSocketConnected = property.call(PVSioWeb.prototype, false);
    /**
        Get or set whether the client is connected to the server pvsprocess
    */
    PVSioWeb.prototype.isPVSProcessConnected = property.call(PVSioWeb.prototype, false);
    /**get or set the port for the server connection */
	PVSioWeb.prototype.port = property.call(PVSioWeb.prototype, _port);
	
    /**
        Get or set the url for the server connection
    */
	PVSioWeb.prototype.serverUrl = property.call(PVSioWeb.prototype, url);
    /**
     * Checks whether the server is running on localhost
     */
	PVSioWeb.prototype.serverOnLocalhost = function () { return url.indexOf("ws://localhost") === 0; };
	/**
        Initiate connection to the server.
        Returns a promise object that resolves to the websocket connection when the connection opens
    */
	PVSioWeb.prototype.connectToServer = function () {
		if (this.isWebSocketConnected()) {
			return Promise.resolve(this.getWebSocket());
		}
        if (this.port()) {
            return ws.serverUrl(this.serverUrl()).port(this.port()).logon();
        } else {
            return ws.serverUrl(this.serverUrl()).logon();
        }
	};
	
    /**
        Disconnects from the server
    */
	PVSioWeb.prototype.disconnectFromServer = function () {
		ws.close();
		return this;
	};
	/**
        Get the websocket connection
    */
	PVSioWeb.prototype.getWebSocket = function () { return ws; };
	
    /**
        Creates a collapsible panel on the client app
		@param {object} options 
			{
			headerText: string to display in panel header
			owner: <string> the name of the plugin that owns the panel
			onClick: function - handler to invoke when the panel is toggled
			showContent: Whether the default initial state of the panel is open (showContent == true) or closed (showContent == true or undefined)
			parent: the html element selector for the parent i.e., where the panel should be created
			}
        @returns {d3.selection} The div created
    */
	PVSioWeb.prototype.createCollapsiblePanel = function (options) {
		options = options || {};
		options.parent = options.parent || "#body";
		var div = d3.select(options.parent).append("div").attr("class", "collapsible-panel-parent");
		var header = div.append("div").classed("header", true);
		var content = div.append("div").attr("class", "collapsible-panel");
		
        if (!options.isDemo) {
            header.on("click", function () {
                var icon = d3.select(this.firstChild);
                var label = d3.select(this.lastChild);
                if (content.attr("style") === null) {
                    content.attr("style", "display: none");
                    label.node().textContent += " (click to expand)";
                    icon.classed("glyphicon-plus-sign", true).classed("glyphicon-minus-sign", false);
                } else {
                    content.attr("style", null);
                    label.node().textContent = label.node().textContent.replace(" (click to expand)", "");
                    icon.classed("glyphicon-minus-sign", true).classed("glyphicon-plus-sign", false);
                }
                if (options.onClick && typeof options.onClick === "function") {
                    options.onClick();
                }
            });
        }
		header.append("span")
			.attr("class", function () {
				return options.showContent === true ? "toggle-collapse glyphicon glyphicon-minus-sign" :
						"toggle-collapse glyphicon glyphicon-plus-sign";
            });
		if (options.owner) {
			div.attr("plugin-owner", options.owner);
		}
		if (options.headerText) {
			header.append("span").html(options.headerText).attr("class", "header");
		}
		if (!options.showContent && !options.isDemo) {
            header.node().lastChild.textContent += " (click to expand)";
            content.style("display", "none");
        }
		return content;
	};
    
    /**
        Removes the collapsible specified in the parameter.
        @param {d3.selection} container The div returned from a call to createCollapsiblePanel
    */
    PVSioWeb.prototype.removeCollapsiblePanel = function (container) {
        if (container && !container.empty() && container.node()) {
            var parent = d3.select(container.node().parentElement);
            if (parent.classed("collapsible-panel-parent")) {
                parent.remove();
            }
        }
    };
    
    /**
        Adds a stylesheet with the specified url to the page
     */
    PVSioWeb.prototype.addStyleSheet = function (url, cb) {
        cb = cb || function () {};
        var link = d3.select("html head").append("link").attr("type", "text/css").attr("rel", "stylesheet").attr("href", url);
        link.on("load", function () {
            cb(null, link);
        }).on("error", function () {
            cb("error");
        });
    };
	
	module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new PVSioWeb();
            }
            return instance;
        }
    };
});
