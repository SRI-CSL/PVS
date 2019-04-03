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
    var pvsioweb_version = "2.3";

    var pvsws                   = require("websockets/pvs/pvsWSClient"),
		eventDispatcher			= require("util/eventDispatcher"),
		d3						= require("d3/d3"),
		property				= require("util/property"),
		ws,
		_port = (window.location.href.indexOf(".herokuapp.com") >= 0 ||
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
			.addListener("ConnectionOpened", function (e) {
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

        // Forward key events to the active panel. By using this instead of registering their own global key listeners,
        // panels avoid breaking the functionality of other panels or causing behaviour that is confusing for the user.
        d3.select("body").on("keydown.global", function () {
            var e = d3.event;
            if (d3.select("form").empty() && _pvsioweb._activePanel && typeof _pvsioweb._activePanel.handleKeyDownEvent === "function") {
                _pvsioweb._activePanel.handleKeyDownEvent(e);
            }
        });
        d3.select("body").on("keyup.global", function () {
            var e = d3.event;
            if (d3.select("form").empty() && _pvsioweb._activePanel && typeof _pvsioweb._activePanel.handleKeyUpEvent === "function") {
                _pvsioweb._activePanel.handleKeyUpEvent(e);
            }
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
			ownerObject: <object> Main class of the plugin that owns the panel. Used for forwarding key events.
			onClick: function - handler to invoke when the panel is toggled. Argument passed to the function identifies
            whether the panel is now collapsed (true) or not (false).
			showContent: Whether the default initial state of the panel is open (showContent == true) or closed (showContent == true or undefined)
			parent: the html element selector for the parent i.e., where the panel should be created
			}
        @returns {d3.selection} The div created
    */
	PVSioWeb.prototype.createCollapsiblePanel = function (options) {
        var _this = this;
        var collapsed;

		options = options || {};
		options.parent = options.parent || "#body";
        var _div = d3.select(options.parent).append("div").attr("class", "collapsible-panel-parent"); // use this to append
		// var _div = d3.select(options.parent).insert("div",":first-child").attr("class", "collapsible-panel-parent"); // use this to pre-pend
		var _header = _div.append("div").classed("header", true);
		var _content = _div.append("div").attr("class", "collapsible-panel");

        _header.append("span")
			.attr("class", function () {
				return options.showContent === true ? "toggle-collapse glyphicon glyphicon-minus-sign" :
						"toggle-collapse glyphicon glyphicon-plus-sign";
            });

        if (!options.isDemo && _header.node() && _header.node().firstChild) {
            d3.select(_header.node().firstChild).on("click", function () {
                var icon = d3.select(this.parentNode.firstChild);
                var label = d3.select(this.parentNode.lastChild);

                if (_content.attr("style") === null) {
                    _content.attr("style", "display: none");
                    label.node().textContent += " (click to expand)";
                    icon.classed("glyphicon-plus-sign", true).classed("glyphicon-minus-sign", false);
                    collapsed = true;
                } else {
                    _content.attr("style", null);
                    label.node().textContent = label.node().textContent.replace(" (click to expand)", "");
                    icon.classed("glyphicon-minus-sign", true).classed("glyphicon-plus-sign", false);
                    collapsed = false;
                }
                if (options.onClick && typeof options.onClick === "function") {
                    options.onClick(collapsed);
                }
            });
        }
        // var _resizebar = _div.append("div").style("width", "100%").style("height", "16px").attr("id", "resizebar")
        //                         .style("border", "solid 1px #08589a")
        //                         .style("opacity", 0.4).style("background-color", "gray").style("cursor", "ns-resize");
        // var dragResize = d3.behavior.drag()
        //                     // .origin(function () {
        //                     //     var origin = _div.node().parentNode.getBoundingClientRect();
        //                     //     console.log(origin);
        //                     //     return { x: 0, y: origin.top };
        //                     // })
        //                     .on("dragstart", function () {
        //                         d3.event.sourceEvent.stopPropagation();
        //                         _this.mtop = parseFloat(_resizebar.style("margin-top"));
        //                         console.log(_this.mtop);
        //                     })
        //                     .on("drag", function () {
        //                         d3.event.sourceEvent.stopPropagation();
        //                         var m = d3.mouse(_resizebar.node());
        //                         // update panel height
        //                         if (m && m.length > 1) {
        //                             console.log(parseFloat(_this.mtop + m[1]));
        //                             // var height = parseFloat(_div.style("height")) - m[1];
        //                             _resizebar.style("margin-top", (_this.mtop + m[1]) + "px");
        //                             // _div.style("height", height + "px");
        //                             // console.log(height);
        //                             // top = (top < maximised_table.top) ? maximised_table.top
        //                             //         : (top > minimized_table.top) ? minimized_table.top
        //                             //         : top;
        //                             // height = (height < minimized_table.height) ? minimized_table.height
        //                             //         : (height > maximised_table.height) ? maximised_table.height
        //                             //         : height;
        //                             // d3.select("#EmuchartsFloatTable").style("top", top + "px");
        //                             // d3.selectAll(".EmuchartsTableContent").style("width", "100% ").style("height", height + "px");
        //                         }
        //                     });
        // _resizebar.call(dragResize);


        if (options.ownerObject) {
            _div.on("mouseover", function () {
                _this._activePanel = options.ownerObject;
            });
        }

        if (options.width) {
            _div.style("width", options.width + "px");
        }

		if (options.owner) {
			_div.attr("plugin-owner", options.owner);
            _header.attr("id", options.owner); //this is useful for scrolling the page to the panel
		}
		if (options.headerText) {
			_header.append("span").html(options.headerText).attr("class", "header");
		}
		if (!options.showContent && !options.isDemo) {
            _header.node().lastChild.textContent += " (click to expand)";
            _content.style("display", "none");
        }
		return _content;
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

    PVSioWeb.prototype.version = function () {
        return pvsioweb_version;
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
