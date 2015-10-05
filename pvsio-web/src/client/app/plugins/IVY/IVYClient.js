/**
 *
 * @author Paolo Masci
 * @date 28/05/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher");

    /**
     * @function IVYClient
     * @description Constructor.
     * @param ...
     * @memberof module:IVYClient
     * @instance
     */
    function IVYClient(opt) {
        opt = opt || {};
        this.url = opt.url || "ws://localhost:2317";
        eventDispatcher(this);
        _this = this;
        return this;
    }

    var _this;
    
    /**
     * Callback function when a message is received from the nc websocket
     * @param event
     */
    function onMessageReceived(event) {
        var res = event;
        console.log("IVY-ws -- new message received --");
        console.log(res);
        _this.fire({type: "ivyMessageReceived", res: res});
    }


    IVYClient.prototype.connect = function () {
        return new Promise(function (resolve, reject) {
            _this.ws = new WebSocket(_this.url);
            _this.ws.onopen = function () {
                console.log("IVY-ws connection open");
                _this.fire({type: "WebSocketConnectionOpened", message: "Connected to IVY!", serverURL: _this.url});
                resolve();
            };
            _this.ws.onmessage = onMessageReceived;
            _this.ws.onclose = function () {
                console.log("IVY-ws connection closed");
                _this.fire({type: "WebSocketConnectionClosed", message: "Disconnected from IVY (" + _this.url + ")", serverURL: _this.url});
                _this.ws = null;
                reject({ code: "CLOSED" });
            };
            _this.ws.onerror = function () {
                console.log("IVY-ws connection error");
                _this.fire({type: "WebSocketConnectionError", message: "Unable to connect to IVY (" + _this.url + ")", serverURL: _this.url});
                _this.ws = null;
                reject({ code: "ERROR" });
            };
        });
    };

    IVYClient.prototype.send = function(cmd) {
        if(_this.ws && _this.ws.readyState === _this.ws.OPEN) {
            _this.fire({type: "WebSocketSend", message: "Sending command over websocket connection", serverURL: _this.url, cmd: cmd});
            _this.ws.send(cmd);
        }
        else{
            _this.fire({type: "WebSocketConnectionError", message: "Websocket not opened", serverURL: _this.url});
        }
    };

    module.exports = IVYClient;

});