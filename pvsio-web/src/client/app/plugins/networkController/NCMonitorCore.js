/**
 *
 * @author Piergiuseppe Mallozzi
 * @date 14/05/2015 11:33 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";
    var _this;

    var nc_websocket_monitor;
    var eventDispatcher = require("util/eventDispatcher");

    /**
     * @function NCMonitorCore
     * @description Constructor.
     * @param opt {Object} It contains the url of NC endpoint <br>
     *     <li>url </li>
     * @memberof module:NCMonitorCore
     * @instance
     */
    function NCMonitorCore(opt) {
        opt = opt || {};
        this.extended = opt.extended || false;
        this.debugging = opt.debugging || false;
        this.url = opt.url || "ws://localhost:8080/NetworkController/monitor";
        _this = this;
        eventDispatcher(this);
        return this;
    }


    NCMonitorCore.prototype.start = function () {
        return new Promise(function (resolve, reject) {
            nc_websocket_monitor = new WebSocket(_this.url);
            /*
             * It starts the control process that send the information to NC
             */
            nc_websocket_monitor.onopen = function () {
                _this.fire({type: "notify", message: "[MONITOR] Connected to ICE Network Controller!"});
                resolve();
                sendMonitorOptions();
            };

            nc_websocket_monitor.onmessage = onMessageReceivedNCMonitorCore;
            /*
             * Close event
             */
            nc_websocket_monitor.onclose = function () {
                _this.fire({
                    type: "notify",
                    message: "[MONITOR] Disconnected from ICE Network Controller (" + _this.url + ")"
                });
                nc_websocket_monitor = null;
                reject({code: "CLOSED"});
            };
            /*
             * Connection failed
             */
            nc_websocket_monitor.onerror = function () {
                _this.fire({
                    type: "error",
                    message: "[MONITOR] Unable to connect to ICE Network Controller (" + _this.url + ")"
                });
                nc_websocket_monitor = null;
                reject({code: "ERROR"});
            };
        });
    };


    /**
     * Callback function when a message is received from the sapere_handler network_controller
     * @param event
     */
    function onMessageReceivedNCMonitorCore(event) {
        var data = event.data;

        // JSON FORMAT
        if (tryJSON(data)) {
            data = tryJSON(data);
            parseAction(data);
        }
        // NO JSON
        else {
            _this.fire({type: "debug_backend", message: data});
        }

    }


    function parseAction(data) {
        /**
         * Notifies when a device has been successfully added to Sapere
         */
        if (data.action === "add") {
            _this.fire({type: "action", kind: "add", data: data});
        }

        /**
         * Notifies when a device has been successfully removed from Sapere
         */
        if (data.action === "remove") {
            _this.fire({type: "action", kind: "remove", data: data});

        }

        /**
         * Notifies when a device has been successfully activated or deactivated
         */
        if (data.action === "connected") {
            _this.fire({type: "action", kind: "connected", data: data});
        }

        /**
         * Notifies when a device has been successfully activated or deactivated
         */
        if (data.action === "disconnected") {
            _this.fire({type: "action", kind: "disconnected", data: data});
        }

        /**
         * Notifies when a publish-agent has been injected into the LSA space in Sapere.
         * Every publish-agent is is strictly connected to the device it publishes the data for.
         */
        if (data.action === "publish") {
            _this.fire({type: "action", kind: "publish", data: data});
        }

        /**
         * Notifies when a subscribe-agent has been injected into the LSA space in Sapere.
         * A device could have multiple subscribe-sapere_handler. For example the Supervisor has a subscribe-agent for every device.
         */
        if (data.action === "subscribe") {
            _this.fire({type: "action", kind: "subscribe", data: data});
        }

        /**
         * Notifies when a publish-agent has been removed from the LSA space in Sapere.
         */
        if (data.action === "publish-remove") {
            _this.fire({type: "action", kind: "publish-remove", data: data});
        }

        /**
         * Notifies when a subscribe-agent has been removed from the LSA space in Sapere.
         */
        if (data.action === "subscribe-remove") {
            _this.fire({type: "action", kind: "subscribe-remove", data: data});
        }

        /**
         * Notifies when a two sapere_handler have bonded so are exchanging data.
         */
        if (data.action === "bond-update") {
            _this.fire({type: "action", kind: "bond-update", data: data});
        }

        /**
         * Notifies when a two sapere_handler have bonded so are exchanging data.
         */
        if (data.action === "bond-added") {
            _this.fire({type: "action", kind: "bond-added", data: data});
        }

        /**
         * Notifies when a the device connected to the agent received a message;
         */
        if (data.action === "msg-update") {
            _this.fire({type: "action", kind: "msg-update", data: data});
        }


        /**
         * Sends the whole LSASpace of Sapere
         */
        if (data.action === "lsaspace") {
            _this.fire({type: "action", kind: "lsaspace", data: data});
        }

        /**
         * Sends the whole LSASpace of Sapere
         */
        if (data.action === "error") {
            _this.fire({type: "action", kind: "error_backend", data: data});
        }
    }


    /**
     * Sends Monitor options
     */
    function sendMonitorOptions() {
        var MonitorOptions = {
            action: "add_monitor",
            extended: _this.extended,
            debugging: _this.debugging
        };
        nc_websocket_monitor.send(JSON.stringify(MonitorOptions));
    }

    function tryJSON(jsonString) {
        try {
            var o = JSON.parse(jsonString);

            if (o && typeof o === "object" && o !== null) {
                return o;
            }
        }
        catch (e) {
        }

        return false;
    }

    NCMonitorCore.prototype.sendJSON = function (data) {
        nc_websocket_monitor.send(JSON.stringify(data));
    };


    NCMonitorCore.prototype.activateDebug = function () {
        var MonitorOptions = {
            action: "debug"
        };
        _this.fire({type: "notify", message: "Activating Back-End Debugging Mode..."});
        nc_websocket_monitor.send(JSON.stringify(MonitorOptions));
    };

    NCMonitorCore.prototype.deactivateDebug = function () {
        var MonitorOptions = {
            action: "no_debug"
        };
        _this.fire({type: "notify", message: "Deactivating Back-End Debugging Mode..."});
        nc_websocket_monitor.send(JSON.stringify(MonitorOptions));
    };

    module.exports = NCMonitorCore;

});