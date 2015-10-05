/**
 *
 * @author Piergiuseppe Mallozzi
 * @date 14/05/2015 11:33 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/

/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher");

    /**
     * @function NCDevice
     * @description Constructor.
     * @param device {!Object} This object describes the device. It has the following fields:<br>
     *     <li>id </li>
     *     <li>type </li>
     *     <li>description (optional) </li>
     * @param opt {Object} It contains the url of NC endpoint <br>
     *     <li>url </li>
     * @memberof module:NetworkController
     * @instance
     */
    function NCDevice(device, opt) {
        opt = opt || {};
        this.url = opt.url || "ws://localhost:8080/NetworkController/devices";
        this.deviceID = device.id;
        this.deviceType = device.type;
        this.deviceDescription = device.description || (device.type + " " + device.id);
        eventDispatcher(this);
        _this = this;
        return this;
    }


    var nc_websocket_device;
    var deviceAdded = false;
    var deviceON = false;
    var _this;

    NCDevice.prototype.start = function () {
        return new Promise(function (resolve, reject) {
            nc_websocket_device = new WebSocket(_this.url);
            /*
             * It starts the control process that send the information to NC
             */
            nc_websocket_device.onopen = function () {
                _this.fire({type: "notify", message: "Connected to ICE Network Controller!"});
                addDevice();
            };

            nc_websocket_device.onmessage = onMessageReceivedNCDevice(resolve, reject);
            /*
             * Close event
             */
            nc_websocket_device.onclose = function () {
                _this.fire({type: "notify", message: "Disconnected from ICE Network Controller (" + _this.url + ")"});
                nc_websocket_device = null;
                reject({code: "CLOSED"});
            };
            /*
             * Connection failed
             */
            nc_websocket_device.onerror = function () {
                _this.fire({type: "error", message: "Unable to connect to ICE Network Controller (" + _this.url + ")"});
                nc_websocket_device = null;
                reject({code: "ERROR"});
            };
        });
    };

    var addDevice = function () {
        if (nc_websocket_device != null) {
            if (!deviceAdded) {
                _this.fire({type: "notify", message: "-> adding " + _this.deviceID});
                var Action = {
                    action: "add",
                    deviceID: _this.deviceID,
                    type: _this.deviceType,
                    description: _this.deviceDescription
                };
                nc_websocket_device.send(JSON.stringify(Action));
            }
            else {
                _this.fire({type: "error", message: _this.deviceID + " already added"});
            }
        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };


    NCDevice.prototype.connect = function () {
        if (nc_websocket_device != null) {
            if (deviceAdded && !deviceON) {
                var DeviceAction = {
                    action: "connect",
                    deviceID: _this.deviceID
                };
                nc_websocket_device.send(JSON.stringify(DeviceAction));
            }
            else {
                _this.fire({type: "error", message: _this.deviceID + " not added or already ON"});
            }
        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };

    NCDevice.prototype.disconnect = function () {
        if (nc_websocket_device != null) {
            if (deviceAdded && deviceON) {
                var DeviceAction = {
                    action: "disconnect",
                    deviceID: _this.deviceID
                };
                nc_websocket_device.send(JSON.stringify(DeviceAction));
            }
            else {
                _this.fire({type: "error", message: _this.deviceID + " not added or already OFF"});
            }
        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };

    NCDevice.prototype.sendControlData = function (to, message) {
        if (nc_websocket_device != null) {
            if (deviceAdded && deviceON) {
                if (_this.deviceType === "Supervisor") {
                    _this.fire({type: "notify", message: "-> " + message + "\n - " + to});
                    var data = {
                        to: to,
                        msg: message
                    };
                    var DeviceAction = {
                        action: "publish",
                        deviceID: _this.deviceID,
                        message: data
                    };
                    nc_websocket_device.send(JSON.stringify(DeviceAction));
                }
                else {
                    _this.fire({
                        type: "error",
                        message: "This function is reserved to Devices with type 'Supervisor' \nUse sendDataUpdate() instead"
                    });
                }
            }
            else {
                _this.fire({type: "error", message: _this.deviceID + " not added or if OFF state"});
            }

        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };

    NCDevice.prototype.sendDataUpdate = function (message) {
        if (nc_websocket_device != null) {
            if (deviceAdded && deviceON) {
                if (_this.deviceType !== "Supervisor") {
                    _this.fire({type: "notify", message: "-> " + message});
                    var DeviceAction = {
                        action: "publish",
                        deviceID: _this.deviceID,
                        message: message
                    };
                    nc_websocket_device.send(JSON.stringify(DeviceAction));
                }
                else {
                    _this.fire({
                        type: "error",
                        message: "This function is reserved to Devices different from 'Supervisor' \nUse sendControlData() instead"
                    });
                }
            }
            else {
                _this.fire({type: "error", message: _this.deviceID + " not added or if OFF state"});
            }
        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };


    NCDevice.prototype.subscribeTo = function (publisherID) {
        if (nc_websocket_device != null) {
            _this.fire({type: "notify", message: _this.deviceID + " subTO -> " + publisherID});
            var DeviceAction = {
                action: "subscribe",
                deviceID: _this.deviceID,
                publisherID: publisherID
            };
            nc_websocket_device.send(JSON.stringify(DeviceAction));
        }
        else {
            _this.fire({type: "error", message: "Websocket not opened"});
        }
    };

    function isJSON(text) {
        try {
            JSON.parse(text);
            return true;
        }
        catch (err) {
            return false;
        }
    }

    /**
     * Callback function when a message is received from the nc websocket
     * @param resolve
     * @param reject
     */
    var onMessageReceivedNCDevice = function (resolve, reject) {
        return function (event) {

            var text = event.data;

            // JSON FORMAT
            if (isJSON(text)) {

                var data = JSON.parse(event.data);

                if (data.action === "add") {
                    deviceAdded = true;
                    _this.fire({type: "notify", message: "<- " + _this.deviceID + " added to NC", data: data});
                    resolve();
                }
                if (data.action === "remove") {
                    deviceAdded = false;
                    _this.fire({type: "notify", message: "<- " + _this.deviceID + " removed from NC"});
                }
                if (data.action === "connected") {
                    deviceON = true;
                    _this.fire({type: "notify", message: "<- " + _this.deviceID + " is now connected", data: data});
                    _this.fire({type: "connected", message: event.data});
                }
                if (data.action === "disconnected") {
                    deviceON = false;
                    _this.fire({type: "notify", message: "<- " + _this.deviceID + " is now disconnected", data: data});
                    _this.fire({type: "disconnected", message: event.data});
                }
                if (data.action === "error") {
                    _this.fire({type: "error", message: data.message});
                    reject({code: "ERROR"});
                }

                /**
                 * Update message from another device subscribed to
                 */
                if (data.action === "update") {
                    // orchestrate message
                    if (isJSON(data.message)) {
                        var content = JSON.parse(data.message);
                        // filtering destination device
                        if (content.to === _this.deviceID) {
                            _this.fire({type: "notify", message: "<- control message from: " + data.from, data: data});
                            _this.fire({
                                type: "control",
                                from: data.from,
                                message: content.msg,
                                data: data
                            });
                        }
                    }
                    else {
                        _this.fire({type: "notify", message: "<- update message from: " + data.from, data: data});
                        _this.fire({
                            type: "update",
                            from: data.from,
                            message: data.message,
                            data: data
                        });
                    }
                }

            }
            // NO JSON
            else {
                _this.fire({type: "notify", message: text});
            }
        };
    };

    module.exports = NCDevice;

});