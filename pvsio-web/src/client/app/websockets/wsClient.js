/**
 * Generic websocket client that is able to listen to messages and handle specific results from
 * server function call
 * @author Patrick Oladimeji
 * @contributors Paolo Masci
 * @date 6/4/13 18:50:25 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext:true */
/*global define, WebSocket, Promise, _*/
define(function (require, exports, module) {
    "use strict";
    const property = require("util/property");
    const eventDispatcher = require("util/eventDispatcher");
    const uuid = require("util/uuidGenerator");
    const events = require("websockets/events");

    module.exports = function () {
        let o = eventDispatcher({}), ws, callbackRegistry = {}, receiversRegistry = {}, dbg = false;
        o.url = property.call(o, "ws://localhost");
		o.port = property.call(o);
        /**
         * Attempts to logon to the websocket server
         * returns a promise that resolves when the connection has been opened
         */
        o.logon = function () {
            return new Promise(function (resolve, reject) {
                if (ws) {
                    resolve(ws);
                } else {
                    let wsUrl = o.url();
                    if (o.port()) { wsUrl = wsUrl + ":" + o.port(); }
                    ws = new WebSocket(wsUrl);
                    ws.onopen = function (event) {
                        o.fire({type: events.ConnectionOpened, event: event});
                        resolve(ws);
                    };
                    ws.onerror = function (event) {
                        console.error("socket closed unexpectedly :/");
                        reject(event);
                    };
                    ws.onclose = function (event) {
                        ws = undefined;
                        o.fire({type: events.ConnectionClosed, event: event});
                        reject(event);
                    };
                    //when a message is received, look for the callback for that message id in the callbackRegistry
                    //if no callback exists then broadcast the event using the token type string
                    ws.onmessage = function (event) {
                        let token = JSON.parse(event.data);
                        //if token has an id check if there is a function to be called in the registry
                        if (token) {
                            if (token.err && !token.id) {
                                console.error("Warning: server replied with error state", token.err);
                                // these are critical errors such as websocket being closed
                                if (token.err.code !== "EPIPE") {
                                    console.error(JSON.stringify(token)); // errors should always be reported in the browser console
                                }
                                // clear callback log to unlock functions waiting
                                _.each(callbackRegistry, function (f) {
                                    f.call(o, token.err, null);
                                });
                                callbackRegistry = {}; // clean up callback registry for critical errors
                            }
                            if (token.type === "ctrl") {
                                let cbs = receiversRegistry[token.data.channelID];
                                if (cbs && cbs.length > 0) {
                                    cbs.forEach(function (f) {
                                        if (f && typeof f === "function") {
                                            f(token.data);
                                        }
                                    });
                                }
                            }
                            if (token.id && typeof callbackRegistry[token.id] === "function") {
                                let time = new Date().getTime() - token.time.client.sent;
                                console.log("Time to response", time, "ms");
                                if (token.type.indexOf("_error") >= 0 && dbg) {
                                    console.error(token); // errors should always be reported in the browser console
                                }
                                let f = callbackRegistry[token.id];
                                delete callbackRegistry[token.id];
                                f.call(o, token.err, token);
                            } else if (token.type && token.type !== "FileSystemUpdate") {
                                console.error("Warning: token id is not in callback registry");
                                o.fire(token);
                            }
                        }
                    };
                }
            });
        };
        /**
         * sends a message and register a callback to invoke when the message response is received from the server.
         */
        o.send = function (token, cb) {
            if (ws) {
                let id = uuid();
                if (token && token.type) {
                    token.id = token.id || id;
                    token.time = { client: { sent: new Date().getTime() } };
                    if (token.data && token.data.command && typeof token.data.command === "string") {
                        // removing white space is useful to reduce the message size (e.g., to prevent stdin buffer overflow)
                        token.data.command = token.data.command.split(",").map(function(str) { return str.trim(); }).join(",");
                    }
                    callbackRegistry[id] = cb;
                    ws.send(JSON.stringify(token));
                } else {
                    console.error("Token is undefined or malformed");
                    console.error(token);
                }
            } else {
                console.error("Cannot send: WebSocket is closed :/");
            }
            return o;
        };
        /**
            closes the websocket connection
        */
        o.close = function () {
            if (ws) {
                ws.close();
                ws = undefined;
                console.log("Client closes websocket connection...");
            }
        };

        o.registerReceiver = function (channelID, cb) {
            if (!receiversRegistry[channelID]) {
                receiversRegistry[channelID] = [];
            }
            receiversRegistry[channelID].push(cb);
            // if (ws) {
            //     if (!receiversRegistry[channelID]) {
            //         receiversRegistry[channelID] = [];
            //     }
            //     ws.send(JSON.stringify({
            //         id: uuid(),
            //         time: { client: { sent: new Date().getTime() } },
            //         type: "registerReceiver"
            //     }));
            // }
        };

        return o;
    };
});
