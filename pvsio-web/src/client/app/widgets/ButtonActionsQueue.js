/**
 * Manages a queue of messages to send to the server
 */
define(function (require, exports, module) {

    var guiActions, instance;

    var WSManager = require("websockets/pvs/WSManager");

    function ButtonActionsQueue() {
        guiActions = Promise.resolve();
    }

    function getGUIActionPromise(action, cb) {
        var ws = WSManager.getWebSocket();
        cb = cb || function () {};
        return new Promise(function (resolve, reject) {
            ws.sendGuiAction(action, function (err, res) {
                cb(err, res);
                if (err) {
                    reject(err);
                } else {
                    console.log(res.command);
                    console.log(res.data);
                    resolve(res);
                }
            });
        });
    }

     /**
            Queue the next gui action onto the promise chain. This ensures that actions are
            executed on the server in the same order as they are sent on the client
            @param {string} action the concatenation of button action and function name to call in pvs on the server
                e.g., "click_bigUP"
        */
    ButtonActionsQueue.prototype.queueGUIAction = function (action, cb) {
        var ws = WSManager.getWebSocket();
        guiActions = guiActions.then(function (res) {
            var guiAction = action;
            if (action !== "<ping>" && action !== "<pong>") {
                guiAction += "(" + ws.lastState().toString().replace(/,,/g, ",") + ");";
            }
            var guiActionPromise = getGUIActionPromise(guiAction, cb);
            return guiActionPromise;
        }).catch(function (err) {
            if (!(typeof err.message === "string" && err.message.indexOf("No resolution for tick") >= 0)) {
                if (err.code === "EPIPE") {
                    console.log("Unable to evaluate command in PVSio :/");
                } else {
                    console.error(err);
                }
            }
        });
    };

    ButtonActionsQueue.prototype.sendINIT = function (cb) {
        return this.queueGUIAction("", cb);
    };

    ButtonActionsQueue.prototype.send = function (action, cb) {
        return this.queueGUIAction(action, cb);
    };

    module.exports = {
        getInstance: function () {
            instance = instance || new ButtonActionsQueue();
            return instance;
        }
    };
});
