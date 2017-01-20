/**
 * Manages a queue of messages to send to the server
 */
define(function (require, exports, module) {

    var guiActions,
        instance;

    var WSManager               = require("websockets/pvs/WSManager");

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
            var guiAction = action + "(" + ws.lastState().toString().replace(/,,/g, ",") + ");";
            var guiActionPromise = getGUIActionPromise(guiAction, cb);
            return guiActionPromise;
        }).catch(function (err) {
            console.log(err);
        });
    };

    ButtonActionsQueue.prototype.sendINIT = function (cb) {
        return this.queueGUIAction("", cb);
    };

    module.exports = {
        getInstance: function () {
            instance = instance || new ButtonActionsQueue();
            return instance;
        }
    };
});
