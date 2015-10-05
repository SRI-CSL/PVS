/**
 * Manages user action recording for project
 * @author Patrick Oladimeji
 * @date 3/24/14 16:17:55 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var actions = [];//list of objects representing user actions
    var isRecording = false;

    function startRecording() {
        actions = [];
        isRecording = true;
    }

    function addAction(action) {
        if (isRecording) {
            actions.push(action);
            console.log(action);
        }
    }

    function stopRecording() {
        isRecording = false;
        return actions;
    }

    module.exports = {
        startRecording: startRecording,
        addAction: addAction,
        stopRecording: stopRecording,
        isRecording: function () {
            return isRecording;
        }
    };
});
