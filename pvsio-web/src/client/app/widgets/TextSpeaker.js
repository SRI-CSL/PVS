/**
 * uses the speaker.js module to read out what is displayed on the user interface
 * @author Patrick Oladimeji
 * @date 2/24/14 21:18:04 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, speak */
define(function (require, exports, module) {
    "use strict";
    var speaking = false, allowOverlap = true;

    module.exports = {
        speak: function (message) {
            //skip successive speak calls if overlap is disabled
            if (allowOverlap) {
                speak(message);
            } else {
                if (!speaking) {
                    speaking = true;
                    speak(message, null, function () {
                        speaking = false;
                    });
                }
            }
        },
        allowOverlap: function (val) {
            allowOverlap = val;
        }
    };
});
