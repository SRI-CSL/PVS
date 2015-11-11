/**
 * manages websocket connection to the pvsio-web server-- currently useful
 * @author Patrick Oladimeji
 * @date 6/20/13 10:45:57 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    module.exports = {
        getWebSocket: function () {
            return require("./pvsWSClient")();
        }
    };
});
