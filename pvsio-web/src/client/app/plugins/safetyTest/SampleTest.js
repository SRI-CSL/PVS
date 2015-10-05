/**
 * Sample list of test cases to run
 * @author Patrick Oladimeji
 * @date 9/24/14 11:37:17 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */
define(function (require, exports, module) {
    "use strict";
    var tests = [
        { init: "init(0)",
         keySequence: ["SELECT_RATE", "5", "SELECT_VTBI"],
         designIssue: "dispval($) /= 5;"
        },
        {
            init: "init(0)",
            keySequence: ["SELECT_RATE", "0", "4"],
            designIssue: "dispval($) = 4;"
        }
    ];
    
    module.exports = tests;
});
