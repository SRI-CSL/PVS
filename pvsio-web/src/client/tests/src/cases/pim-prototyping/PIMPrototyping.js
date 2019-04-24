/**
 * Aggregates the various tests for the PIM prototyping mode (and related features)
 * @author Nathaniel Watson <nathaniel@nwatson.nz>
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, describe */

define(function (require, exports, module) {
    "use strict";

    var PIMViews = require("./views/PIMViews"),
        PIMUnits = require("./unit/PIMUnits");
    module.exports = {
        run: function () {
            describe("The behaviour of the PIM Prototyping", require("./PIMPrototyping_Behavior"));
            PIMViews.run();
            PIMUnits.run();
        }
    };
});
