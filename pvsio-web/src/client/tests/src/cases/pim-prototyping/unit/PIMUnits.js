/**
 * Aggregates the various tests for the PIM prototyping mode components
 * @author Nathaniel Watson <nathaniel@nwatson.nz>
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, describe */

define(function (require, exports, module) {
    "use strict";

    module.exports = {
        run: function () {
            describe("The PIM Prototyping components",

            function() {
                require("./ScreenCollection_UnitTest")();
                require("./Screen_UnitTest")();
                require("./PIMPrototyper_UnitTest")();
                require("./PIMProjectManager_UnitTest")();
                require("./PIMWidget_UnitTest")();
                require("./EmuWrapper_UnitTest")();
            }
        );
        }
    };
});
