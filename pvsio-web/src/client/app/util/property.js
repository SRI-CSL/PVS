/**
 * utility function for defining a property inside a function
 * @author hogfather
 * @date Apr 25, 2012
 * @project JSLib
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/

define(function (require, exports, module) {
    "use strict";
    var eventDispatcher = require("util/eventDispatcher");

    //defines property function
    module.exports = function (v) {
        var p = function (_) {
            if (!arguments.length) {
                return v;
            }
            //fire property changed event if _ (incoming) is not equal to v (old property)
            if (v !== _) {
                var event = {type: "PropertyChanged", old: v, fresh: _};
                v = _;
                p.fire(event);
            }
            return this;
        };
        p = eventDispatcher(p);
        return p;
    };
});
