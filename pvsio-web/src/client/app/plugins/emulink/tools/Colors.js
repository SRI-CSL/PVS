/**
 *
 * @author Paolo Masci
 * @date 28/10/15
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/
define(function (require, exports, module) {
    "use strict";
    
    var d3 = require("d3/d3");
    var colors = d3.scale.category10();
    
    module.exports = {
        getColor: function(id) {
            return (id === 0) ? d3.rgb(colors(id)).brighter().toString() : colors(id);
        }
    };
});
