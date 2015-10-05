/**
 * This is a JSDoc plugin used to strip off the "define(.." and ")};" in amd defined modules
 * @author Patrick Oladimeji
 * @date 11/26/13 20:18:10 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, exports, MouseEvent */
exports.handlers = {
    beforeParse: function (e) {
        "use strict";
        console.log("processing file " + e.filename);
        var lines = e.source.split("\n"), foundDefine = false;
        e.source = lines.filter(function (line) {
            return line.trim().length;
        }).map(function (line, index, lines) {
            if (!foundDefine) {
                foundDefine = line.trim().indexOf("define") === 0;
                return "";
            }
            return (foundDefine && index === lines.length - 1 && line.trim() === "});") ? "" : line;
        }).join("\n");
    }
};