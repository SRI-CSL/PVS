/**
 * PVS syntax highlighter
 * @author Paolo Masci
 * @date 13/10/13 15:31 PM
 */
/*global define*/
define("ace/mode/pvsLanguage", function (require, exports, module) {
    "use strict";

    var oop = require("ace/lib/oop"),
        TextMode = require("ace/mode/text").Mode,
        pvsHighlightRules = require("plugins/emulink/pvs_highlight_rules").pvsHighlightRules,
        Mode = function () {
            this.HighlightRules = pvsHighlightRules;
        };
    oop.inherits(Mode, TextMode);

    (function () {

        this.lineCommentStart = "%";

        // TODO: create worker for live syntax checking
        //this.createWorker = function(session) {
        //};

    }).call(Mode.prototype);

    exports.Mode = Mode;

});

