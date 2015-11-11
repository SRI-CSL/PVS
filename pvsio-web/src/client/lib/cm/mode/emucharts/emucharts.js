/**
 * CodeMirror mode for the Emucharts language. Uses EmuchartsParser to parse the string.
 * @author Paolo Masci
 * @date 15/11/14 5:11:34 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global exports, module, require, define, CodeMirror */
(function (mod) {
    "use strict";
    if (typeof exports === "object" && typeof module === "object") {// CommonJS
        mod(require("../../lib/codemirror"));
    } else if (typeof define === "function" && define.amd) {// AMD
        define(["../../lib/codemirror"], mod);
    } else { // Plain browser env
        mod(CodeMirror);
    }
}(function (CodeMirror) {
    "use strict";
    
    var dbg = true;
    
    CodeMirror.defineMode("emucharts", function () {
        var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
        var parser = new EmuchartsParser();
        var rules = parser.getLexerRules();
        rules.push({ regex: new RegExp("."), type: "error" });
        var tokenLexer = function (stream, state) {
            var i = 0;
            for (i = 0; i < rules.length; i++) {
                if (stream.match(rules[i].regex)) {
                    if (dbg && rules[i].type !== "whitespace") {
                        console.log("found " + rules[i].type + " using rule " + rules[i].regex);
                    }
                    return rules[i].type;
                }
            }
            return "error";
        };
        return {
            token: tokenLexer
        };
    });
    CodeMirror.defineMIME("text/x-emucharts", "emucharts");
}));