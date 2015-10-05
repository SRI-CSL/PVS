/**
 * CodeMirror mode for the MAL language.
 * @author Paolo Masci
 * @date 22/10/14 17:57:32 PM
 * This file is based on the pvs syntax highlighter developed by Patrick
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, window, CodeMirror, exports, module */
(function (mod) {
    "use strict";
    if (typeof exports === "object" && typeof module === "object") { // CommonJS
        mod(require("../../lib/codemirror"));
    } else if (typeof define === "function" && define.amd) { // AMD
        define(["../../lib/codemirror"], mod);
    } else { // Plain browser env
        mod(CodeMirror);
    }
})(function (CodeMirror) {
    "use strict";
    CodeMirror.defineMode("mal", function () {
        function toMap(str) {
            var res = {};
            str.split("|").forEach(function (d) {
                res[d.trim()] = true;
            });
            return res;
        }
        var opStart = "+ |- |\\/ |/\\ |/ |& |^ |~ |< |> |<= |=> |== |!= |<> |= |:= |(# |#) |~ |(: |:)".split("|").map(function (o) {
            return o.trim()[0];
        });
        var operatorRegex = new RegExp("\\+ |\\- |\\\\\/ |\\/\\\\ |\\/ |& |\\^ |~ |< |> |<= |=> |== |!= |<> |= |:= |\\(\\# |\\#\\) |~ |\\(\\: |\\:\\)"),
            constantNumRegex = new RegExp("[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b");
            
        var keywords = toMap("defines|types|interactor|attributes|actions|per|if|then|else|elsif|end|iff|implies|or|and|xor|andthen|orelse|not|let|in|where|with|cases|of|test");

        var storageType = toMap("int|nat|bool");

        var builtinConstants = toMap("true|false");
        
        function stringTokenizer(quote) {
            return function (stream, state) {
                var escaped = false, next, end = false;
                while ((next = stream.next()) != null) {
                    if (next === quote && !escaped) {
                        end = true;
                        break;
                    }
                    escaped = !escaped && next === "\\";
                }
                if (end || !escaped) {
                    state.tokenize = null;
                }
                return "string";
            };
        }
        
        function tokenBase(stream, state) {
            var ch = stream.next();
            if (ch === "#") {
                stream.skipToEnd();
                return "comment";
            }
            
            if (ch === "'" || ch === "[" || ch === "]" || ch === "{" || ch === "}" || ch === "(" || ch === ")") {
                return "builtin";
            }

            
            if (ch === '"') {
                state.tokenize = stringTokenizer(ch);
                return state.tokenize(stream, state);
            }

            if (/\d|\./.test(ch)) {
                stream.eatWhile(constantNumRegex);
                return "number";
            }
            
            if (opStart.indexOf(ch) > -1) {
                stream.eatWhile(operatorRegex);
                return "operator";
            }
            
            stream.eatWhile(/[\w\$_]/);
            var word = stream.current().toLowerCase();
            if (storageType[word]) { return "builtin"; }
            if (keywords[word]) { return "keyword"; }
            
            if (builtinConstants[word]) { return "atom"; }
            return "variable";
        }
        
        return {
            startState: function () {
                return {tokenize: null};
            },
            token: function (stream, state) {
                if (stream.eatSpace()) {return null; }
                return (state.tokenize || tokenBase)(stream, state);
            },
            fold: "indent",
            lineComment: "#"
        };
    });
    CodeMirror.defineMIME("text/x-mal", "mal");
});