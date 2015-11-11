/**
 * CodeMirror mode for the PVS language.
 * @author Patrick Oladimeji
 * @date 5/24/14 20:27:57 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, brackets, window, CodeMirror */
(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
    "use strict";
    CodeMirror.defineMode("pvs", function () {
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
            
        var keywords = toMap("theory|begin|end|importing|exporting|all|closure|assumption|assuming|endassuming|judgement|subtype_of|has_type|library|type|datatype|nonempty_type|type+|from|containing|var|recursive|measure|by|macro|inductive|conversion|conversion+|conversion-|function|array|lambda|forall|exists|table|endtable|if|then|else|elsif|end|iff|implies|or|and|xor|andthen|orelse|not|let|in|where|with|cases|of|cond|endcond|axiom|challenge|claim|conjecture|corollary|fact|formula|law|lemma|obligation|postulate|proposition|sublemma|theorem");

        var storageType = toMap("nonneg_real|real|int|integer|nat|naturalnumber|bool|boolean|char");

        var builtinConstants =toMap("true|false");

        var preludeFunctionsAndConstants = toMap("upto|below|upfrom|ceiling|floor|fractional|set|member|empty?|emptyset|nonempty?|nonempty_set|full|fullset|nontrivial?|subset?|strict_subset?|union|intersection|disjoint?|complement|difference|symmetric_difference|empty|every|some|singleton|singleton?|add|remove|choose|choose_is_epsilon|the");
        
        function stringTokenizer(quote) {
            return function (stream, state) {
                var escaped = false, next, end = false;
                while((next = stream.next()) != null) {
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
            if (ch === "%") {
                stream.skipToEnd();
                return "comment";
            }
            
            if (ch === "'" || ch === '"') {
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
            if (storageType[word] || preludeFunctionsAndConstants[word]) { return "builtin"; }
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
            lineComment: "%"
        };
    });
    CodeMirror.defineMIME("text/x-pvs", "pvs");
});