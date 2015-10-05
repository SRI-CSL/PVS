/**
 * PVS syntax rules
 * @author Paolo Masci
 * @date 13/10/13 15:31 PM
 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var oop = require("ace/lib/oop"),
        TextHighlightRules = require("ace/mode/text_highlight_rules").TextHighlightRules,
        pvsHighlightRules = function () {
            var keywords = "theory|begin|end|importing|exporting|all|closure|" +
                            "assumption|assuming|endassuming|judgement|subtype_of|has_type|library|" +
                            "type|datatype|nonempty_type|type+|from|containing|var|recursive|measure|" +
                            "by|macro|inductive|conversion|conversion+|conversion-|function|array|" +
                            "lambda|forall|exists|table|endtable|" +
                            "if|then|else|elsif|end|" +
                            "iff|implies|or|and|xor|andthen|orelse|not|" +
                            "let|in|where|with|cases|of|cond|endcond|" +
                            "axiom|challenge|claim|conjecture|corollary|fact|formula|" +
                            "law|lemma|obligation|postulate|proposition|sublemma|theorem",
                storageType = (
                    "nonneg_real|real|int|integer|nat|naturalnumber|bool|boolean|char"
                ),
                builtinConstants = (
                    "true|false"
                ),
                preludeFunctionsAndConstants = (
                    "upto|below|upfrom|" +
                    "ceiling|floor|fractional|" +
                    "set|member|empty?|emptyset|nonempty?|nonempty_set|full|fullset|" +
                    "nontrivial?|subset?|strict_subset?|union|intersection|disjoint?|" +
                    "complement|difference|symmetric_difference|empty|every|some|" +
                    "singleton|singleton?|add|remove|choose|choose_is_epsilon|the"
                ),
                keywordMapper = this.createKeywordMapper({
                    "keyword": keywords,
                    "storage.type" : storageType,
                    "constant.language": builtinConstants,
                    "support.function": preludeFunctionsAndConstants
                }, "identifier", true);

            this.$rules = {
                "start" : [ {
                    token : "comment",
                    regex : "%.*$"
                }, {
                    token : "string",           // " string
                    regex : '".*?"'
                }, {
                    token : "string",           // ' string
                    regex : "'.*?'"
                }, {
                    token : "constant.numeric", // float
                    regex : "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
                }, {
                    token : keywordMapper,
                    regex : "[a-zA-Z_$][a-zA-Z0-9_$]*\\b"
                }, {
                    token : "keyword.operator",
                    regex : "\\+ |\\- |\\\\\/ |\\/\\\\ |\\/ |& |\\^ |~ |< |> |<= |=> |== |!= |<> |= |:= |\\(\\# |\\#\\) |~ |\\(\\: |\\:\\)"
                }, {
                    token : "paren.lparen",
                    regex : "[\\(]"
                }, {
                    token : "paren.rparen",
                    regex : "[\\)]"
                }, {
                    token : "text",
                    regex : "\\s+"
                } ]
            };
        };

    oop.inherits(pvsHighlightRules, TextHighlightRules);

    exports.pvsHighlightRules = pvsHighlightRules;
});


