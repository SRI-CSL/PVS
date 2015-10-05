// Author: Paolo Masci
// code based on anyword-hint.js, originally developed from Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

    var WORD = /[\w$]+/, RANGE = 500;
    
    var modellingKeywords =  "theory|begin|end|importing|type|datatype|nonempty_type|type+|from|containing|recursive|measure|inductive|conversion|lambda|forall|exists|if|then|else|elsif|end|implies|or|and|xor|not|let|in|with|cases|of|cond|endcond".split("|");
    var modellingKeywordsCapitalised = "THEORY|BEGIN|END|IMPORTING|TYPE|DATATYPE|NONEMPTY_TYPE|TYPE+|FROM|CONTAINING|RECURSIVE|MEASURE|INDUCTIVE|CONVERSION|LAMBDA|FORALL|EXISTS|IF|THEN|ELSE|ELSIF|END|IMPLIES|OR|AND|XOR|NOT|LET|IN|WITH|CASES|OF|COND|ENDCOND".split("|");
    
    var storageType = "nonneg_real|real|int|integer|nat|naturalnumber|bool|boolean|char".split("|");

    var builtinConstants = "true|false".split("|");

    var preludeFunctionsAndConstants = "upto|below|upfrom|ceiling|floor|fractional|set|member|empty?|emptyset|nonempty?|nonempty_set|full|fullset|nontrivial?|subset?|strict_subset?|union|intersection|disjoint?|complement|difference|symmetric_difference|empty|every|some|singleton|singleton?|add|remove|choose|choose_is_epsilon|the".split("|");


  CodeMirror.registerHelper("hint", "pvs", function(editor, options) {
    var word = options && options.word || WORD;
    var range = options && options.range || RANGE;
    var cur = editor.getCursor(), curLine = editor.getLine(cur.line);
    var start = cur.ch, end = start;
    while (end < curLine.length && word.test(curLine.charAt(end))) {
        ++end;
    }
    while (start && word.test(curLine.charAt(start - 1))) {
        --start;
    }
    var curWord = start != end && curLine.slice(start, end);
      
    var list = [], seen = {};
    var re = new RegExp(word.source, "g");
    for (var dir = -1; dir <= 1; dir += 2) {
        var line = cur.line,
            endLine = Math.min(Math.max(line + dir * range, editor.firstLine()),
                               editor.lastLine()) + dir;
        for (; line != endLine; line += dir) {
            var text = editor.getLine(line), m;
            while (m = re.exec(text)) {
                if (line == cur.line && m[0] === curWord) {
                    continue;
                }
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }
            while (m = re.exec(modellingKeywords)) {
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }
            while (m = re.exec(modellingKeywordsCapitalised)) {
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }            
            while (m = re.exec(storageType)) {
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }            
            while (m = re.exec(builtinConstants)) {
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }            
            while (m = re.exec(preludeFunctionsAndConstants)) {
                if ((!curWord || m[0].lastIndexOf(curWord, 0) == 0) &&
                    !Object.prototype.hasOwnProperty.call(seen, m[0])) {
                    seen[m[0]] = true;
                    list.push(m[0]);
                }
            }            
        }
    }
    return {list: list, from: CodeMirror.Pos(cur.line, start), to: CodeMirror.Pos(cur.line, end)};
  });
});