/** @module EmuchartsGrammar*/
/**
 * EmuchartsGrammar defines the modes of the emuchart editor. This is code is a re-engineered version of stateMachine.js implemented in branch emulink-commented
 * @author Paolo Masci
 * @date 16/05/14 10:44:12 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var instance;

    function EmuchartsGrammar() { }

    EmuchartsGrammar.prototype.grammar4triggers = function () {
        return {
            lex: {
                rules: [
                    ["\\s+", "/* skip whitespace */", "whitespace"],
                    ["[^\\[|\\]|\\{|\\}]+", // any non-whitespace character except '{' '}' '[' ']'
                     "return 'generic_expression'", "generic_expression"],
                    ["\\(", "return '('"],
                    ["\\)", "return ')'"],
                    ["\\[", "return '['"],
                    ["\\]", "return ']'"],
                    ["\\{", "return '{'"],
                    ["\\}", "return '}'"]
                ]
            },
            operators: [
                ["left", "[", "]", "{", "}"]
            ],
            start: "production",
            bnf: {
                production: [ ["transition", "return $1"] ],
                transition: [
                    ["id [ cond ] { actions } ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: $cond, actions: $actions }}"],
                    ["id { actions } ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: null, actions: $actions }}"],
                    ["id [ cond ] ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: $cond, actions: null }}"],
                    ["id",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: null, actions: null }}"],
                    ["[ cond ]",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: { type: 'identifier', val: 'tick' }, cond: $cond, actions: null }}"],
                    ["{ actions }",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: { type: 'identifier', val: 'tick' }, cond: null, actions: $actions }}"]
                ],
                id: [
                    ["generic_expression", "$$ = $1.trim()"]
                ],
                cond: [
                    ["generic_expression", "$$ = $1.trim()"]
                ],
                actions: [
                    ["generic_expression", "$$ = $1.trim()"]
                ]
            }
        };
    };

    // utility functions used to flatten expressions
    function exprWithBinaryOp() {
        return " if (!Array.isArray($$)) { $$ = []; }" +
               " Array.isArray($1) ? $$.concat($1) : $$.push($1);" +
               " $$.push({" +
               "      type: 'binop'," +
               "      val:  $2.toUpperCase()" +
               " });" +
               " $$ = $$.concat($3)";
    }
    function exprWithUnaryOp() {
        return " if (!Array.isArray($$)) { $$ = []; }" +
               " $$.push({" +
               "      type: 'unaryop'," +
               "      val:  $1.toUpperCase()" +
               " });" +
               " $$ = $$.concat($2);";
    }
    function exprWithParenthesis() {
        return " if (!Array.isArray($$)) { $$ = []; }" +
               " $$.push({type: 'par', val: $1});" +
               " $$ = $$.concat($2);" +
               " $$.push({type: 'par', val: $3})";
    }
    function getFunctionRule(opt) {
        return "$$ = { type: 'function', val: [] }; " +
               "$$.val.push({ type: 'identifier', val: $IDENTIFIER }); " +
               "$$.val.push({type: 'par', val: $2}); " +
               "var i = 0;" +
               "for (i = 0; i < $args.length; i++) {" +
               "    $$.val = $$.val.concat($args[i].val);" +
               "    if (i < $args.length - 1) {" +
               "        $$.val.push({ type: 'separator', val: ',' });" +
               "    }" +
               "}" +
               "$$.val.push({type: 'par', val: $4}); ";
    }

    EmuchartsGrammar.prototype.grammar4conditions = function () {
        return {
            lex: {
                rules: [
                    ["\\s+", "/* skip whitespace */", "whitespace"],
                    ["\\b(?!(AND|and|OR|or|TRUE|true|FALSE|false|IMPLIES|implies|NOT|not)\\b)" + // filters out AND/and/OR/or/TRUE/true/FALSE/false/IMPLIES/implies/NOT/not
                        "([a-zA-Z][a-zA-Z0-9_]*)", "return 'IDENTIFIER'", "identifier"],
                    ["[0-9]+(?:\\.[0-9]+)?\\b", "return 'NUMBER'", "number"],
                    ['\\"([^\\"]+)\\"',         "return 'STRING'", "string"], // anything but double quotes
                    ["\\*",                     "return '*'"],
                    ["\\/",                     "return '/'"],
                    ["-",                       "return '-'"],
                    ["(?!(?:(\\!\\=|not[a-zA-Z0-9_]|NOT[a-zA-Z0-9_])))" + // filtering out != and identifiers starting with NOT/not
                     "(\\!|NOT|not)",           "return 'NOT'"],
                    ["\\+",                     "return '+'"],
                    ["(\\!\\=)",                "return '!='"],
                    ["(?!(?:(\\=\\>|\\=\\=)))" + // filtering out implication and equality
                     "(\\=)",                   "return '='"],
                    ["(\\=\\=)",                "return '=='"],
                    ["(?!(?:(\\>\\=)))" + // filtering out >=
                     "(\\>)",   "return '>'"],
                    ["(\\>\\=)",                "return '>='"],
                    ["(?!(?:(\\<\\=)))" + // filtering out <=
                     "(\\<)",   "return '<'"],
                    ["(\\<\\=)",                "return '<='"],
                    ["(?!(?:(implies[a-zA-Z0-9_]|IMPLIES[a-zA-Z0-9_])))" + // filtering out identifiers starting with IMPLIES/implies
                        "(IMPLIES|implies|(\\=\\>))", "return 'IMPLIES'"],
                    ["(?!(?:(and[a-zA-Z0-9_]|AND[a-zA-Z0-9_])))" + // filtering out identifiers starting with AND/and
                        "(AND|and|&&|&)",                "return 'AND'"],
                    ["(?!(?:(or[a-zA-Z0-9_]|OR[a-zA-Z0-9_])))" + // filtering out identifiers starting with OR/or
                        "(OR|or|\\|\\|)",             "return 'OR'"],
                    ["(?!(?:(true[a-zA-Z0-9_]|TRUE[a-zA-Z0-9_])))" + // filtering out identifiers starting with TRUE/true
                        "(TRUE|true)",             "return 'TRUE'"],
                    ["(?!(?:(false[a-zA-Z0-9_]|FALSE[a-zA-Z0-9_])))" + // filtering out identifiers starting with TRUE/true
                        "(FALSE|false)",           "return 'FALSE'"],
                    ["\\(",                     "return '('"],
                    ["\\)",                     "return ')'"],
                    ["\\[",                     "return '['"],
                    ["\\]",                     "return ']'"],
                    ["\\{",                     "return '{'"],
                    ["\\}",                     "return '}'"],
                    ["\\.",                     "return '.'"],
                    ['\\"',                     "return '\"'"]
                ]
            },
            operators: [
                ["left", "+", "-", "*", "/"], // left means left-to-right
                ["left", "==", "=", "!=", ">", "<", "<=", ">="], // = and == have the same meaning
                ["left", "IMPLIES", "AND", "OR"],
                ["right", ":="],
                ["right", ";", ","],
                ["right", "UMINUS", "NOT"] // unary negation, not
            ],
            start: "production",
            bnf: {
                production: [ ["cond", "return $1"] ],
                cond: [
                    ["e", "$$ = { type: 'bool_expr', val: $1 }"]
                ],
                e: [
                    ["NOT e", exprWithUnaryOp()],
                    ["( e )", exprWithParenthesis()],
                    ["e = e", exprWithBinaryOp()],  // comparison of equality of two terms
                    ["e == e", exprWithBinaryOp()],  // comparison of equality of two terms
                    ["e != e", exprWithBinaryOp()],  // comparison of inequality of two terms
                    ["e AND e", exprWithBinaryOp()],
                    ["e OR e", exprWithBinaryOp()],
                    ["e IMPLIES e", exprWithBinaryOp()],

                    ["e > e", exprWithBinaryOp()],
                    ["e >= e", exprWithBinaryOp()],
                    ["e < e", exprWithBinaryOp()],
                    ["e <= e", exprWithBinaryOp()],

                    ["e + e", exprWithBinaryOp()],
                    ["e - e", exprWithBinaryOp()],
                    ["e * e", exprWithBinaryOp()],
                    ["e / e", exprWithBinaryOp()],
                    ["- e", exprWithUnaryOp(), {"prec": "UMINUS"}],

                    ["term", "$$ = [$1]"]
                ],
                term: [
                    ["number", "$$ = $1"],
                    ["id", "$$ = $1"],
                    ["string", "$$ = $1"],
                    ["true_false", "$$ = $1"]
                ],
                number: [
                    ["NUMBER", "$$ = { type: 'number', val: $NUMBER }"]
                ],
                id: [
                    ["IDENTIFIER", "$$ = { type: 'identifier', val: $IDENTIFIER }"],
                    ["IDENTIFIER . id", "$$ = { type: 'identifier', val: $IDENTIFIER + '.' + $id.val }"],
                    ["IDENTIFIER ( args )", getFunctionRule() ]
                ],
                args: [
                    ["expression",        "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression ,",      "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression , args", "if (!Array.isArray($$)) { $$ = []; }; $$.push($1); $$ = $$.concat($3);"]
                ],
                string: [
                    ["STRING", "$$ = { type: 'string', val: $STRING }"]
                ],
                true_false: [
                    ["TRUE", "$$ = { type: 'constant', val: $TRUE }"],
                    ["FALSE", "$$ = { type: 'constant', val: $FALSE }"]
                ]
            }
        };
    };

    EmuchartsGrammar.prototype.grammar4actions = function () {
        return {
            lex: {
                rules: [
                    ["\\s+", "/* skip whitespace */", "whitespace"],
                    ["\\b(?!(AND|and|OR|or|TRUE|true|FALSE|false|IMPLIES|implies|NOT|not)\\b)" + // filters out AND/and/OR/or/TRUE/true/FALSE/false/IMPLIES/implies/NOT/not
                        "([a-zA-Z][a-zA-Z0-9_]*)", "return 'IDENTIFIER'", "identifier"],
                    ["[0-9]+(?:\\.[0-9]+)?\\b", "return 'NUMBER'", "number"],
                    ['\\"([^\\"]+)\\"',         "return 'STRING'", "string"], // anything but double quotes
                    ["\\*",                     "return '*'"],
                    ["\\/",                     "return '/'"],
                    ["-",                       "return '-'"],
                    ["(?!(?:(\\!\\=)))" + // filtering out !=
                     "(\\!|NOT|not)",           "return 'NOT'"],
                    ["\\+",                     "return '+'"],
                    ["(\\!\\=)",                "return '!='"],
                    ["(\\:\\=)",                "return ':='"],
                    ["(?!(?:(\\=\\>|\\=\\=|\\:\\=)))" + // filtering out implication, equality, and assignment
                     "(\\=)",                   "return '='"],
                    ["(\\=\\=)",                "return '=='"],
                    ["(?!(?:(\\>\\=)))" + // filtering out >=
                     "(\\>)",   "return '>'"],
                    ["(\\>\\=)",                "return '>='"],
                    ["(?!(?:(\\<\\=)))" + // filtering out <=
                     "(\\<)",   "return '<'"],
                    ["(\\<\\=)",                "return '<='"],
                    ["(?!(?:(implies[a-zA-Z0-9_]|IMPLIES[a-zA-Z0-9_])))" + // filtering out identifiers starting with IMPLIES/implies
                        "(IMPLIES|implies|(\\=\\>))", "return 'IMPLIES'"],
                    ["(?!(?:(and[a-zA-Z0-9_]|AND[a-zA-Z0-9_])))" + // filtering out identifiers starting with AND/and
                        "(AND|and|&&|&)",                "return 'AND'"],
                    ["(?!(?:(or[a-zA-Z0-9_]|OR[a-zA-Z0-9_])))" + // filtering out identifiers starting with OR/or
                        "(OR|or|\\|\\|)",             "return 'OR'"],
                    ["(?!(?:(true[a-zA-Z0-9_]|TRUE[a-zA-Z0-9_])))" + // filtering out identifiers starting with TRUE/true
                        "(TRUE|true)",             "return 'TRUE'"],
                    ["(?!(?:(false[a-zA-Z0-9_]|FALSE[a-zA-Z0-9_])))" + // filtering out identifiers starting with TRUE/true
                        "(FALSE|false)",           "return 'FALSE'"],
                    ["\\(",                     "return '('"],
                    ["\\)",                     "return ')'"],
                    ["\\[",                     "return '['"],
                    ["\\]",                     "return ']'"],
                    ["\\{",                     "return '{'"],
                    ["\\}",                     "return '}'"],
                    ["\\.",                     "return '.'"],
                    ['\\"',                     "return '\"'"],
                    [';',                       "return ';'"],
                    [',',                       "return ','"]
                ]
            },
            operators: [
                ["left", "+", "-", "*", "/"], // left means left-to-right
                ["left", "==", "=", "!=", ">", "<", "<=", ">="], // = and == have the same meaning
                ["left", "IMPLIES", "AND", "OR"],
                ["right", ":="],
                ["right", ";", ","],
                ["right", "UMINUS", "NOT"] // unary negation, not
            ],
            start: "production",
            bnf: {
                production: [ ["action", "return $1"] ],
                action: [
                    ["assignment_expr", "$$ = { type: 'assignment_expr', val: $1 }"]
                ],
                assignment_expr: [
                    ["a", "$$ = $1"],
                    ["a ;", "$$ = $1"],
                    ["( a )", "$$ = $a"]
                ],
                a: [
                    ["id := expression", "$$ = { type: 'assignment', val: { identifier: $1, binop: { type: 'binop', val: $2 }, expression: $3 }};"]
                ],
                expression:  [
                    ["e", "$$ = { type: 'expression', val: $e }"]
                ],
                e: [
                    ["NOT e", exprWithUnaryOp()],
                    ["( e )", exprWithParenthesis()],
                    ["e = e", exprWithBinaryOp()],  // comparison of equality of two terms
                    ["e == e", exprWithBinaryOp()],  // comparison of equality of two terms
                    ["e != e", exprWithBinaryOp()],  // comparison of inequality of two terms
                    ["e AND e", exprWithBinaryOp()],
                    ["e OR e", exprWithBinaryOp()],
                    ["e IMPLIES e", exprWithBinaryOp()],

                    ["e > e", exprWithBinaryOp()],
                    ["e >= e", exprWithBinaryOp()],
                    ["e < e", exprWithBinaryOp()],
                    ["e <= e", exprWithBinaryOp()],

                    ["e + e", exprWithBinaryOp()],
                    ["e - e", exprWithBinaryOp()],
                    ["e * e", exprWithBinaryOp()],
                    ["e / e", exprWithBinaryOp()],
                    ["- e", exprWithUnaryOp(), {"prec": "UMINUS"}],

                    ["term", "$$ = [$1]"]
                ],
                term: [
                    ["number", "$$ = $1"],
                    ["id", "$$ = $1"],
                    ["string", "$$ = $1"],
                    ["true_false", "$$ = $1"]
                ],
                number: [
                    ["NUMBER", "$$ = { type: 'number', val: $NUMBER }"]
                ],
                id: [
                    ["IDENTIFIER", "$$ = { type: 'identifier', val: $IDENTIFIER }"],
                    ["IDENTIFIER . id", "$$ = { type: 'identifier', val: $IDENTIFIER + '.' + $id.val }"],
                    ["IDENTIFIER ( args )", getFunctionRule() ]
                ],
                args: [
                    ["expression",        "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression ,",      "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression , args", "if (!Array.isArray($$)) { $$ = []; }; $$.push($1); $$ = $$.concat($3);"]
                ],
                string: [
                    ["STRING", "$$ = { type: 'string', val: $STRING }"]
                ],
                true_false: [
                    ["TRUE", "$$ = { type: 'constant', val: $TRUE }"],
                    ["FALSE", "$$ = { type: 'constant', val: $FALSE }"]
                ]
            }
        };
    };

    /**
     * @function getLexerRules
     * @memberof module:EmuchartsParser
     * @instance
     * @description Returns the lexer rules for Emucharts.
     * @returns Object{Array({ regex: (String), type: (String) })}
     *      Property regex specifies a regular expression.
     *      Property type describes the type of expression captured with the provided regex. The current set of classes are:
     *      "builtin", "keyword", "operator", "variable", "number", "whitespace". The value of this property can be used as a basis to apply styles to text editors like code mirror.
     */
    EmuchartsGrammar.prototype.getLexerRules = function () {
        var ans = { triggers: "", conditions: "" };
        this.grammar4triggers().forEach(function (r) {
            if (r.rule && r.rule.length > 0) {
                ans.triggers.push({ regex: new RegExp(r.rule[0]), type: (r.rule.length === 3) ? r.rule[2] : "builtin" });
            }
        });
        this.grammar4conditions().forEach(function (r) {
            if (r.rule && r.rule.length > 0) {
                ans.conditions.push({ regex: new RegExp(r.rule[0]), type: (r.rule.length === 3) ? r.rule[2] : "builtin" });
            }
        });
        this.grammar4actions().forEach(function (r) {
            if (r.rule && r.rule.length > 0) {
                ans.actions.push({ regex: new RegExp(r.rule[0]), type: (r.rule.length === 3) ? r.rule[2] : "builtin" });
            }
        });
        return ans;
    };


    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new EmuchartsGrammar();
            }
            return instance;
        }
    };
});
