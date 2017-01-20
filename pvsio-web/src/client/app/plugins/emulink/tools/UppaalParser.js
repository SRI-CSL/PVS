/**
 * @module UppaalParser
 * @version 0.1
 * @description
 * UppaalParser is a parser for the Uppaal language.
 * The main API of this implementation is parseDeclaration(label), for parsing Uppaal declarations
 * The parser is dynamically generated from the Emuchart grammar using Jison http://zaach.github.io/
 * @author Paolo Masci
 * @date Nov 1, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */

/**
 *
 * Usage notes about Jison
 *   lex.rules defines the alphabet, i.e., the symbols known to the parser
 *   $$ is the Jison variable of type Array that needs to be used in the parser to collect information
 *       about the parsed tokens
 *   $1, $2, $3, etc... can be used in the production rules to obtain the value of the tokens specified in the rules
 *       alternatively, the name of the token preceded by $ can be used for the same purpose (i.e., the value
 *       of a token t is $t)
 */
define(function (require, exports, module) {
    "use strict";

    var Parser = require("lib/jison/jison");

    var lexerRules = [
        { rule: ["\\s+",                    "/* skip whitespace */"], type: "whitespace" },
        { rule: ["(?!(?:imply|and|or|not|true|false|const|typedef))"
                 + // keywords shall not be used as identifiers
                "([a-zA-Z_]([a-zA-Z0-9_])*)",  "return 'IDENTIFIER'"],   type: "variable" },
        { rule: ["[0-9]+(?:\\.[0-9]+)?\\b", "return 'NUMBER'"],       type: "number"  },
        { rule: ["\"(.*?)\"",               "return 'STRING'"],       type: "string"  },
        { rule: ["\\*",                     "return '*'"],            type: "builtin" },
        { rule: ["\\/",                     "return '/'"],            type: "builtin" },
        { rule: ["-",                       "return '-'"],            type: "builtin" },
        { rule: ["(?!(?:(\\!\\=)))" + // filtering out !=
                 "(\\!|NOT|not)",         "return 'NOT'"],            type: "builtin" },
        { rule: ["\\+",                     "return '+'"],            type: "builtin" },
        { rule: ["(\\!\\=)",               "return '!='"],            type: "builtin" },
        { rule: ["(?!(?:(\\=\\>|\\=\\=)))" + // filtering out implication and equality
                 "(\\=)",                   "return '='"],            type: "builtin" },
        { rule: ["(\\=\\=)",               "return '=='"],            type: "error"   }, // invalid operator
        { rule: ["(?!(?:(\\>\\=)))" + // filtering out >=
                "(\\>)",   "return '>'"],                             type: "builtin" },
        { rule: ["(\\>\\=)",               "return '>='"],            type: "builtin" },
        { rule: ["(?!(?:(\\<\\=)))" + // filtering out <=
                 "(\\<)",   "return '<'"],                            type: "builtin" },
        { rule: ["(\\<\\=)",               "return '<='"],            type: "builtin" },
        { rule: ["(imply|(\\=\\>))",    "return 'imply'"],            type: "builtin" },
        { rule: ["(and|&&)",              "return 'AND'"],            type: "builtin" },
        { rule: ["(or|\\|\\|)",            "return 'OR'"],            type: "builtin" },
        { rule: ["(true)",               "return 'TRUE'"],            type: "builtin" },
        { rule: ["(false)",             "return 'FALSE'"],            type: "builtin" },
        { rule: ["(const)",             "return 'const'"],            type: "builtin" },
        { rule: ["(typedef)",             "return 'typedef'"],            type: "builtin" },
        { rule: ["\\(",                     "return '('"],            type: "builtin" },
        { rule: ["\\)",                     "return ')'"],            type: "builtin" },
        { rule: ["\\[",                     "return '['"],            type: "builtin" },
        { rule: ["\\]",                     "return ']'"],            type: "builtin" },
        { rule: ["\\{",                     "return '{'"],            type: "builtin" },
        { rule: ["\\}",                     "return '}'"],            type: "builtin" },
        { rule: [":=",                      "return ':='"],           type: "builtin" },
        { rule: [";",                       "return ';'"],            type: "builtin" },
        { rule: [",",                       "return ','"],            type: "builtin" },
        { rule: [".",                       "return '.'"],            type: "builtin" },
        { rule: ['"',                       "return '\"'"],           type: "builtin" }
    ];

    var lexerRule2Array = function () {
        var ans = [];
        lexerRules.forEach(function (lexerRule) {
            ans.push(lexerRule.rule);
        });
        return ans;
    };

    var grammar = function (opt) {
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
        function assignmentExpr() {
            return " $$ = {" +
                   "    type: 'assignment'," +
                   "    val: {" +
                   "       identifier: $1," +
                   "       binop: { type: 'binop', val: $2 }," +
                   "       expression: $3 " +
                   "    }" +
                   " };";
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

        function expressionBNF(mode) {
            return [
                ["e + e",       exprWithBinaryOp()],
                ["e - e",       exprWithBinaryOp()],
                ["e * e",       exprWithBinaryOp()],
                ["e / e",       exprWithBinaryOp()],
                ["- e",            exprWithUnaryOp(), {"prec": "UMINUS"}],
                ["NOT e",          exprWithUnaryOp()],
                ["( e )",          exprWithParenthesis()],
                ["e = e",       exprWithBinaryOp()],  // comparison of equality of two terms
                ["e != e",      exprWithBinaryOp()],  // comparison of inequality of two terms
                ["e > e",       exprWithBinaryOp()],
                ["e >= e",      exprWithBinaryOp()],
                ["e < e",       exprWithBinaryOp()],
                ["e <= e",      exprWithBinaryOp()],
                ["e AND e",     exprWithBinaryOp()],
                ["e OR e",      exprWithBinaryOp()],
                ["e IMPLIES e", exprWithBinaryOp()],
                ["term",           "$$ = [$term]"]
            ];
        }

        return {
            "lex": {
                "rules": lexerRule2Array()
            },

            // the first field specified the associativity of the operator
            "operators": [
                ["left", "+", "-", "*", "/"], // left means left-to-right
                ["left", "=", "!=", ">", "<", "<=", ">="],
                ["left", "IMPLIES", "AND", "OR", "MOD"],
                ["right", ":="],
                ["right", ";", ","],
                ["right", "UMINUS", "NOT"] // unary negation, not
            ],

            "start": "production",
            "bnf": {
                "production": [
                    ["declaration",   "return $declaration"]
                ],
                "declaration": [
                    ["type variableID ;",
                        "$$ = { type: 'VariableDeclaration', " +
                                "val: { type: $type, variableID: $variableID }}"],
                    ["type variableID = expression ;",
                        "$$ = { type: 'TypeDeclaration', " +
                                "val: { type: $type, variableID: $variableID, val: $expression }}"],
                    ["typedef type id ;",
                        "$$ = { type: 'TypeDeclaration', " +
                                "val: { type: $type, id: $id }}"]
                ],
                "type": [
                    ["prefix typeID", "$$ = { prefix: $prefix, typeID: $typeID }"],
                    ["typeID", "$$ = { typeID: $typeID }"]
                ],
                "prefix": [
                    ["const", "$$ = $const"]
                ],
                "typeID": [
                    ["IDENTIFIER range", "$$ = { type: 'typeID', val: $IDENTIFIER, range: $range }"],
                    ["IDENTIFIER", "$$ = { type: 'typeID', val: $IDENTIFIER }"]
                ],
                "range": [
                    ["[ NUMBER , NUMBER ]", "$$ = [$2,$4]"]
                ],
                "variableID": [
                    ["id", "$$ = $id"]
                ],
                "assignment": [
                    ["id := expression", assignmentExpr()]
                ],
                "id": [
                    ["IDENTIFIER", "$$ = { type: 'VariableID', val: $IDENTIFIER }"],
                    ["IDENTIFIER . id", "$$ = { type: 'VariableID', val: $IDENTIFIER + '.' + $id.val }"],
                    ["IDENTIFIER ( args )", getFunctionRule(opt) ]
                ],
                "args": [
                    ["expression",        "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression ,",      "if (!Array.isArray($$)) { $$ = []; } $$.push($1);"],
                    ["expression , args", "if (!Array.isArray($$)) { $$ = []; }; $$.push($1); $$ = $$.concat($3);"]
                ],
                "number": [
                    ["NUMBER", "$$ = { type: 'number', val: $NUMBER }"]
                ],
                "string": [
                    ["STRING", "$$ = { type: 'string', val: $STRING }"]
                ],
                "true_false": [
                    ["TRUE", "$$ = { type: 'constant', val: $TRUE }"],
                    ["FALSE", "$$ = { type: 'constant', val: $FALSE }"]
                ],
                "term": [
                    ["number", "$$ = $number"],
                    ["id", "$$ = $id"],
                    ["string", "$$ = $string"],
                    ["true_false", "$$ = $true_false"]
                ],
                "expression":  [
                    ["e",   "$$ = { type: 'expression', val: $e }"]
                ],
                "e": expressionBNF()
            }
        };
    };

    /**
     * @function UppaalParser
     * @memberof module:UppaalParser
     * @instance
     * @description Constructor
     * @returns Object{Object({getParserCode, getLexerRules, parseTransition, parseVariables})}
     */
    function UppaalParser() {
        try {
            this.parser = new Parser(grammar());
        } catch (e) {
            console.log(e);
        } finally {
            return this;
        }
    }

    /**
     * @function getParserCode
     * @memberof module:UppaalParser
     * @instance
     * @description Returns the javascript code of the parser.
     * @returns {String}
     *      The javascript code of the parser automatically generated using the Jison library and the lexer rules for Emucharts.
     */
    UppaalParser.prototype.getParserCode = function () {
        //var parser = new UppaalParser();
        try {
            return this.parser.generate({moduleType: "js"});
        } catch (e) { console.log(e); }
    };

    /**
     * @function getLexerRules
     * @memberof module:UppaalParser
     * @instance
     * @description Returns the lexer rules for Emucharts.
     * @returns Object{Array({ regex: (String), type: (String) })}
     *      Property regex specifies a regular expression.
     *      Property type describes the type of expression captured with the provided regex. The current set of classes are:
     *      "builtin", "keyword", "operator", "variable", "number", "whitespace". The value of this property can be used as a basis to apply styles to text editors like code mirror.
     */
    UppaalParser.prototype.getLexerRules = function () {
        var ans = [];
        lexerRules.forEach(function (r) {
            ans.push({ regex: new RegExp(r.rule[0]), type: r.type });
        });
        return ans;
    };

    UppaalParser.prototype.parseDeclaration = function (label) {
        console.log("Parsing declaration " + label);
        var ans = { err: null, res: null };
        try {
            ans.res = this.parser.parse(label);
        } catch (e) {
            ans.err = e.message;
        }
        return ans;
    };

    module.exports = UppaalParser;
});
