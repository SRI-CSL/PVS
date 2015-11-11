/**
 * @module EmuchartsParser
 * @version 0.3
 * @description
 * EmuchartsParser is a parser for the Emucharts language.
 * The main API of the parser is function parseTransition(label); it can be used to parse
 * the label of an Emucharts transition.
 * The parser is dynamically generated from the Emuchart grammar using Jison http://zaach.github.io/
 * @author Paolo Masci
 * @date 06/10/14 2:24:01 PM
 *
 * @example
// Basic Emucharts printer module that uses EmuchartsParser.
define(function (require, exports, module) {
    "use strict";

    var EmuchartsParser = require("plugins/emulink/EmuchartsParser");
    var parser;

    function Printer(name) {
        parser = new EmuchartsParser();
        return this;
    }

    Printer.prototype.print_transition = function (label) {
        if (!label || label === "") {
            return { err: "Unexpected label", res: null };
        }
        var ans = parser.parseTransition(label);
        if (ans.res) {
            var theTransition = {
                identifier: ans.res.val.identifier || { type: "identifier", val: "tick" },
                cond:       ans.res.val.cond || { type: "expression", val: [] },
                actions:    ans.res.val.actions || { type: "actions", val: [] }
            };
            // do something useful with the transition...
        }
        return ans;
    }
});
 *
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
 *       or a token t is $t)
 */
define(function (require, exports, module) {
    "use strict";

    var Parser = require("lib/jison/jison");

    var lexerRules = [
        { rule: ["\\s+",                    "/* skip whitespace */"], type: "whitespace" },
        { rule: ["(?!(?:IMPLIES|implies|AND|and|OR|or|NOT|not|TRUE|true|FALSE|false))"
                 + // keywords shall not be used as identifiers
                "([a-zA-Z][a-zA-Z0-9_]*)",  "return 'IDENTIFIER'"],   type: "variable" },
        { rule: ["[0-9]+(?:\\.[0-9]+)?\\b", "return 'NUMBER'"],       type: "number"  },
        { rule: ["\"(.*?)\"",               "return 'STRING'"],       type: "string"  },
        { rule: ["\\*",                     "return '*'"],            type: "builtin" },
        { rule: ["\\/",                     "return '/'"],            type: "builtin" },
        { rule: ["-",                       "return '-'"],            type: "builtin" },
        { rule: ["(?!(?:(\\!\\=)))" + // filtering out !=
                 "(\\!|NOT|not)",           "return 'NOT'"],          type: "builtin" },
        { rule: ["\\+",                     "return '+'"],            type: "builtin" },
        { rule: ["(\\!\\=)",                "return '!='"],           type: "builtin" },
        { rule: ["(?!(?:(\\=\\>|\\=\\=)))" + // filtering out implication and equality
                 "(\\=)",                   "return '='"],            type: "builtin" },
        { rule: ["(\\=\\=)",                "return '=='"],           type: "error"   }, // invalid operator
        { rule: ["(?!(?:(\\>\\=)))" + // filtering out >=
                "(\\>)",   "return '>'"],                             type: "builtin" },
        { rule: ["(\\>\\=)",                "return '>='"],           type: "builtin" },
        { rule: ["(?!(?:(\\<\\=)))" + // filtering out <=
                 "(\\<)",   "return '<'"],                            type: "builtin" },
        { rule: ["(\\<\\=)",                "return '<='"],           type: "builtin" },
        { rule: ["(IMPLIES|implies|(\\=\\>))", "return 'IMPLIES'"],   type: "builtin" },
        { rule: ["(AND|&&)",                "return 'AND'"],          type: "builtin" },
        { rule: ["(OR|\\|\\|)",             "return 'OR'"],           type: "builtin" },
        { rule: ["(TRUE|true)",             "return 'TRUE'"],         type: "builtin" },
        { rule: ["(FALSE|false)",           "return 'FALSE'"],        type: "builtin" },
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

        // The order of rules in expressionBNF reflects the precedence of the operators (lowest precedence is at the top)
        // The considered precedence and associativity is that of the C++ language
        // (see http://en.cppreference.com/w/cpp/language/operator_precedence)
        // NOTE: according to the PVS language reference, PVS uses a different associativity for logical operators
        //       wrt C (associativity of AND/OR is right-to-left, see page 46,
        //       http://pvs.csl.sri.com/doc/pvs-language-reference.pdf).
        //       This might be a typo in the PVS manual, as the implemented associativity seems to be that of C++
        //       (need to check with Sam Owre).
        // For the correctness of this parser, associatibity and precedence don't really matter,
        // because expressions are parsed into a flat sequence of tokens.
        // The parser of the formal tool that will process the formal model generated with an Emucharts printer
        // will take care of establishing the correct precendence and associativity of operators.
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
                ["left", "IMPLIES", "AND", "OR"],
                ["right", ":="],
                ["right", ";", ","],
                ["right", "UMINUS", "NOT"] // unary negation, not
            ],

            "start": "production",
            "bnf": {
                "production": [
                    ["transition",   "return $transition"]
                ],
                "transition": [
                    ["id [ cond ] { actions } ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: $cond, actions: $actions }}"],
                    ["id { actions } ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: null, actions: $actions }}"],
                    ["id [ cond ] ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: $cond, actions: null }}"],
                    ["id ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: $id, cond: null, actions: null }}"],
                    ["[ cond ] ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: { type: 'identifier', val: 'tick' }, cond: $cond, actions: null }}"],
                    ["{ actions } ",
                        "$$ = { type: 'transition', " +
                                "val: { identifier: { type: 'identifier', val: 'tick' }, cond: null, actions: $actions }}"]
                ],
                "cond": [
                    ["expression", "$$ = $expression"]
                ],
                "actions": [
                    ["a", "$$ = { type: 'actions', val: $a }"]
                ],
                "a": [
                    ["assignment",     "if (!Array.isArray($$)) { $$ = []; } $$.push($assignment);"],
                    ["assignment ;",   "if (!Array.isArray($$)) { $$ = []; } $$.push($assignment);"],
                    ["assignment ; a", "if (!Array.isArray($$)) { $$ = []; } $$.push($1); $$ = $$.concat($3);"]
                ],
                "assignment": [
                    ["id := expression", assignmentExpr()]
                ],
                "id": [
                    ["IDENTIFIER", "$$ = { type: 'identifier', val: $IDENTIFIER }"],
                    ["IDENTIFIER . id", "$$ = { type: 'identifier', val: $IDENTIFIER + '.' + $id.val }"],
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
                    ["STRING", "$$ = { type: 'constant', val: $STRING }"]
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
     * @function EmuchartsParser
     * @memberof module:EmuchartsParser
     * @instance
     * @description Constructor
     * @returns Object{Object({getParserCode, getLexerRules, parseTransition, parseVariables})}
     */
    function EmuchartsParser() {
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
     * @memberof module:EmuchartsParser
     * @instance
     * @description Returns the javascript code of the parser.
     * @returns {String}
     *      The javascript code of the parser automatically generated using the Jison library and the lexer rules for Emucharts.
     */
    EmuchartsParser.prototype.getParserCode = function () {
        //var parser = new EmuchartsParser();
        try {
            return this.parser.generate({moduleType: "js"});
        } catch (e) { console.log(e); }
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
    EmuchartsParser.prototype.getLexerRules = function () {
        var ans = [];
        lexerRules.forEach(function (r) {
            ans.push({ regex: new RegExp(r.rule[0]), type: r.type });
        });
        return ans;
    };

    /**
     * @function parseTransition
     * @memberof module:EmuchartsParser
     * @instance
     * @description Parses Emucharts transition labels.
     * @param label {String}
     *    The label of an Emucharts transition.</br>
     *    Emucharts transition labels are strings in the form "name  [ cond ] { actions }", where:
     *      <li>"name" is the transition name, e.g., "click_up";</li>
     *      <li>"[ cond ]" is the transition condition, e.g., "[ display < 100 ]";</li>
     *      <li>"{ actions }" are the transition actions, e.g., "{ display := display + 1; }".</li>
     *    If multiple actions are specified, they are performed in sequence, e.g., "{ display := display + 1; cursor := 0; }"
     *    updates the display value and then sets the cursor position.
     *    The transition name is mandatory; transition conditions and transition actions are optional.
     * @returns Object{Object({ res: Object, err: String })}
     * {<ul>
     *     res: Object,</br>
     *     err: String</ul>}</br>
     * <ul>
     * <li>Property err is either null or a string reporting a parser error.</li>
     * <li>Property res is a typed object representing the parsed transition elements.
     * The typed objects produced for the parsed transition elements are as follows:
     * <ul>
     * <li><h6>Transition</h6>
     * {<ul>
     *     type: "transition",</br>
     *     val: {<ul>
     *             identifier: (Object of type "identifier"),</br>
     *             cond: (Object of type "expression"),</br>
     *             actions: (Object of type "action")</ul>}</ul>}</li>
     * <li><h6>Condition</h6>
     * {<ul>
     *     type: "expression",</br>
     *     val: (Array of objects of type "identifier"|"number"|"binop"|"unaryop"|"function"|"par")</ul>}</li>
     * <li><h6>Expression</h6></br>
     * {<ul>
     *     type: "expression",</br>
     *     val: (Array of objects of type "identifier"|"number"|"binop"|"unaryop"|"function"|"par")</ul>}</li>
     * <li><h6>Action</h6>
     * {<ul>
     *     type: "action",</br>
     *     val: (Array of objects of type "assignment")</ul>}</li>
     * <li><h6>Assignment</h6>
     * {<ul>
     *     type: "assignment",</br>
     *     val: {<ul>
     *            identifier: (Object of type "identifier"),</br>
     *            binop: (Object of type "binop"),</br>
     *            expression: (Object of type "expression")</ul>}</ul>}</li>
     * <li><h6>Identifier</h6>
     * {<ul>
     *     type: "identifier",</br>
     *     val: {<ul>
     *            type: "identifier",</br>
     *            val: (String)</ul>}</ul>}</li>
     * <li><h6>Binary operator</h6>
     * {<ul>
     *     type: "binop",</br>
     *     val: (String)</ul>}</li>
     * <li><h6>Unary operator</h6>
     * {<ul>
     *     type: "unaryop",</br>
     *     val: (String)</ul>}</li>
     * <li><h6>Number</h6>
     * {<ul>
     *     type: "number",</br>
     *     val: (String)</ul>}</li>
     * <li><h6>Function</h6>
     * {<ul>
     *     type: "function",</br>
     *     val: (Array of objects of type "identifier"|"number"|"binop"|"unaryop"|"function"|"par").
     *           <em>Note: the first array element is always an identifier, and it represents the function name.</em>)</ul>}</li>
     * <li><h6>Parenthesis</h6>
     * {<ul>
     *     type: "par",</br>
     *     val: (String)</ul>}</li></ul></ul>
     *
     * @example
var label = "inc [ display > display + 100 ] { display := display + s }";
var ans = parser.parseTransition(label);
if (ans.res) {
    console.log(JSON.stringify(ans.res, null, 2));
    // the console output will be
    //{
    //  "type": "transition",
    //  "val": {
    //    "identifier": {
    //      "type": "identifier",
    //      "val": "inc"
    //    },
    //    "cond": {
    //      "type": "expression",
    //      "val": [
    //        {
    //          "type": "identifier",
    //          "val": "display"
    //        },
    //        {
    //          "type": "binop",
    //          "val": ">"
    //        },
    //        {
    //          "type": "identifier",
    //          "val": "display"
    //        },
    //        {
    //          "type": "binop",
    //          "val": "+"
    //        },
    //        {
    //          "type": "number",
    //          "val": "100"
    //        }
    //      ]
    //    },
    //    "actions": {
    //      "type": "actions",
    //      "val": [
    //        {
    //          "type": "assignment",
    //          "val": {
    //            "identifier": {
    //              "type": "identifier",
    //              "val": "display"
    //            },
    //            "binop": {
    //              "type": "binop",
    //              "val": ":="
    //            },
    //            "expression": {
    //              "type": "expression",
    //              "val": [
    //                {
    //                  "type": "identifier",
    //                  "val": "display"
    //                },
    //                {
    //                  "type": "binop",
    //                  "val": "+"
    //                },
    //                {
    //                  "type": "identifier",
    //                  "val": "s"
    //                }
    //              ]
    //            }
    //          }
    //        }
    //      ]
    //    }
    //  }
    //}
}

     */
    EmuchartsParser.prototype.parseTransition = function (label) {
        label = (label === "" || label.trim().indexOf("[") === 0 || label.trim().indexOf("{") === 0) ? 
            ("tick " + label) : label;
        console.log("Parsing transition " + label);
        var ans = { err: null, res: null };
        try {
            ans.res = this.parser.parse(label);
        } catch (e) {
            ans.err = e.message;
        }
        return ans;
    };


    /**
     * @function parseVariables
     * @memberof module:EmuchartsParser
     * @instance
     * @description
     *    Parses an array of Emuchart variables and creates a typed object.
     *
     * @param {Array(Object)} variables Array of emuchart variables to be parsed.
     *            Each emuchart variable is a structured object
     *            { name: (String), type: (String), value: (String) }.
     *            Property name is mandatory, whilst the others are optional.
     *
     * @returns Object{Object({ res: Object, err: String })}
     * {<ul>
     *     res: Object,</br>
     *     err: String</ul>}</br>
     * <ul>
     * <li>Property err is either null or a string reporting a parser error.</li>
     * <li>Property res is a typed object representing the parsed variables.
     * The typed objects produced by the function are:
     * <ul>
     * <li><h6>Selector</h6>
     * {<ul>
     *     type: "selector",</br>
     *     children: (Array of typed objects)</ul>}</li>
     * <li><h6>Variable</h6>
     * {<ul>
     *     type: "variable",</br>
     *     val: {<ul>
     *         name: (String),</br>
     *         type: (String),</br>
     *         value: (String)</ul>}</ul>}</ul></li>
     *
     * @memberof module:EmuchartsParser
     *
     * @example
var variables = [ { name: 'pump.device1', type: 'A', value: 'initA' },
                  { name: 'pump.device2', type: 'B', value: 'initB' } ];
var ans = parser.parseVariables(variables);
if (ans.res) {
    console.log(JSON.stringify(ans.res, null, 2));
    // the console output will be
    //{
    //  pump: {
    //    type: selector,
    //    children: {
    //      device1: {
    //        type: variable,
    //        val: {
    //          name: device1,
    //          type: A,
    //          value: initA
    //        }
    //      },
    //      device2: {
    //        type: variable,
    //        val: {
    //          name: device2,
    //          type: B,
    //          value: initB
    //        }
    //      }
    //    }
    //  }
    //}
}
     */
    EmuchartsParser.prototype.parseVariables = function (variables, opt) {
        function extend(ans, v) {
            var path = v.name.split(".");
            var i = 0, tmp = ans.res;
            for (i = 0; i < path.length; i++) {
                if (i < path.length - 1) {
                    if (tmp[path[i]] && tmp[path[i]].type === "variable") {
                        if (opt.onNameConflict) {
                            opt.onNameConflict(path[i]);
                            ans.err = "Name conflict for " + path[i];
                            return;
                        }
                    } else {
                        if (!tmp[path[i]]) {
                            tmp[path[i]] = {
                                type: "selector",
                                children: { }
                            };
                        }
                        tmp = tmp[path[i]].children;
                    }
                } else {
                    if (tmp[path[i]] && tmp[path[i]].type === "selector") {
                        if (opt.onNameConflict) {
                            opt.onNameConflict(path[i]);
                            ans.err = "Name conflict for " + path[i];
                            return;
                        }
                    } else {
                        tmp[path[i]] = {
                            type: "variable",
                            val: {
                                name : path[i],
                                type : v.type,
                                value: v.value
                            }
                        };
                    }
                }
            }
        }
        var ans = { err: null, res: {} };
        if (variables) {
            variables.forEach(function (variable) {
                console.log("Parsing variable " + JSON.stringify(variable));
                extend(ans, {
                    name : variable.name,
                    type : variable.type,
                    value: variable.value
                });
            });
        }

        return ans;
    };

    module.exports = EmuchartsParser;
});
