/**
 * @module EmuchartsParser2
 * @version 2.0
 * @description
 * EmuchartsParser2 is a parser for the Emucharts language.
 * The main API of the parser is function parseTransition(label); it can be used to parse
 * the label of an Emucharts transition.
 * The parser is dynamically generated from the Emuchart grammar using Jison http://zaach.github.io/
 * @author Paolo Masci
 * @date 06/10/14 2:24:01 PM
 *
 * @example
// Basic Emucharts printer module that uses EmuchartsParser2.
define(function (require, exports, module) {
    "use strict";

    var EmuchartsParser2 = require("plugins/emulink/EmuchartsParser2");
    var parser;

    function Printer(name) {
        parser = EmuchartsParser2.getInstance();
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
 *       of a token t is $t)
 */
define(function (require, exports, module) {
    "use strict";

    var Parser = require("lib/jison/jison");
    var grammar = require("plugins/emulink/parser/EmuchartsGrammar").getInstance();
    var dbg = true;
    function dbg_parser_rules (label, rules) {
        function check_rule () {
            var ans = true;
            var matched = false;
            rules.forEach(function (rule) {
                var x = label.match(rule);
                if (x) {
                    x.rule = "Using rule " + rule;
                    //console.log(x);
                    matched = true;
                    label = label.substring(x[0].length, label.length);
                    if(x[0].length) { ans = false; }
                }
            });
            if (!matched) {
                console.error("Unable to match " + label + " with lexer rules :((");
            }
            return ans;
        }
        var stop = false;
        while(label && label !== "" && !stop) {
            stop = check_rule();
        }
    }

    var instance;

    /**
     * @function EmuchartsParser2
     * @memberof module:EmuchartsParser2
     * @instance
     * @description Constructor
     * @returns Object{Object({getParserCode, getLexerRules, parseTransition, parseVariables})}
     */
    function EmuchartsParser2() {
        try {
            this.parser4triggers = new Parser(grammar.grammar4triggers());
            this.parser4conditions = new Parser(grammar.grammar4conditions());
            this.parser4actions = new Parser(grammar.grammar4actions());
            this.variables = [];
        } catch (e) {
            console.log(e);
        } finally {
            return this;
        }
    }

    EmuchartsParser2.prototype.setVariables = function (emuchart_variables) {
        this.variables = emuchart_variables;
    };

    /**
     * @function getParserCode
     * @memberof module:EmuchartsParser2
     * @instance
     * @description Returns the javascript code of the parser.
     * @returns {String}
     *      The javascript code of the parser automatically generated using the Jison library and the lexer rules for Emucharts.
     */
    EmuchartsParser2.prototype.getParserCode = function () {
        //var parser = new EmuchartsParser2();
        try {
            return this.parser4triggers.generate({moduleType: "js"});
        } catch (e) { console.log(e); }
    };

    function identifyVariables (_this, term) {
        function equalsName (name, variableName) {
            return name === variableName
                    || variableName.indexOf(name + ".") >= 0;
        }
        function isVariable (name) {
            if (_this.variables) {
                for (var i = 0; i < _this.variables.length; i++) {
                    if (equalsName(name, _this.variables[i].name)) {
                        return true;
                    }
                }
            }
            return false;
        }
        function getVariableType (name) {
            if (_this.variables) {
                for (var i = 0; i < _this.variables.length; i++) {
                    if (equalsName(name, _this.variables[i].name)) {
                        return _this.variables[i].type;
                    }
                }
            }
            return null;
        }
        if (term.type === "identifier") {
            term.isVariable = isVariable(term.val);
            if (term.isVariable) {
                term.variableType = getVariableType(term.val);
            }
        } else if (term.type === "function" && term.val.length > 1) {
            // term.val[0] is the function name
            for (var j = 1; j < term.val.length; j++) {
                if (term.val[j].type === "identifier") {
                    term.val[j].isVariable = isVariable(term.val[j].val);
                    if (term.val[j].isVariable) {
                        term.val[j].variableType = getVariableType(term.val[j].val);
                    }
                } else if (term.val[j].type === "function") {
                    identifyVariables(_this, term.val[j]);
                }
            }
        }
        return term;
    }

    /**
     * @function parseTrigger
     * @memberof module:EmuchartsParser2
     * @instance
     * @description Parses Emucharts trigger labels.
     * @param label {String}
     *    The label of an Emucharts transition.</br>
     *    Emucharts transition labels are strings in the form "name  [ cond ] { actions }", where:
     *      <li>"name" is the transition name, e.g., "click_up";</li>
     *      <li>"[ cond ]" is the transition condition, e.g., "[ display < 100 ]";</li>
     *      <li>"{ actions }" are the transition actions, e.g., "{ display := display + 1; }".</li>
     *    If multiple actions are specified, they are performed in sequence, e.g., "{ display := display + 1; cursor := 0; }"
     *    updates the display value and then sets the cursor position.
     *    The transition name is mandatory; transition conditions and transition actions are optional.
     */
    EmuchartsParser2.prototype.parseTrigger = function (label) {
        var ans = { err: null, res: null };
        if (typeof label === "string") {
            //-- dbg
            if (dbg) {
                console.log("Parsing trigger " + label);
                dbg_parser_rules(label, this.parser4triggers.lexer.rules);
            }
            //--
            try {
                ans.res = this.parser4triggers.parse(label);
            } catch (e) {
                ans.err = e.message;
                console.error(ans.err);
            }
        } else {
            ans.err = "Erroneous argument type for string label: " + (typeof label);
            console.error(ans.err);
        }
        return ans;
    };
    EmuchartsParser2.prototype.parseCondition = function (label) {
        var ans = { err: null, res: null };
        if (label) {
            if (typeof label === "string") {
                //-- dbg
                if (dbg) {
                    console.log("Parsing condition " + label);
                    dbg_parser_rules(label, this.parser4conditions.lexer.rules);
                }
                //--
                try {
                    ans.res = this.parser4conditions.parse(label);
                } catch (e) {
                    ans.err = e.message;
                    console.error(ans.err);
                }
            } else {
                ans.err = "Erroneous argument type for string label: " + (typeof label);
                console.error(ans.err);
            }
        }
        if (ans && ans.res && ans.res.val) {
            var _this = this;
            ans.res.val = ans.res.val.map(function (x) {
                return identifyVariables(_this, x);
            });
        }
        return ans;
    };
    EmuchartsParser2.prototype.parseAction = function (label) {
        var ans = { err: null, res: null };
        if (label) {
            if (typeof label === "string") {
                //-- dbg
                if (dbg) {
                    console.log("Parsing action " + label);
                    dbg_parser_rules(label, this.parser4actions.lexer.rules);
                }
                //--
                try {
                    ans.res = this.parser4actions.parse(label);
                } catch (e) {
                    ans.err = e.message;
                    console.error(ans.err);
                }
            } else {
                ans.err = "Erroneous argument type for string label: " + (typeof label);
                console.error(ans.err);
            }
        }
        if (ans && ans.res && ans.res.val) {
            var _this = this;
            ans.res.val.val.identifier = identifyVariables(_this, ans.res.val.val.identifier);
            ans.res.val.val.expression.val = ans.res.val.val.expression.val.map(function (x) {
                return identifyVariables(_this, x);
            });
        }
        return ans;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new EmuchartsParser2();
            }
            return instance;
        }
    };
});
