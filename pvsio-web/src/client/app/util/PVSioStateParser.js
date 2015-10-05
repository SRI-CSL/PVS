/**
 * Parses pvsio output string into a json object of key-value pairs. All non-object values (i.e. values which are themselves key value pairs) are strings.
 * @author Patrick Oladimeji
 * @date 3/17/14 14:06:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, white:true */
/*global define */

define(function (require, exports, module) {
    "use strict";
    var groups = {
        "(#": "#)",
        "(": ")",
        "(:": ":)",
        "#)": "(#",
        ")": "(",
        ":)": "):"
    };

    /** matches a numeric character */
    function num(d) {
        return (/[0-9]/).test(d);
    }

    /** matches a space character */
    function space(s) {
        return (/\s/).test(s);
    }

    /** matches a word character */
    function wordChar(c) {
        return (/[\w]/).test(c);
    }

    /**
        Evaluates a numeric string represented as a fraction. If the string is not a fraction,
        it converts the string to a number. If the argument is not a string, then the argument is returned unchanged.
    */
    function evaluate(str) {
        if (typeof str === "string") {
            var args = str.split("/");
            if (args.length === 2 && !isNaN(+args[0]) && !isNaN(+args[1])) {
                return +args[0] / +args[1];
            }
            if (!isNaN(+str)) { return +str; }
        }
        return str;
    }

    /**
        Reads the given string from the specified start position until the specified condition returns false or
        the end of the string is reached.
        @param {string} str the string the read
        @param {Number} start the index of the string where reading should start
        @param {function<ch, prevCh>} condition a function to be called on each character read from the string. This function's second parameter is the previous character read. The function should return false to stop the read process.
        @return {string, number} An object representing the word consumed by the read process and the last index of the character read before the condition returned false or the end of the string was reached.
    */
    function readUntil(str, start, cond) {
        var i = start,
            word = "";
        for (i = start; i < str.length; i++) {
            if (!cond(str[i], str[i - 1])) {
                return {
                    word: word,
                    index: i
                };
            } else {
                word = word.concat(str[i]);
            }
        }
        return {
            word: word,
            index: i
        };
    }

    function isclosebr(s) {
        return s.indexOf(")") >= 0;
    }

    function isopenbr(s) {
        return s.indexOf("(") >= 0;
    }

    function wordBeforeEqual(ch, prev) {
        return (prev + ch) !== ":=";
    }
    /**
        Resolves the chain of properties represented as a dot-separated string on the state provided
        @param {object} state a JSON object representing the state returned by the PVS spec
        @param {string} property a string representing a property or a chain of properties to read from the state
        @return {object|string|number} the value at the specified property
    */
    function resolve(state, property) {
        var pChain = property.split(".");
        var obj = state, i = 0, key;

        while (i < pChain.length && obj) {
            key = pChain[i++];
            obj = obj[key];
        }
        return obj;
    }

    /**
        Parses a string value representing PVSio function output and returns a JSON object representing the same information.
        "(", and "(:" designate the beginning of a list and are thus converted to arrays. "(#" designate the beginning of a
        key-value pair
    */
    function parseValue(value) {
        var res;
        var token = readUntil(value, 0, space), br = "(", close, subValue, stack, subbr, index, val;
        if (value[token.index] === "(") {
            br = br.concat(value[token.index + 1]);
            close = groups[br];
            if (close) {
                stack = [];
                subValue = readUntil(value, token.index + 2, function (ch, prev) {
                    subbr = prev + ch;
                    if (!groups[subbr]) {
                        if (isopenbr(subbr)) {
                            subbr = "(";
                        } else if (isclosebr(subbr)) {
                            subbr = ")";
                        }
                    }
                    //check if the current br is open or close (if open push on stack if close compare to the top of the stack
                    if (isopenbr(subbr) && groups[subbr]) {
                        stack.push(groups[subbr]);
                    } else if (isclosebr(subbr) && subbr === stack[stack.length - 1]) {
                        stack.pop();
                    }
                    return !(stack.length === 0 && prev.concat(ch) === close);
                });
                subValue.index = subValue.index + 1;
                if (br === "(#") { //we are reading key-value pairs
                    index = 0;
                    res = {};
                    //while we can still find a comma after parsing a value,
                    //parse the key and recursively call parseValue to parse the value
                    do {
                        var key = readUntil(subValue.word, index + 1, wordBeforeEqual);
                        key.word = key.word.trim();
                        key.word = key.word.substr(0, key.word.length - 1); //removing spaces and last char ":"
                        key.index = key.index + 1;
                        val = parseValue(subValue.word.substring(key.index));
                        res[key.word] = val.value;
                        index = (val.index + key.index);
                    } while (subValue.word[index] === ",");

                    return {
                        value: res,
                        index: subValue.index
                    };
                } else if (br === "(:") { //we are reading a list
                    res = [];
                    index = 0;
                    do {
                        val = parseValue(subValue.word.substring(index));
                        res.push(val.value);
                        index += val.index;
                    } while (subValue.word[index++] === ",");
                    return {
                        value: res,
                        index: subValue.index
                    };
                }
            } else {
                //just an open bracket -- they usually signify a list of some sort
                subValue = readUntil(value, token.index + 1, function (ch, prev) {
                    return ch !== ")";
                });
                //parse the list as an array separated by commas and return it
                res = subValue.word.split(",").map(function (d) {
                    return d.trim();
                });
                return {
                    value: res,
                    index: subValue.index
                };
            }
        } else {
            token = readUntil(value, token.index, function (ch, prev) {
                return wordChar(ch, prev) || num(ch) || (/[\/\.\-\"\'\s\_\:]/).test(ch);
            });
            return {
                value: token.word,
                index: token.index
            };
        }
    }

    module.exports = {
        parse: function (state) {
            return parseValue(state).value;
        },
        evaluate: evaluate,
        resolve: resolve,
        isState: function (str) {
            if (Array.isArray(str)) {
                str = str.join("");
            }
            return str.trim().indexOf("(#") === 0;
        }
    };
});
