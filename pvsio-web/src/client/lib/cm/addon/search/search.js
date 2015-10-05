// Refactored by Paolo Masci for PVSio-web Emucharts 

// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// Define search commands. Depends on dialog.js or another
// implementation of the openDialog method.

// Replace works a little oddly -- it will do the replace on the next
// Ctrl-G (or whatever is bound to findNext) press. You prevent a
// replace by making sure the match is no longer selected when hitting
// Ctrl-G.

/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global exports, module, require, define, CodeMirror, dialog */

(function (mod) {
    "use strict";
    if (typeof exports === "object" && typeof module === "object") {// CommonJS
        mod(require("../../lib/codemirror"), require("./searchcursor"), require("../dialog/dialog"));
    } else if (typeof define === "function" && define.amd) {// AMD
        define(["../../lib/codemirror", "./searchcursor", "../dialog/dialog"], mod);
    } else {// Plain browser env
        mod(CodeMirror);
    }
}(function (CodeMirror) {
    "use strict";
    function searchOverlay(query, caseInsensitive) {
        if (typeof query === "string") {
            query = new RegExp(query.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&"), caseInsensitive ? "gi" : "g");
        } else if (!query.global) {
            query = new RegExp(query.source, query.ignoreCase ? "gi" : "g");
        }

        return {
            token: function (stream) {
                query.lastIndex = stream.pos;
                var match = query.exec(stream.string);
                if (match && match.index === stream.pos) {
                    stream.pos += match[0].length;
                    return "searching";
                } else if (match) {
                    stream.pos = match.index;
                } else {
                    stream.skipToEnd();
                }
            }
        };
    }

    var queryDialog = 'Search: <input type="text" style="width: 80%" class="CodeMirror-search-field" placeholder="type text or regular expression here..."/>';
    var replaceQueryDialog = 'Replace: <input type="text" style="width: 80%" class="CodeMirror-search-field" placeholder="type text or regular expression here..."/>';
    var replacementQueryDialog = 'With: <input type="text" style="width: 10em" class="CodeMirror-search-field"/>';
    var doReplaceConfirm = "Replace? <button>Yes</button> <button>No</button> <button>Stop</button>";
    
    function SearchState() {
        this.posFrom = this.posTo = this.query = null;
        this.overlay = null;
    }
    function getSearchState(cm) {
        cm.state.search = cm.state.search || new SearchState();
        return cm.state.search;
    }
    function queryCaseInsensitive(query) {
        return typeof query === "string" && query === query.toLowerCase();
    }
    function getSearchCursor(cm, query, pos) {
        // Heuristic: if the query string is all lowercase, do a case insensitive search.
        return cm.getSearchCursor(query, pos, queryCaseInsensitive(query));
    }
    function findNext(cm, rev) {
        cm.operation(
            function () {
                var state = getSearchState(cm),
                    cursor = getSearchCursor(cm, state.query, rev ? state.posFrom : state.posTo);
                if (!cursor.find(rev)) {
                    cursor = getSearchCursor(cm,
                                             state.query,
                                             rev ? CodeMirror.Pos(cm.lastLine()) : CodeMirror.Pos(cm.firstLine(), 0));
                    if (!cursor.find(rev)) { return; }
                }
                cm.setSelection(cursor.from(), cursor.to());
                cm.scrollIntoView({from: cursor.from(), to: cursor.to()});
                state.posFrom = cursor.from();
                state.posTo = cursor.to();
            }
        );
    }
    function parseQuery(query) {
        var isRE = query.match(/^\/(.*)\/([a-z]*)$/);
        if (isRE) {
            query = new RegExp(isRE[1], isRE[2].indexOf("i") === -1 ? "" : "i");
            if (query.test("")) {
                query = /x^/;
            }
        } else if (query === "") {
            query = /x^/;
        }
        return query;
    }
    var searchFunction = function (cm, rev) {
        var state = getSearchState(cm);
        return function (query) {
            cm.operation(function () {
                if (!query || state.query) { return; }
                state.query = parseQuery(query);
                cm.removeOverlay(state.overlay, queryCaseInsensitive(state.query));
                state.overlay = searchOverlay(state.query, queryCaseInsensitive(state.query));
                cm.addOverlay(state.overlay);
                state.posFrom = state.posTo = cm.getCursor();
                findNext(cm, rev);
            });
        };
    };
    function doSearch(cm, rev) {
        var state = getSearchState(cm);
        if (state.query) {
            return findNext(cm, rev);
        }
        dialog(cm, queryDialog, "Search for:", cm.getSelection(), searchFunction(cm, rev));
    }
    function clearSearch(cm) {
        cm.operation(function () {
            var state = getSearchState(cm);
            if (!state.query) { return; }
            state.query = null;
            cm.removeOverlay(state.overlay);
        });
        cm.setSelection(cm.getCursor(), cm.getCursor()); // clear the selection
    }
    function dialog(cm, text, shortText, deflt, f) {
        if (cm.fromInputArea) {
            cm.fromInputArea(
                document.getElementById("model-editor-search-input"),
                text,
                f,
                { value: deflt,
                    bottom: true,
//                    onInput: function (e, query, close) {
//                        clearSearch(cm);
//                        searchFunction(cm, false)(query);
//                    },
                    closeOnBlur: false }
            );
        } else if (cm.openDialog) {
            cm.openDialog(text, f, {
                value: deflt,
                bottom: true,
//                onInput: function (e, query, close) {
//                    clearSearch(cm);
//                    searchFunction(cm, false)(query);
//                },
                closeOnBlur: false
            });
        } else {
            f(prompt(shortText, deflt));
        }
    }
    function confirmDialog(cm, text, shortText, fs) {
        if (cm.openConfirm) {
            cm.openConfirm(text, fs);
        } else if (confirm(shortText)) {
            fs[0]();
        }
    }
    function replace(cm, all) {
        if (cm.getOption("readOnly")) { return; }
        dialog(cm, replaceQueryDialog, "Replace:", cm.getSelection(),
               function (query) {
                if (!query) { return; }
                query = parseQuery(query);
                dialog(cm, replacementQueryDialog, "Replace with:", "",
                       function (text) {
                        if (all) {
                            cm.operation(function () {
                                var cursor = getSearchCursor(cm, query);
                                while (cursor.findNext()) {
                                    if (typeof query !== "string") {
                                        var match = cm.getRange(cursor.from(), cursor.to()).match(query);
                                        cursor.replace(text.replace(/\$(\d)/g, function (_, i) {
                                            return match[i];
                                        }));
                                    } else { cursor.replace(text); }
                                }
                            });
                        } else {
                            clearSearch(cm);
                            var cursor = getSearchCursor(cm, query, cm.getCursor());
                            var advance;
                            var doReplace = function (match) {
                                cursor.replace(
                                    typeof query === "string" ? text
                                        : text.replace(/\$(\d)/g, function (_, i) { return match[i]; })
                                );
                                advance();
                            };
                            advance = function () {
                                var start = cursor.from(), match;
                                if (!(match = cursor.findNext())) {
                                    cursor = getSearchCursor(cm, query);
                                    if (!(match = cursor.findNext()) ||
                                            (start && cursor.from().line === start.line &&
                                                cursor.from().ch === start.ch)) {
                                        return;
                                    }
                                }
                                cm.setSelection(cursor.from(), cursor.to());
                                cm.scrollIntoView({from: cursor.from(), to: cursor.to()});
                                confirmDialog(cm, doReplaceConfirm, "Replace?",
                                              [ function () { doReplace(match); }, advance ]);
                            };
                            advance();
                        }
                    });
            });
    }

    CodeMirror.commands.find = function (cm) {
        if (cm.options.search) {
            if (cm.options.search === "") {
                cm.setSelection(cm.getCursor(), cm.getCursor()); // clear the selection
                return;
            } else {
                cm.state.search = cm.state.search || new SearchState();
                cm.state.search.query = cm.options.search;
            }
        } else {
            clearSearch(cm);
        }
        doSearch(cm);
    };
    CodeMirror.commands.findNext = doSearch;
    CodeMirror.commands.findPrev = function (cm) {
        doSearch(cm, true);
    };
    CodeMirror.commands.clearSearch = clearSearch;
    CodeMirror.commands.replace = replace;
    CodeMirror.commands.replaceAll = function (cm) {
        replace(cm, true);
    };
}));