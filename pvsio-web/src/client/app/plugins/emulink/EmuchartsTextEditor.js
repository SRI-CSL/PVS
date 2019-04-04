/** @module EmuchartsTextEditor */
/**
 * EmuchartsTextEditor is a text editor for editing labels of emuchart diagrams
 * @author Paolo Masci
 * @date 15/11/14 4:05:22 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var CodeMirror          = require("cm/lib/codemirror");

    require("cm/addon/fold/foldcode");
    require("cm/addon/fold/foldgutter");
    require("cm/addon/fold/indentFold");
    require("cm/addon/hint/show-hint");
    require("cm/addon/hint/emucharts-hint");
    require("cm/addon/edit/closebrackets");
    require("cm/addon/edit/matchbrackets");
    require("cm/addon/selection/active-line");
    require("cm/addon/display/placeholder");
    require("cm/addon/dialog/dialog");
    require("cm/addon/search/searchcursor");
    require("cm/addon/search/search");
    require("cm/mode/emucharts/emucharts");

    var options = {
        mode: "emucharts",
        lineNumbers: true,
        lineWapping: true,
        foldGutter: true,
        autofocus: false,
        gutters: ["CodeMirror-linenumbers", "CodeMirror-foldgutter"],
        autoCloseBrackets: false,
        matchBrackets: true,
        styleActiveLine: false,
        placeholder: "Type your Emucharts code here...",
        extraKeys: {
            "Ctrl-Space": "autocomplete"
        }
    };

    var editor;

    /**
     * @par obj is a structure { textArea, size }, where textArea is a DOM object of type textArea,
     *          and size is a structure { width, height } specifying the desired size of the editor
     */
    function EmuchartsTextEditor(obj) {
        editor = new CodeMirror.fromTextArea(obj.textArea, options);
        var showHint = true;
        editor.on("inputRead", function (cm) {
            if (showHint) {
                CodeMirror.showHint(cm, CodeMirror.hint.emucharts, { completeSingle: false, alignWithWord: true });
            }
        });
        editor.on("keyup", function (cm, event) {
            // update the original text area every time
            cm.save();
            // show hints only when typing words
            var keyCode = event.keyCode || event.which;
            if (keyCode < 65 || keyCode > 90) { // 65 = a, 90 = z
                showHint = false;
            } else {
                showHint = true;
            }
        });
        editor.setSize(obj.size.width, obj.size.height);
        editor.refresh();
    }

    module.exports = EmuchartsTextEditor;
});
