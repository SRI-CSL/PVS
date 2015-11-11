// Refactored by Paolo Masci for PVSio-web Emucharts 

// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

// Open simple dialogs on top of an editor. Relies on dialog.css.

/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global exports, module, require, define, CodeMirror, dialog */

(function (mod) {
    "use strict";
    if (typeof exports === "object" && typeof module === "object") {// CommonJS
        mod(require("../../lib/codemirror"));
    } else if (typeof define === "function" && define.amd) {// AMD
        define(["../../lib/codemirror"], mod);
    } else {// Plain browser env
        mod(CodeMirror);
    }
}(function (CodeMirror) {
    "use strict";
    function dialogDiv(cm, template, bottom) {
        var wrap = cm.getWrapperElement();
        var dialog = wrap.appendChild(document.createElement("div"));
        if (bottom) {
            dialog.className = "CodeMirror-dialog CodeMirror-dialog-bottom";
        } else {
            dialog.className = "CodeMirror-dialog CodeMirror-dialog-top";
        }

        if (typeof template === "string") {
            dialog.innerHTML = template;
        } else { // Assuming it's a detached DOM element.
            dialog.appendChild(template);
        }
        return dialog;
    }

    function closeNotification(cm, newVal) {
        if (cm.state.currentNotificationClose) {
            cm.state.currentNotificationClose();
        }
        cm.state.currentNotificationClose = newVal;
    }

    CodeMirror.defineExtension("openDialog", function (template, callback, options) {
        options = options || {};
        closeNotification(this, null);

        var dialog = dialogDiv(this, template, options.bottom);
        var closed = false, me = this;
        var inp = dialog.getElementsByTagName("input")[0], button;
        
        function close(newVal) {
            if (typeof newVal === 'string') {
                inp.value = newVal;
            } else {
                if (closed) { return; }
                closed = true;
                dialog.parentNode.removeChild(dialog);
                me.focus();
                if (options.onClose) {
                    options.onClose(dialog);
                }
            }
        }
        
        if (inp) {
            if (options.value) {
                inp.value = options.value;
                inp.select();
            }
            if (options.onInput) {
                CodeMirror.on(inp, "input", function (e) {
                    options.onInput(e, inp.value, close);
                });
            }
            if (options.onKeyUp) {
                CodeMirror.on(inp, "keyup", function (e) {
                    options.onKeyUp(e, inp.value, close);
                });
            }

            CodeMirror.on(inp, "keydown", function (e) {
                if (options && options.onKeyDown &&
                        options.onKeyDown(e, inp.value, close)) {
                    return;
                }
                if (e.keyCode === 27 || // Esc
                        (options.closeOnEnter !== false && e.keyCode === 13)) {
                    inp.blur();
                    CodeMirror.e_stop(e);
                    close();
                }
                if (e.keyCode === 13) {
                    callback(inp.value);
                }
            });

            if (options.closeOnBlur !== false) {
                CodeMirror.on(inp, "blur", close);
            }

            inp.focus();
        } else if (button = dialog.getElementsByTagName("button")[0]) {
            CodeMirror.on(button, "click", function () {
                close();
                me.focus();
            });

            if (options.closeOnBlur !== false) {
                CodeMirror.on(button, "blur", close);
            }

            button.focus();
        }
        return close;
    });

    CodeMirror.defineExtension("fromInputArea", function (inp, template, callback, options) {
        var cm = this; // this is the codemirror instance
        options = options || {};
        closeNotification(this, null);

        var dialog = inp.parentNode;
        var closed = false;
        var button;

        // store current cursor position
        var cursorPosition = this.doc.getCursor();
        
        function close(newVal) {
            if (typeof newVal === 'string') {
                inp.value = newVal;
            } else {
                if (closed) { return; }
                closed = true;
                dialog.parentNode.removeChild(dialog);
                cm.focus();
                if (options.onClose) {
                    options.onClose(dialog);
                }
            }
        }
        
        if (inp) {
            if (options.value) {
                inp.value = options.value;
                inp.select();
            }
            if (options.onInput) {
                CodeMirror.on(inp, "input", function (e) {
                    options.onInput(e, inp.value, close);
                });
            }
            if (options.onKeyUp) {
                CodeMirror.on(inp, "keyup", function (e) {
                    options.onKeyUp(e, inp.value, close);
                });
            }

            CodeMirror.on(inp, "keydown", function (e) {
                if (options && options.onKeyDown &&
                        options.onKeyDown(e, inp.value, close)) {
                    return;
                }
                if (e.keyCode === 27) { // Esc
                    // stop propagation
                    CodeMirror.e_stop(e);
                    // remove overlay selections
                    cm.state.overlays.forEach(function (overlay) {
                        cm.removeOverlay(overlay.mode);
                    });
                    // focus goes to CM editor
                    cm.focus();
                    // restore cursor position
                    cm.setCursor(cursorPosition);
                    // clear input text
                    inp.value = "";
                }
                if (e.keyCode === 13) {
                    // stop propagation
                    CodeMirror.e_stop(e);
                    // move cursor to the first selection, or restore cursor position if no selection available
                    var selections = cm.doc.listSelections();
                    if (selections && selections.length > 0) {
                        cm.setCursor(selections[0].head);
                    } else {
                        cm.setCursor(cursorPosition);
                    }
                    // focus goes to CM editor
                    cm.focus();
                }
            });

            if (options.closeOnBlur !== false) {
                CodeMirror.on(inp, "blur", close);
            }

            inp.focus();
        } else if (button = dialog.getElementsByTagName("button")[0]) {
            CodeMirror.on(button, "click", function () {
                close();
                cm.focus();
            });

            if (options.closeOnBlur !== false) {
                CodeMirror.on(button, "blur", close);
            }

            button.focus();
        }
        return close;
    });
    
    CodeMirror.defineExtension("openConfirm", function (template, callbacks, options) {
        closeNotification(this, null);
        var dialog = dialogDiv(this, template, options && options.bottom);
        var buttons = dialog.getElementsByTagName("button");
        var closed = false, me = this, blurring = 1;
        function close() {
            if (closed) {
                return;
            }
            closed = true;
            dialog.parentNode.removeChild(dialog);
            me.focus();
        }
        buttons[0].focus();
        var i = 0;
        for (i = 0; i < buttons.length; ++i) {
            var b = buttons[i];
            (function (callback) {
                CodeMirror.on(b, "click", function (e) {
                    CodeMirror.e_preventDefault(e);
                    close();
                    if (callback) {
                        callback(me);
                    }
                });
            }(callbacks[i]));
            CodeMirror.on(b, "blur", function () {
                --blurring;
                setTimeout(function () { if (blurring <= 0) { close(); } }, 200);
            });
            CodeMirror.on(b, "focus", function () { ++blurring; });
        }
    });

  /*
   * openNotification
   * Opens a notification, that can be closed with an optional timer
   * (default 5000ms timer) and always closes on click.
   *
   * If a notification is opened while another is opened, it will close the
   * currently opened one and open the new one immediately.
   */
    CodeMirror.defineExtension("openNotification", function (template, options) {
        var dialog = dialogDiv(this, template, options && options.bottom);
        var closed = false, doneTimer;
        var duration = options && typeof options.duration !== "undefined" ? options.duration : 5000;

        function close() {
            if (closed) { return; }
            closed = true;
            clearTimeout(doneTimer);
            dialog.parentNode.removeChild(dialog);
        }

        closeNotification(this, close);

        CodeMirror.on(dialog, 'click', function (e) {
            CodeMirror.e_preventDefault(e);
            close();
        });

        if (duration) {
            doneTimer = setTimeout(close, duration);
        }

        return close;
    });
}));