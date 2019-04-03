/** @module EmuchartsEditorModes*/
/**
 * EmuchartsEditorModes defines the modes of the emuchart editor. This is code is a re-engineered version of stateMachine.js implemented in branch emulink-commented
 * @author Paolo Masci
 * @date 16/05/14 10:44:12 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var MODE;

    function EmuchartsEditorModes() {
        MODE = { BROWSE: 0, ADD_TRANSITION: 1, ADD_STATE: 2, RENAME: 3, ZOOM: 4, DELETE: 5 };
    }

    EmuchartsEditorModes.prototype.BROWSE = function () { return MODE.BROWSE; };
    EmuchartsEditorModes.prototype.ADD_TRANSITION = function () { return MODE.ADD_TRANSITION; };
    EmuchartsEditorModes.prototype.ADD_STATE = function () { return MODE.ADD_STATE; };
    EmuchartsEditorModes.prototype.RENAME = function () { return MODE.RENAME; };
    EmuchartsEditorModes.prototype.DELETE = function () { return MODE.DELETE; };

    EmuchartsEditorModes.prototype.mode2string = function (mode) {
        if (mode === MODE.BROWSE) {
            return "browse diagram";
        } else if (mode === MODE.ADD_TRANSITION) {
            return "add transitions";
        } else if (mode === MODE.ADD_STATE) {
            return "add states";
        } else if (mode === MODE.RENAME) {
            return "rename states/transitions";
        } else if (mode === MODE.DELETE) {
            return "delete states/transitions";
        } else { return "error: unknown mode"; }
    };

    EmuchartsEditorModes.prototype.modeTooltip = function (mode) {
        if (mode === MODE.BROWSE) {
            return "Use the toolbar to create or edit modes and triggers.";
        } else if (mode === MODE.ADD_TRANSITION) {
            return "Drag mouse between two states to create a new trigger between modes.";
        } else if (mode === MODE.ADD_STATE) {
            return "Click on an empty area of the diagram to create a new mode.";
        } else if (mode === MODE.RENAME) {
            return "Double click on modes and trigger to rename them.";
        } else if (mode === MODE.DELETE) {
            return "Click on modes and triggers to delete them.";
        } else { return "Error: unexpected editor mode -- please report a bug."; }
    };

    module.exports = EmuchartsEditorModes;
});
