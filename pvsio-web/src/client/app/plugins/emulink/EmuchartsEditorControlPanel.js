/** @module EmuchartsEditorControlPanel */
/**
 * EmuchartsEditorControlPanel provides the user with forms for editing the content for emucharts diagrams.
 * @author Paolo Masci
 * @date 05/11/14 5:43:08 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher"),
        EmuchartsEditor = require("plugins/emulink/EmuchartsEditor");

    var _this;

    /**
     * Constructor
     * @memberof EmuchartsEditorControlPanel
     */
    function EmuchartsEditorControlPanel(emuEditor) {
        _this = this;
        this.emuEditor = emuEditor;
        eventDispatcher(this);
    }


    /**
     * Utility function to generate formatted labels for transitions
     * @returns labels, as a formatted string
     * @memberof EmuchartsEditorControlPanel
     */
//    function labelToString(label) {
//        var ans = "";
//        if (label.listConditions) {
//            label.listConditions.forEach(function (cond) { ans += cond + "; "; });
//            ans = " [ " + ans + " ] ";
//        }
//        if (label.listOfOperations) {
//            label.listOfOperations.forEach(function (item) { ans += item + "; "; });
//            ans = " { " + ans + " } ";
//        }
//        return ans;
//    }


    /**
     * Returns a fresh state name
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getFreshStateName = function () {
        return this.emuEditor.getFreshStateName();
    };

    /**
     * Returns a fresh transition name
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getFreshTransitionName = function () {
        return this.emuEditor.getFreshTransitionName();
    };

    /**
     * Returns a fresh name for initial transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getFreshInitialTransitionName = function () {
        return this.emuEditor.getFreshInitialTransitionName();
    };

    /**
     * Returns an array containing the current set of states in the diagram
     * Each states is given as a pair { name, id }
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getStates = function () {
        return this.emuEditor.getStates();
    };

    /**
     * Returns an array containing the current set of constants defined in the diagram
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getConstants = function () {
        return this.emuEditor.getConstants();
    };

    /**
     * Returns an array containing the current set of variables defined in the diagram
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getVariables = function () {
        return this.emuEditor.getVariables();
    };

    /**
     * Returns an array containing the current set of input variables defined in the diagram
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getInputVariables = function () {
        return this.emuEditor.getInputVariables();
    };

    /**
     * Returns an array containing the current set of output variables defined in the diagram
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getOutputVariables = function () {
        return this.emuEditor.getOutputVariables();
    };

    /**
     * Returns an array containing the current set of local variables defined in the diagram
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getLocalVariables = function () {
        return this.emuEditor.getLocalVariables();
    };

    /**
     * Returns an array specifying the supported variable scopes
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getVariableScopes = function () {
        return this.emuEditor.getVariableScopes();
    };

    /**
     * Returns an array containing the current set of transitions in the diagram
     * Each transition is given as a 4-tuple { name, id, source, target }
     * where source and target are pairs { name, id }
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getTransitions = function () {
        return this.emuEditor.getTransitions();
    };

    /**
     * Returns an array containing the current set of initial transitions in the diagram
     * Each transition is given as a 3-tuple { name, id, target }
     * where target is a pair { name, id }
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.getInitialTransitions = function () {
        return this.emuEditor.getInitialTransitions();
    };

    /**
     * utility function to rename transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditorControlPanel.prototype.rename_transition = function (transitionID, newLabel) {
        return this.emuEditor.rename_transition(transitionID, newLabel);
    };

    /**
     * utility function to rename initial transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.rename_initial_transition = function (transitionID, newLabel) {
        return this.emuEditor.rename_initial_transition(transitionID, newLabel);
    };


    /**
     * utility function to rename states
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.rename_state = function (stateID, newLabel) {
        return this.emuEditor.rename_state(stateID, newLabel);
    };

    /**
     * Interface function for adding states
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.add_state = function (stateName, position) {
        return this.emuEditor.add_state(stateName, position);
    };

    /**
     * Interface function for deleting states and all transitions incoming/outgoing to this state
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditor.prototype.delete_state = function (stateID) {
        return this.emuEditor.delete_state(stateID);
    };

    /**
     * Interface function for deleting transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.delete_transition = function (transitionID) {
        return this.emuEditor.delete_transition(transitionID);
    };

    /**
     * Interface function for deleting initial transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.delete_initial_transition = function (transitionID) {
        return this.emuEditor.remove_initial_transition(transitionID);
    };

    /**
     * Interface function for adding transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.add_transition = function (transitionName, from, to) {
        return this.emuEditor.add_transition(transitionName, from, to);
    };

    /**
     * Interface function for adding initial transitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.add_initial_transition = function (transitionName, to) {
        return this.emuEditor.add_initial_transition(transitionName, to);
    };

    /**
     * Interface function for adding new constant definitions
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.add_constant = function (newConstant) {
        return this.emucharts.add_constant(newConstant);
    };

    /**
     * Interface function for adding new state variables
     * @memberof EmuchartsEditorControlPanel
     */
    EmuchartsEditorControlPanel.prototype.add_variable = function (newVariable) {
        return this.emucharts.add_variable(newVariable);
    };

    /**
     * Interface function for deleting charts
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_chart = function () {
        return this.emuEditor.delete_chart();
    };

    /**
     * Interface function for checking whether the current chart is empty
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.empty_chart = function () {
        return this.emuEditor.empty_chart();
    };

    module.exports = EmuchartsEditorControlPanel;
});
