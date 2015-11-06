/**
 * @module PIMEmulink
 * @version 1.0
 * @description
 * The PIM specific Emulink functions.
 * Includes: editState and editTransition.
 * @author Nathan Robb
 * @date 20/10/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, es5:true */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var displayEditState       = require("plugins/emulink/models/pim/forms/displayEditPIMState"),
        displayEditTransition  = require("plugins/emulink/models/pim/forms/displayEditPIMTransition");

    /**
     * Creates a new instance of the PIMEmulink object,
     * this facilitates the PIM edit state and transition modals.
     * The current emuchartManager object is used internally.
     * @param emuchartsManager The current emuchartsManager object.
     * @constructor
     * @memberof PIMEmulink
     */
    function PIMEmulink(emuchartsManager) {
        this.emuchartsManager = emuchartsManager;
    }

    /**
     * Edits a PIM state (PM), opens a modal where:
     * 	The PM can be renamed,
     * 	The PM's widgets can be managed and
     * 	The PM's PMR can me set.
     * @param s the state (PM) to edit.
     * @memberof PIMEmulink
     */
    PIMEmulink.prototype.editState = function (s) {
        var _this = this;
        displayEditState.create({
            header: "Please enter new state...",
            textLabel: {
                newStateName: "State name",
                newStateWidgets: "State widgets",
            //	newStateComponents: "State components", // Not supported (PIMs within PIM).
                newStatePMR: "State PMR"
            },
            placeholder: {
                newStateName: "Name, e.g., StartInfusing",
                newStateWidgets: "Click to edit this states widgets",
            //	newStateComponents: "[Not Implemented]",
                newStatePMR: "Click to edit this states PMR"
            },
            value: {
                newStateName: s.name,
                widgets: s.widgets,
            //	components: s.components,
                // Emuchart wide unique PMR.
                pmr: _this.emuchartsManager.getPMR()
            },
            buttons: ["Cancel", "Save State"]
        }).on("save_state", function (e, view) {
            // Get new values from template.
            var newStateName = e.data.labels.get("newStateName");
            var newStateWidgets = e.data.labels.get("newStateWidgets");
            //var newStateComponents = e.data.labels.get("newStateComponents");
            var newStatePMR = e.data.labels.get("newStatePMR");

            if (newStateName && newStateName.value !== "") {
                // Save over only the new values.
                s.name = newStateName;
                s.widgets = newStateWidgets;
                //s.components = newStateComponents;

                _this.emuchartsManager.rename_state(s.id, s);
                // Save the PMR set within the edit state modal into the existing PMR.
                _this.emuchartsManager.mergePMR(newStatePMR);
                view.remove();
            }
        }).on("cancel", function (e, view) {
            // just remove window.
            view.remove();
        });
    };

    /**
     * Edits a PIM transition, opens a modal where:
     * 	The PIM transition can be set via a dropdown of
     * 	the source node's widget's I-Behaviours.
     * @param t the transition to edit.
     * @memberof PIMEmulink
     */
    PIMEmulink.prototype.editTransition = function (t) {
        var _this = this;
        var behavs = [];

        t.source.widgets.forEach(function (w) {
            w.behaviours.forEach(function (b) {
                var type = b.substring(0, 2);
                // Only show interactive behaviours.
                if (type === 'I_') {
                    behavs.push({value: b, text: b + " (w: " + w.name + ")"});
                }
            });
        });
        displayEditTransition.create({
            header: "Please select the transition behaviour (" + t.source.name + " --> " + t.target.name + ")...",
            textLabel: {
                i_behaviour: "I-Behaviour"
            },
            placeholder: {
                i_behaviour: "Select Interactive Behaviour"
            },
            value: {
                i_behaviours: behavs
            },
            buttons: ["Cancel", "Save"]
        }).on("save", function (e, view) {
            var transitionLabel = e.data.options.get("i_behaviour");
            if (transitionLabel) {
                // -1 as index 0 in dropdown list is placeholder.
                var name = behavs[transitionLabel - 1].value;
                // Rename the transition to the I-Behaviour.
                _this.emuchartsManager.rename_transition(t.id, name);
                view.remove();
            }
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    };

    module.exports = PIMEmulink;
});
