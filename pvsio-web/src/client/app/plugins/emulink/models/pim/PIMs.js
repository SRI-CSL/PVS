/**
 * @module PIMs
 * @version 1.0
 * @description
 * PIMs handles the Emuchart functionality that is PIM specific,
 * these methods should be used with Emuchart methods if the Emuchart is a PIM (isPIM).
 * @author Nathan Robb
 * @date 20/10/2015
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/

define(function (require, exports, module) {
    "use strict";

    /**
     * Creates a new instance of the PIMEmulink object,
     * this facilitates in cloning PIM objects, typically used
     * in the edit modals to ensure changes don't take effect
     * too early. Also contains PMR helpers.
     * @param isPIM If the current Emuchart is a pim.
     * @constructor
     * @memberof PIMs
     */
    function PIMs(isPIM) {
        this.isPIM = isPIM || false;
    }

    /**
     * Does the PIMs object this the Emuchart is a PIM.
     * @returns {boolean} true if it think it is a PIM.
     * @memberof PIMs
     */
    PIMs.prototype.getIsPIM = function () {
        return this.isPIM || false;
    };

    /**
     * Set the Emuchart as a PIM (or vica versa).
     * @param toPIM Are we converting to a PIM? (defaults to yes).
     * @returns {boolean} true if successful.
     * @memberof PIMs
     */
    PIMs.prototype.toPIM = function (toPIM) {
        toPIM =  toPIM === 'undefined' ? true : toPIM;
        if (toPIM && !this.isPIM) {
            this.isPIM = true;
            return true;
        }
        if (!toPIM && this.isPIM) {
            this.isPIM = false;
            return true;
        }
        return false;
    };

    /**
     * Returns a clone of the supplied widget.
     * Intended for internal use.
     * @param widget PM widget to clone.
     * @returns object {
            name : string,
            category : string,
            behaviours : array of string
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.clonePIMWidget = function (widget) {
        if (!widget) { return null; }
        return {
            name : widget.name,
            category : widget.category,
            // If behaviours becomes an anon object this clone needs to be updated.
            behaviours : widget.behaviours.slice()
        };
    };

    /**
     * Get the PM widget (returns a clone).
     * @param widget widget to 'get'.
     * @returns object {
            name : string,
            category : string,
            behaviours : array of string
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.getWidget = function (widget) {
        if (!widget) { return null; }
        return this.clonePIMWidget(widget);
    };

    /**
     * Gets cloned PIM versions of the supplied widgets.
     * @param widgets Widgets to get cloned PIM versions of.
     * @returns Array of object {
                name : string,
                category : string,
                behaviours : array of string
            } or null
        }
     * @memberof PIMs
     */
    PIMs.prototype.getWidgets = function (widgets) {
        if (!widgets) { return []; }
        var clonedWidgets = [], _this = this;
        widgets.forEach(function (w) {
            // Warning: If widgets become stored in a d3 map, this will need to be updated to:
            // clonedWidgets.push(_this.getWidget(widgets.get(w)));
            clonedWidgets.push(_this.getWidget(w));
        });
        return clonedWidgets;
    };

    /**
     * Returns a clone of the supplied state.
     * Intended for internal use.
     * @param node state (or PM) to clone.
     * @returns object {
            name: string,
            id: string,
            x: int,
            y: int,
            width : int,
            height: int,
            widgets : array of widget
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.cloneAsPIMState = function (node) {
        if (!node) { return null; }
        return {
            name: node.name,
            id: node.id,
            x: node.x,
            y: node.y,
            width : node.width,
            height: node.height,
            widgets : this.getWidgets(node.widgets)
        //	components : this.getStates(node.components)
        };
    };

    /**
     * Returns a clone of the supplied state.
     * @param node state (or PM) to clone.
     * @returns object {
            name: string,
            id: string,
            x: int,
            y: int,
            width : int,
            height: int,
            widgets : array of widget
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.getState = function (node) {
        if (!node) { return null; }
        return this.cloneAsPIMState(node);
    };


    /**
     * Gets cloned PIM versions (PM) of the supplied states.
     * @param nodes The states to get cloned PIM versions of.
     * @returns Array of object {
            name: string,
            id: string,
            x: int,
            y: int,
            width : int,
            height: int,
            widgets : array of widget
        } or null, or empty array
     * @memberof PIMs
     */
    PIMs.prototype.getStates = function (nodes) {
        if (!nodes) { return []; }
        var states = [], _this = this;
        nodes.forEach(function (key) {
            states.push(_this.getState(nodes.get(key)));
        });
        return states;
    };

    /**
     * Returns a clone of the supplied transition.
     * Intended for internal use.
     * @param trans Transition to clone.
     * @returns object {
            name: string,
            id: string,
            source: PM (PIM state),
            target: PM (PIM state),
            controlPoint: object,
            // PIM specific.
            start_state : string,
            end_state : string,
            i_behaviour : string
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.cloneAsPIMTransition = function (trans) {
        if (!trans) { return null; }
        return {
            name: trans.name,
            id: trans.id,
            source: this.cloneAsPIMState(trans.source),
            target: this.cloneAsPIMState(trans.target),
            controlPoint: (trans.controlPoint) ? {
                x: trans.controlPoint.x,
                y: trans.controlPoint.y
            } : null,
            // PIM specific.
            start_state : trans.source.name,
            end_state : trans.target.name,
            i_behaviour : trans.name
        };
    };

    /**
     * Returns a clone of the supplied transition.
     * @param trans Transition to clone.
     * @returns object {
            name: string,
            id: string,
            source: PM (PIM state),
            target: PM (PIM state),
            controlPoint: object,
            // PIM specific.
            start_state : string,
            end_state : string,
            i_behaviour : string
        } or null
     * @memberof PIMs
     */
    PIMs.prototype.getTransition = function (trans) {
        if (!trans) { return null; }
        return this.cloneAsPIMTransition(trans);
    };


    /**
     * Returns a clone of all the supplied transitions.
     * @param edges Transitions get PIM cloned versions of.
     * @returns Array of object {
            name: string,
            id: string,
            source: PM (PIM state),
            target: PM (PIM state),
            controlPoint: object,
            // PIM specific.
            start_state : string,
            end_state : string,
            i_behaviour : string
        } or null, or empty array
     * @memberof PIMs
     */
    PIMs.prototype.getTransitions = function (edges) {
        if (!edges) { return []; }
        var transitions = [], _this = this;
        edges.forEach(function (key) {
            transitions.push(_this.getTransition(edges.get(key)));
        });
        return transitions;
    };

    /**
     * Get the PIM's PMR.
     * @param pmrs The current PMRs.
     * @param behaviour a specific S-Behaviour to relieve.
     * @param isSave Special case, if this is a save (see return).
     * @returns {*} If only PMRs provided returns all PMR as a new d3 map,
     * If PMRs and behaviour provided returns either:
     * 	the relation (if a behaviour could be found) as { behaviour, operation },
     * 	else returns null.
     * If PMRs, no behaviour (i.e. null) and isSave - special case for saving the PMR
     * to the edml file. This returns an object with behaviour as the property and the
     * operation as the value (ensures correct stringify).
     * @memberof PIMs
     */
    PIMs.prototype.getPMR = function (pmrs, behaviour, isSave) {
        if (!pmrs) { return d3.map(); }
        if (behaviour) {
            return pmrs.get(behaviour) || null;
        }

        if (isSave) {
            // Store as a standard JS object to ensure correct stringify on save.
            var obj = {};
            pmrs.forEach(function (behaviour) {
                obj[behaviour] = pmrs.get(behaviour);
            });
            return obj;
        }

        // Return all PMR in new d3 set.
        var _this = this;
        var _pmr = d3.map();
        pmrs.forEach(function (behaviour) {
            _this.addPMR(_pmr, behaviour, pmrs.get(behaviour));
        });
        return _pmr;
    };

    /**
     * Add a PMR (overrides any existing PMR for the given behaviour).
     * ({behaviour (string), operation (string)}).
     * @param pmrs The PMRs to add the new PMR too.
     * @param behaviour the PMR behaviour.
     * @param operation The PMR operation.
     * @returns {boolean} true if successfully added.
     * @memberof PIMs
     */
    PIMs.prototype.addPMR = function (pmrs, behaviour, operation) {
        if (!pmrs || !behaviour || !operation) {
            return false;
        }
        pmrs.set(behaviour, operation);
        return true;
    };

    /**
     * Saves the new PMRs into the d3 map of all PMRs.
     * @param pmrs The current PMRs for the PIM.
     * @param newPMRs the new PMRs.
     * @memberof PIMs
     */
    PIMs.prototype.mergePMR = function (pmrs, newPMRs) {
        var _this = this;
        if (!pmrs || !newPMRs) { return; }
        newPMRs.forEach(function (behaviour) {
            _this.addPMR(pmrs, behaviour, newPMRs.get(behaviour));
        });
    };

    module.exports = PIMs;
});
