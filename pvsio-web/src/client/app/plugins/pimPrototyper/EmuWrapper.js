/**
 * @module EmuWrapper
 * @desc Provides the PIM prototyping module with access to the active EmuChart, allowing the chart to be modified using
 * PIM prototyping objects.
 * @author Nathaniel Watson
 */
/* global _ */
define(function (require, exports, module) {
    "use strict";

    var EmuWrapper = function (emuchartsEditor) {
        this._emuchartsEditor = emuchartsEditor;
    };

    /**
     * Adds the screens from the provided ScreenCollection to the chart
     * @param {ScreenCollection} screens Collection of screens to add
     */
    EmuWrapper.prototype.addScreens = function (screens) {
        var _this = this;

        screens.forEach(function (s) {
            _this._addScreenState(s);
        });

        screens.forEach(function (s) {
            var targets = d3.set();
            var widgets = s.get("widgets");

            _.forEach(widgets, function (w) {
                // Add a transition from this screen to the widget's target, but only if a transition for the same event
                // doesn't already exist.
                if (w.targetScreen() != null && !targets.has(w.targetScreen().get("name"))) {
                    _this._addWidgetTransition(w, s);
                    targets.add(w.targetScreen().get("name"));
                }
            });

            // Add the initial transtion if this screen is the starting screen
            if (s.get("isInitial")) {
                _this._emuchartsEditor.emucharts.add_initial_edge({
                    target: { id: s.id },
                    name: "init",
                    controlPoint: { x: 0, y: 0 }
                });
            }
        });

        this._emuchartsEditor.layOutChart();
        this._emuchartsEditor.render();
    };

    /**
     * Adds the provided screen to the chart as a new state. This does not add any transitions.
     * @param {Screen} scr Screen to add.
     */
    EmuWrapper.prototype.addScreenState = function (scr) {
        this._addScreenState(scr);
        this._emuchartsEditor.renderStates();
    };

    /**
     * Sets the EmuChartsEditor that should be used by the wrapper
     * @param {EmuChartsEditor} editor EmuChartsEditor to interface with for creating/modifying charts
     */
    EmuWrapper.prototype.setEmuchartsEditor = function (editor) {
        this._emuchartsEditor = editor;
    };

    EmuWrapper.prototype._addScreenState = function (scr) {
        var _this = this;
        // Have to call down into the emulink object so that we can get the state/node object back
        var state = this._emuchartsEditor.emucharts.add_node({
            name: scr.get("name"),
            id: scr.id
        });

        var widgets = [];

        var screenWidgets = scr.get("widgets");

        _.forEach(screenWidgets, function (w) {
            var behaviours = [];

            if (w.targetScreen() != null) {
                behaviours.push(_this._createBehaviourName(w.targetScreen()));
            }

            widgets.push({
                name: w.name(),
                category: "ActionControl",
                behaviours: behaviours
            });
        });

        state.widgets = widgets;
    };

    /**
     * Adds any links from the provided widget as transitions within the chart. Should be called after screens/states
     * have been added to the chart.
     * @private
     * @param {PIMWidget} widget Widget who's transitions should be added
     * @param {Screen} sourceScreen Screen that the widget belongs to
     */
    EmuWrapper.prototype._addWidgetTransition = function (widget, sourceScreen) {
        if (widget.targetScreen() == null) {
            return;
        }

        this._emuchartsEditor.emucharts.add_edge({
            source: { id: sourceScreen.id },
            target: { id: widget.targetScreen().id },
            name: this._createBehaviourName(widget.targetScreen()),
            controlPoint: { x: 0, y: 0 }
        });
    };

    EmuWrapper.prototype._createBehaviourName = function (targetScreen) {
        return "I_" + targetScreen.get("name").trim().replace(/[\s-]+/g, "_");
    };

    module.exports = EmuWrapper;
});
