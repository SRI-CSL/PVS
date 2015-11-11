/**
 * @module PIMImporter
 * @version 1.0
 * @description
 * PIMImporter handles the importing a PIM diagram.
 * If an EmuchartManager is supplied the chart is also loaded into Emulink as an Emuchart.
 * @author Nathan Robb
 * @date 20/10/15
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    function PIMImporter() { }

    /**
     * Reads in Presentation Interaction Models with corresponding
     * Presentation Models from a file.
     * @param fileIn The file to read the models from.
     * @param emuchartManager An optional arg, if given the imported PIM will also be loaded as an Emuchart.
     * @returns {{emucharts: Object, models: {pims: Array, pms: Array}}}
     * The emuchart, PIMs and PMs from the file.
     * @memberof PIMImporter
     */
    PIMImporter.prototype.importPIM = function (fileIn, emuchartManager) {
        /** Index within the file */
        var index;
        /** File array */
        var file;
        /** Presentation Models array */
        var pModels = [];
        /** Presentation Interaction Models array */
        var pims = [];

        /**
         * Reads in PIMs (Populates pims)
         */
        function readPIMs() {
            /**
             * Reads in states
             * @returns {Array} Array of states for a PIM
             */
            function readStates() {
                var i, n = parseInt(file[index++], 10);
                var states = [];
                // Read in states for a PIM
                for (i = 0; i < n; i++) {
                    states.push({ name: file[index++] });
                }
                return states;
            }

            /**
             * Reads in the transitions for a PIM
             * @returns {Array} Array of transitions for a PIM
             */
            function readTransitions() {
                var i, n = parseInt(file[index++], 10);
                var transitions = [];
                var tStartState, tEndState, tBehaviour;
                // Read in transitions for a PIM
                for (i = 0; i < n; i++) {
                    tStartState = file[index++];
                    tEndState = file[index++];
                    tBehaviour = file[index++];
                    transitions.push({
                        start_state: tStartState,
                        end_state: tEndState,
                        i_behaviour: tBehaviour
                    } );
                }
                return transitions;
            }

            var i, n = parseInt(file[index++], 10);
            var pm, pmName;
            var pimStates, pimTransitions, pimName, pModel, pimStartState, pimFinalStates;

            // Read in PIMs
            for (i = 0; i < n; i++) {
                pimName = file[index++];
                pimStartState = { name: file[index++] };

                pModel = 'undefined';
                pmName = file[index++];
                for (pm in pModels) {
                    if (pModels.hasOwnProperty(pm)) {
                        if (pModels[pm].name === pmName) {
                            pModel = pModels[pm];
                            break;
                        }
                    }
                }
                // Can't make a PIM without a PM
                if (pModel === 'undefined') {
                    throw "PModel (" + pmName + ") not found for PIM";
                }

                pimStates = readStates();
                pimFinalStates = readStates();
                pimTransitions = readTransitions();

                pims.push({
                    states: pimStates,
                    transitions: pimTransitions,
                    name: pimName,
                    pm: pModel,
                    start_state: pimStartState,
                    final_states: pimFinalStates
                });
            }
        }

        /**
         * Reads in Presentation models (populates pModels)
         * @param withPMR Do these models have presentation model relations
         */
        function readModels(withPMR) {
            /** Counters used for each PMs widgets and CMs */
            var widgetsCount, componentModelsCount;
            /**
             * Returns the widgets
             * @returns {Array} Array of widgets
             */
            function readWidgets() {
                /**
                 * Returns the behaviours for the current widget
                 * @returns {Array.<T>} Array of behaviours
                 */
                function readBehaviours() {
                    var i, n = parseInt(file[index++], 10);
                    var behaviours = [];
                    var behaviour;
                    // Read in the behaviours for a widget
                    for (i = 0; i < n; i++) {
                        behaviour = file[index++];
                        if (behaviour.charAt(behaviours.length - 1) === '/') {
                            behaviour = behaviour.slice(0, -1);
                        }
                        behaviours.push(behaviour);
                    }
                    return behaviours;
                }

                var widgets = [];
                var i;
                var wName, wCategory, wBehaviours;
                // Read in the widgets
                for (i = 0; i < widgetsCount; i++) {
                    wName = file[index++];
                    wCategory = file[index++];
                    wBehaviours = readBehaviours();
                    widgets.push({
                        name : wName,
                        category : wCategory,
                        behaviours : wBehaviours
                    });
                }
                return widgets;
            }

            /**
             * Returns the presentation model relations
             * @returns {Array} Array of PMRs
             */
            function readPMRs() {
                var i, n = parseInt(file[index++], 10);
                var pmrPairs = [];
                var pmrBehaviour, pmrOperation;
                // Read in the PMRs for a presentation model
                for (i = 0; i < n; i++) {
                    // Add pmr pair
                    pmrBehaviour = file[index++];
                    pmrOperation = file[index++];

                    pmrPairs.push({
                        behaviour : pmrBehaviour,
                        operation : pmrOperation
                    });
                }

                return pmrPairs;
            }

            /**
             * Reads in the component models for the presentation model
             * @returns {Array} Array of component models
             */
            function readComponentModels() {
                var components = [];
                var i;
                var cName, pModel;
                // Read in the component models for a presentation model
                for (i = 0; i < componentModelsCount; i++) {
                    cName = file[index++];
                    for (pModel in pModels) {
                        if (pModels.hasOwnProperty(pModel)) {
                            if (cName === pModels[pModel].name) {
                                components.push(pModels[pModel]);
                            }
                        }
                    }
                }
                return components;
            }

            var i, n = parseInt(file[index++], 10);
            var pmName, pmr, pmWidgets, pmComponentModels;
            // Read in the presentation models
            for (i = 0; i < n; i++) {
                pmName = file[index++];

                // Need to read these in together
                widgetsCount = parseInt(file[index++], 10);
                componentModelsCount = parseInt(file[index++], 10);

                pmWidgets = readWidgets();
                pmr = withPMR ? readPMRs() : [];
                pmComponentModels = readComponentModels();

                pModels.push({
                    name: pmName,
                    widgets: pmWidgets,
                    components: pmComponentModels,
                    pmr: pmr
                });
            }
        }

        // Read file
        if (fileIn && fileIn.content) {
            index = 0;
            // Split the file on new line, trim and remove empty lines
            file = fileIn.content.split("\n").map(function(s) { return s.trim(); }).filter(function(s){ return s; });

            readModels(false);
            // If more to read in file
            if (index < file.length - 1) {
                if (file[index++] === "====") {
                    readModels(true);
                } else {
                    throw "Invalid input file. Expected PMRs.";
                }
            }
            // If more to read in file
            if (index < file.length - 1) {
                if (file[index++] === "====") {
                    readPIMs();
                } else {
                    throw "Invalid input file. Expected PIMs.";
                }
            }
        }

        /**
         * Parses a PIM to a Emuchart (pim version) adds to Emulink.
         * @param pim PIM to parse as Emuchart.
         * @param emuchartManager Manager to load the PIM into.
         * @returns object (Emuchart) {
                    states: array of object (Emuchart state),
                    transitions: array of object (Emuchart transition),
                    initial_transitions: array of object (Emuchart transition),
                    constants: null,
                    variables: null,
                    pm: object (PM),
                    start_state: array of object (Emuchart state),
                    final_states: array of object (Emuchart state),
                    isPIM: true
                }
         */
        function parsePIMAsEmuchart(pim, emuchartManager) {
            if (!emuchartManager) {
                return {};
            }

            // TODO: find a nicer way to position the nodes.
            // Currently loads them in on a diagonal (top left to bottom right)
            var init_x = 0, init_y = 0;
            var incr_x = 50, incr_y = 50;

            // Need to get the states info out of the PIM's Component models
            var states = pim.pm.components.map( function(pm) {
                // Don't add the PIM's PM (a PIM is also a PM)
                if (pm.name === pim.name) { return; }
                init_x += incr_x;
                init_y += incr_y;
                return {
                    name: pm.name,
                    id: pm.name,
                    x: init_x,
                    y: init_y,
                    width: 50,
                    height: 50,
                    widgets: pm.widgets,
                    // Only PIMs currently have component models, set it anyway for now.
                    components: pm.components,
                    pmr: pm.pmr
                };
            });

            var transId = 1;
            var transitions = pim.transitions.map( function(trans) {
                return {
                    name: trans.i_behaviour,
                    id: trans.i_behaviour + transId++,
                    source: {
                        name: trans.start_state,
                        id: trans.start_state
                    },
                    target: {
                        name: trans.end_state,
                        id: trans.end_state
                    }
                };
            });

            // PIMs have no initial transition, make one using the start states name.
            var initialTransition = {
                id: "INIT_" + pim.start_state.name,
                name: "INIT_" + pim.start_state.name,
                target: { name: pim.start_state.name, id: pim.start_state.name }
            };

            var chart = {
                descriptor: {
                    file_type: "emdl",
                    version: "1.1",
                    description: "Emucharts PIM from PIM import",
                    chart_name: pim.name
                },
                chart: {
                    states: states,
                    transitions: transitions,
                    initial_transitions: [ initialTransition ],
                    // TODO: Not implemented.
                    constants: null,
                    variables: null,
                    pm: pim.pm,
                    start_state: pim.start_state,
                    final_states: pim.final_states,
                    // This is a PIM, used for deciding which Emulink modals to open and test gen.
                    isPIM: true
                }
            };
            emuchartManager.importEmucharts({ name: pim.name, content: chart });

            return chart;
        }

        // Arrange models in correct order
        pims.reverse();
        pModels.reverse();

        // Assume only 1 PIM in file. Use the first one in chart.
        var chart = emuchartManager ? parsePIMAsEmuchart(pims[0], emuchartManager) : {};

        var models = {
            emucharts: chart,
            models: {
                pims: pims,
                pms: pModels
            }
        };
        // Return the read in PIMs and PMs
        return models;
    };

    module.exports = PIMImporter;
});
