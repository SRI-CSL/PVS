/**
 * @module PMTextGenerator
 * @version 1.0
 * @description Provides functionality for generating a textual representation of presentation models (PMs)
 * @author Nathaniel Watson
 * @date 08/10/2016
 */

/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define */

define(function (require, exports, module) {
    "use strict";

    /**
     * Constructor.
     * @memberof PMTextGenerator
     */
    var PMTextGenerator = {
        /**
         * Returns a string representation of the given PM.
         * @param {object} pm Object containing name and components fields, with components containing the states from a PIM emuchart.
         * @returns {string} String representation of the given PM
         * @memberof PMTextGenerator
         */
        generateText: function (pm) {
            var text = pm.name + " is ";

            for (var i = 0; i < pm.components.length; i++) {
                text += pm.components[i].name + (i < pm.components.length - 1 ? " : " : "\n\n");
            }

            for (i = 0; i < pm.components.length; i++) {
                text += pm.components[i].name + " is\n";
                var widgets = pm.components[i].widgets;

                for (var j = 0; j < widgets.length; j++) {
                    var w = widgets[j];

                    var behaviourString = "";

                    for (var k = 0; k < w.behaviours.length; k++) {
                        behaviourString += w.behaviours[k] + (k < w.behaviours.length - 1 ? ", " : "");
                    }

                    text += "\t(" + w.name + ", " + w.category + ", (" + behaviourString + "))\n";
                }

                text += "\n";
            }

            return text;
        },

        /**
         * Returns a string representation of the given PIM Emuchart to be saved in a file.
         * @param {string} name Name of the emuchart
         * @param {object} chart Object containing a `pm` field to be passed to PMTextGenerator.generateText
         * @memberof PMTextGenerator
         */
        print: function (name, chart) {
            var ans = PMTextGenerator.generateText(chart.pm);
            return { file_name: name + ".txt", res: ans };
        }
    };

    module.exports = PMTextGenerator;
});
