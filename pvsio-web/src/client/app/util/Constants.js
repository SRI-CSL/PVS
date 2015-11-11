/**
 * Constants
 * @description Defines default constants, e.g., default file names and default file content for pvs, ivy, etc.
 * @author Paolo Masci
 * @date 4/12/14 8:51:02 AM
 */
/*jshint unused: false*/
/*global define*/
define(function (require, exports, module) {
    "use strict";
    
    var defaultProjectName = "defaultProject",
        defaultPVSFileName = "main.pvs",
        defaultPVSTheory   = "main: THEORY\n" +
                             " BEGIN\n " +
                             "  %-- Please type your PVS specification here!\n" +
                             " END main",
        autosavePath = "autosave",
        widgetDefinitionsFile = "widgetDefinition.json",
        scriptFile = "scripts.json";
        

    module.exports = {
        defaultProjectName: defaultProjectName,
        defaultPVSTheory: defaultPVSTheory,
        defaultPVSFileName: defaultPVSFileName,
        autosavePath: autosavePath,
        widgetDefinitionsFile: widgetDefinitionsFile,
        scriptFile: scriptFile
    };
});