/** @module EmuchartsCodeGenerators */
/**
 * EmuchartsCodeGenerators collects all Emucharts code/model generators
 * @author Paolo Masci
 * @date Nov 13, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var instance;

    var EmuchartsPVSPrinter    = require("plugins/emulink/models/EmuchartsPVSPrinter"),
        EmuchartsLustrePrinter = require("plugins/emulink/models/EmuchartsLustrePrinter"),
        EmuchartsPIMPrinter    = require("plugins/emulink/models/EmuchartsPIMPrinter"),
        EmuchartsCppPrinter    = require("plugins/emulink/models/EmuchartsCppPrinter"),
        EmuchartsMALPrinter    = require("plugins/emulink/models/EmuchartsMALPrinter2"),
        EmuchartsVDMPrinter    = require("plugins/emulink/models/EmuchartsVDMPrinter"),
        EmuchartsJSPrinter     = require("plugins/emulink/models/EmuchartsJavaScriptPrinter"),
        EmuchartsAdaPrinter    = require("plugins/emulink/models/EmuchartsAdaPrinter"),
        EmuchartsBlessPrinter  = require("plugins/emulink/models/EmuchartsBlessPrinter"),
        EmuchartsMisraCPrinter = require("plugins/emulink/models/EmuchartsMisraCPrinter"),
        EmuchartsNuXMVPrinter  = require("plugins/emulink/models/EmuchartsNuXMVPrinter");

    function EmuchartsCodeGenerators() {
        this.emuchartsPVSPrinter = new EmuchartsPVSPrinter("emuchart_th");
        this.emuchartsLustrePrinter = new EmuchartsLustrePrinter("emuchart_Lustre");
        this.emuchartsPIMPrinter = new EmuchartsPIMPrinter("emuchart_PIM");
        this.emuchartsCppPrinter = new EmuchartsCppPrinter("emuchart_Cpp");
        this.emuchartsMALPrinter = new EmuchartsMALPrinter("emuchart_MAL");
        this.emuchartsVDMPrinter = new EmuchartsVDMPrinter("emuchart_VDM");
        this.emuchartsJSPrinter = new EmuchartsJSPrinter("emucharts_JS");
        this.emuchartsAdaPrinter = new EmuchartsAdaPrinter("emucharts_Ada");
        this.emuchartsBlessPrinter = new EmuchartsBlessPrinter("emucharts_Bless");
        this.emuchartsMisraCPrinter = new EmuchartsMisraCPrinter("emucharts_MisraC");
        this.emuchartsNuXMVPrinter = new EmuchartsNuXMVPrinter("emucharts_NuXMV");
        return this;
    }

    module.exports = {
        getInstance: function () {
            if (!instance) { instance = new EmuchartsCodeGenerators(); }
            return instance;
        },
        hasInstance: function () {
            return !!instance;
        }
    };

});
