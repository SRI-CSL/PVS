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

    var EmuchartsPVSPrinter    = require("plugins/emulink/models/EmuchartsPVSPrinter3"),
        EmuchartsLustrePrinter = require("plugins/emulink/models/EmuchartsLustrePrinter"),
        EmuchartsPIMPrinter    = require("plugins/emulink/models/EmuchartsPIMPrinter"),
        EmuchartsCppPrinter    = require("plugins/emulink/models/EmuchartsCppPrinter"),
        EmuchartsMALPrinter    = require("plugins/emulink/models/EmuchartsMALPrinter2"),
        EmuchartsVDMPrinter    = require("plugins/emulink/models/EmuchartsVDMPrinter"),
        EmuchartsJSPrinter     = require("plugins/emulink/models/EmuchartsJSPrinter"),
        EmuchartsAdaPrinter    = require("plugins/emulink/models/EmuchartsAdaPrinter"),
        EmuchartsBlessPrinter  = require("plugins/emulink/models/EmuchartsBlessPrinter"),
        EmuchartsMisraCPrinter = require("plugins/emulink/models/EmuchartsMisraCPrinter2"),
        EmuchartsAndroidPrinter  = require("plugins/emulink/models/EmuchartsAndroidPrinter"),
        EmuchartsNuXMVPrinter  = require("plugins/emulink/models/EmuchartsNuXMVPrinter"),
        EmuchartsICOPrinter    = require("plugins/emulink/models/EmuchartsICOPrinter"),
        EmuchartsAlloyPrinter  = require("plugins/emulink/models/EmuchartsAlloyPrinter2"),
        EmuchartsFMIPVSPrinter = require("plugins/emulink/models/EmuchartsFMIPVSPrinter");

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
        this.emuchartsAndroidPrinter = new EmuchartsAndroidPrinter("emucharts_Android");
        this.emuchartsNuXMVPrinter = new EmuchartsNuXMVPrinter("emucharts_NuXMV");
        this.emuchartsICOPrinter = new EmuchartsICOPrinter("emucharts_ICO");
        this.emuchartsAlloyPrinter = new EmuchartsAlloyPrinter("emucharts_Alloy");
        this.emuchartsFMIPVSPrinter = new EmuchartsFMIPVSPrinter("emucharts_FMIPVS");
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
