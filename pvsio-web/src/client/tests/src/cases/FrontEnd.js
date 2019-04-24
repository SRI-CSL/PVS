/**
 * front end test cases
 * @author Patrick Oladimeji
 * @date 6/24/14 16:35:39 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/

define(function (require, exports, module) {
    "use strict";

    var Project_UnitTest  = require("test/Project_UnitTest");
    var ProjectManager_UnitTest = require("test/ProjectManager_UnitTest").getInstance();
    var Descriptor_UnitTest = require("test/Descriptor_UnitTest");
    var UI_UnitTest = require("test/UI_UnitTest");
    var PIMPrototyping = require("test/pim-prototyping/PIMPrototyping");
    var EmuchartsParser_UnitTest = require("test/EmuchartsParser_UnitTest").getInstance();

    Project_UnitTest.run();
    ProjectManager_UnitTest.run();
    Descriptor_UnitTest.run();
    UI_UnitTest.run();
    PIMPrototyping.run();
    EmuchartsParser_UnitTest.run();
    //var NetworkController_UnitTest = require("test/NetworkController_UnitTest").getInstance();
    //NetworkController_UnitTest.run();
});
