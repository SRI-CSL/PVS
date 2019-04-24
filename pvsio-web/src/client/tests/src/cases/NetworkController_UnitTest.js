/** @module NetworkController_UnitTest */
/**
 * NetworkController_UnitTest is a test module for ProjectManager
 * @author Piergiuseppe Mallozzi
 * @date 09/07/15 4:09:12 PM
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, describe, expect, it, Promise, beforeAll, afterAll*///, xdescribe*/
define(function (require, exports, module) {
    "use strict";
    var instance;
    var main;
    var NCDevice;
    var NCMonitorCore;

    var ncMonitorCore;
    var ncDevice;


    function NetworkController_UnitTest() {
        main = require("main");
        NCMonitorCore = require("plugins/networkController/NCMonitorCore");
        NCDevice = require("plugins/networkController/ncDevice");

        return this;
    }


    /**
     * Connects a device and checks if the ncMonitor correctly detects its connection
     * @param deviceID
     * @param deviceType
     * @returns {Promise}
     */
    function deviceConnectionDetection(deviceID, deviceType){
        var url = window.location.origin.split(":").slice(0,2).join(":") + ":8080/NetworkController/devices";
        url = url.replace("http://", "ws://");
        ncDevice = new NCDevice({id: deviceID, type: deviceType}, { url: url });
        ncMonitorCore = new NCMonitorCore({extended: false});
        return new Promise(function (resolve, reject) {

            ncMonitorCore.addListener("action", function (event) {
                if (event.data.action === "connected") {
                    console.log(event.data.deviceID + " connected");
                    if (event.data.deviceID === deviceID) {
                        resolve(true);
                    }
                    else {
                        reject(false);
                    }
                }
            });

            ncMonitorCore.start().then(function(res){
                ncDevice.start().then(
                    function (res) {
                        ncDevice.connect();
                    }).catch(function (err) {
                        console.log(err);
                        reject(err);
                    });

            });
        });
    }



    // ----------------------------------------------------------------------------------------------------
    // ----------------------------------------------------------------------------------------------------
    // Project tests
    // ----------------------------------------------------------------------------------------------------
    // ----------------------------------------------------------------------------------------------------
    var connected = [];
    connected.push({
        description: "Testing Radical connecting and generating event...",
        run: function () {
            return new Promise(function (resolve, reject) {
                deviceConnectionDetection("Radical", "sp02 monitor").then(function (res){
                    resolve(res);
                }).catch(function(err){
                    reject(err);
                });
            });
        }
    });

    //connected.push({
    //    description: "Testing Alaris connecting and generating event...",
    //    run: function () {
    //        return new Promise(function (resolve, reject) {
    //            deviceConnectionDetection("Alaris", "infusion pump").then(function (res){
    //                resolve(res);
    //            }).catch(function(err){
    //                reject(err);
    //            });
    //        });
    //    }
    //});

    function runAllTests() {
        describe("NetworkController - Unit Test", function () {
            beforeAll(function (done) {
                //d3.select("div.overlay").remove();
                //main.start({noSplash: true}).then(function () {
                //    fs.mkDir("unit_test", {overWrite: true});
                    done();
                //});
            });

            afterAll(function (done) {
                //fs.rmDir("unit_test");
                done();
            });


            describe("Testing Device connections...", function () {
                connected.forEach(function (test) {
                    it(test.description, function (done) {
                        test.run().then(function (res) {
                            expect(res).toBeTruthy();
                            done();
                        }).catch(function (err) {
                            console.log(err);
                            expect(err).toBeUndefined();
                            done();
                        });
                    });
                });
            });
        });
    }

    NetworkController_UnitTest.prototype.run = function () {
        return runAllTests();
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new NetworkController_UnitTest();
            }
            return instance;
        },
        run: NetworkController_UnitTest.run
    };


});
