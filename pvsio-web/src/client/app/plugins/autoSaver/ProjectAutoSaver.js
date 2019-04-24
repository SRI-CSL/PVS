/**
 * Plugin to automatically save projects every x minutes
 * @author Patrick Oladimeji
 * @date 12/8/14 10:51:56 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
define(function (require, exports, module) {
    "use strict";
    var instance, saveTimer, countdownTimer;
    var ProjectManager = require("project/ProjectManager"),
        Timer          = require("util/Timer"),
        Constants      = require("util/Constants"),
        PreferenceKeys = require("preferences/PreferenceKeys"),
        Preferences  = require("preferences/PreferenceStorage").getInstance();

    var saveInterval = Preferences.get(PreferenceKeys.BACKUP_INTERVAL) * 1000;
    function ProjectAutoSaver() {
        saveTimer = new Timer(saveInterval);
        countdownTimer = new Timer(saveInterval);
        if (window.location.origin.indexOf("pvsioweb.org") >= 0 ||
                window.location.origin.indexOf("pvsioweb.herokuapp.com") >= 0) {
            saveInterval = 0; // disabling autosave on heroku cloud
        }
    }

//    function toTimeString(t) {
//        var minutes = Math.floor(t / 60);
//        var secs = t - (minutes * 60);
//        if (minutes) {
//            if (secs) {
//                return minutes + "m " + secs + "s";
//            } else {
//                return minutes + "m";
//            }
//        } else {
//            return secs + "s";
//        }
//    }

    function reset() {
        saveTimer.reset().start();
        countdownTimer.reset().start();
    }

    ProjectAutoSaver.prototype.getName = function () {
        return "Project Auto Saver";
    };

    ProjectAutoSaver.prototype.getId = function () {
        return "ProjectAutoSaver";
    };

    ProjectAutoSaver.prototype.initialise = function () {
        if (saveInterval > 0) {
            ProjectManager.getInstance().addListener("ProjectSaved", function (e) {
                reset();
            }).addListener("ProjectChanged", function (e) {
                reset();
            });
            saveTimer.addListener("TimerTicked", function (e) {
                saveTimer.stop();
                countdownTimer.reset();
                var pm = ProjectManager.getInstance();
                var autosaveName = Constants.autosavePath + "/" + pm.project().name() +
                                "_autosave_" + (new Date().getFullYear()) + "." +
                                ("0" + (new Date().getMonth() + 1)).slice(-2) + "." +
                                ("0" + (new Date().getDate())).slice(-2) + "." +
                                ("0" + (new Date().getHours())).slice(-2) + "h";
    //            d3.select("#autoSaver").html("Saving backup copy in " + autosaveName + "...");
                pm.backupProject(autosaveName, { overWrite: true, silentMode: true }).then(function (res) {
                    saveTimer.reset().start();
                    countdownTimer.start();
                }).catch(function (err) { console.log(err); });
            }).start();
            countdownTimer.addListener("TimerTicked", function (e) {
                var timeToNextSave = saveInterval - (e.currentCount * 1000);
                timeToNextSave /= 1000;
//                var time = toTimeString(timeToNextSave);
    //            d3.select("#autoSaver").html("Project will auto-save in " + time);
            }).start();
        }
        return Promise.resolve(true);
    };

    ProjectAutoSaver.prototype.unload = function () {
        saveTimer.reset();
        countdownTimer.reset();
        d3.select("#autoSaver").html("");
    };

    ProjectAutoSaver.prototype.getDependencies = function () {
        return [];
    };

    module.exports = {
        getInstance: function () {
            instance = instance || new ProjectAutoSaver();
            return instance;
        }
    };
});
