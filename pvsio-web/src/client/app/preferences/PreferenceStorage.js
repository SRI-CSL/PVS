/**
  * Manages the persistence of the preference items. Persistence is currently done using localStorage in the browser.
  * Hence there is no need to explicitly call a save method after setting values.
  * @author Patrick Oladimeji
  * @date 25/06/2015 9:27:14 AM
  * @usage
  *     var Preferences = require("preferences/PreferenceStorage").getInstance();
  *     var PreferenceKeys = require("preferences/PreferenceKeys");
  *
  *     function updateBackupInterval(interval) {
  *         Preferences.set(PreferenceKeys.BACKUP_INTERVAL, interval);
  *     }
  */
define(function (require, exports, module) {
    "use strict";
    var eventDispatcher    = require("util/eventDispatcher"),
        DefaultPreferences = require("preferences/DefaultPreferences").getInstance(),
        PreferenceKeys     = require("preferences/PreferenceKeys");

    var instance;
    function PreferenceStorage() {  }

    PreferenceStorage.prototype.get  = function (key) {
        var res = localStorage.getItem(key);
        if (res !== undefined && res !== null) {
            res = JSON.parse(res);
        } else {
            res = DefaultPreferences.get(key);
        }

        return res;
    };

    PreferenceStorage.prototype.set = function (key, value) {
        localStorage.setItem(key, JSON.stringify(value));
        instance.fire({ type: "preferenceChanged", key: key, value: value});
    };

    PreferenceStorage.prototype.getAllPreferences = function () {
        var _this = this;
        var res = {};
        Object.keys(PreferenceKeys).forEach(function (k) {
            res[k] = _this.get(k);
        });
        return res;
    };

    module.exports = {
        getInstance: function () {
            instance = instance || eventDispatcher(new PreferenceStorage());
            return instance;
        }
    };
});
