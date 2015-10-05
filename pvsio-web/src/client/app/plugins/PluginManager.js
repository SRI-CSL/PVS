/**
 * Manages the list of avalaible plugins
 * @author Patrick Oladimeji
 * @date 5/3/14 15:46:27 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";
    var eventDispatcher = require("util/eventDispatcher");
    var enabledPlugins;//contains instances of plugins

    var instance;

    function PluginManager() {
        enabledPlugins = [];
        eventDispatcher(this);
    }

    /**
        Enables a plugin
        @param {object} plugin the plugin to enable
        @returns {Promise} a promise that resolves when the plugin has been enabled
    */
    PluginManager.prototype.enablePlugin = function (plugin) {
        var pm = this;
        return new Promise(function (resolve, reject) {
            if (enabledPlugins.indexOf(plugin) < 0) {
                enabledPlugins.push(plugin);
                instance.fire({type: "PluginEnabled", plugin: plugin});
                //initialise the plugin after loading and initialising any dependencies
                var dependencies = plugin.getDependencies();
                if (dependencies && dependencies.length) {
                    var depPromises = dependencies.filter(function (p) {
                        return !pm.isLoaded(p);
                    }).map(function (p) {
                        return pm.enablePlugin(p);
                    });
                    return Promise.all(depPromises).then(function () {
                        plugin.initialise().then(function (res) {
                            resolve(res);
                        }).catch(function (err) { reject(err); });
                    }).catch(function (err) { reject(err); });
                } else {
                    plugin.initialise().then(function (res) {
                        resolve(res);
                    }).catch(function (err) { reject(err); });
                }
            } else {
                //plugin is already enabled
                resolve(true);
            }
        });
    };

    /**
        Disables a plugin.
        @param {object} plugin the plugin to disable
        @returns {Promise} a promise that resolves when the plugin has been disabled
    */
    PluginManager.prototype.disablePlugin = function (plugin) {
        return new Promise(function (resolve, reject) {
            var index = enabledPlugins.indexOf(plugin);
            if (enabledPlugins.indexOf(plugin) > -1) {
                enabledPlugins.splice(index, 1);
                instance.fire({type: "PluginDisabled", plugin: plugin});
                return plugin.unload().then(function (res) {
                    resolve(res);
                }).catch(function (err) { reject(err); });
            } else {
                return resolve(true);
            }
        });
    };

    PluginManager.prototype.getEnabledPlugins = function () {
        return enabledPlugins;
    };

    PluginManager.prototype.isLoaded = function (p) {
        return enabledPlugins.indexOf(p) > -1 ? true : false;
    };

    PluginManager.prototype.init = function () {
        enabledPlugins = [];
        return this;
    };
    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new PluginManager();
            }
            return instance;
        }
    };
});
