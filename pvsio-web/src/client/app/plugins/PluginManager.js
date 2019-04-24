/**
 * Manages the list of avalaible plugins
 * @author Patrick Oladimeji
 * @date 5/3/14 15:46:27 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";

    const eventDispatcher = require("util/eventDispatcher");
    const pluginTemplate = require("text!util/ui.pluginToggle.handlebars");
    const normalize = require("util/Normalize").getInstance();
    const pluginListFile = "plugins/plugins.json";

    let instance;

    let enabledPlugins = [];//contains instances of plugins
    let plugins = {};

    function jumpTo(h){
        // http://bl.ocks.org/mbostock/1649463
        function scrollTween(offset) {
          return function() {
            var i = d3.interpolateNumber(window.pageYOffset || document.documentElement.scrollTop, offset);
            return function(t) { scrollTo(0, i(t)); };
          };
        }
        if (document.getElementById(h)) {
            var top = document.getElementById(h).offsetTop - 60; // 60 is the height of the PVSio-web tool bar
            // window.scrollTo(0, top);
            d3.transition()
                .delay(100)
                .duration(500)
                .tween("scroll", scrollTween(top));
        }
    }

    class PluginManager {
        constructor () { eventDispatcher(this); }
        /**
         * @description Selects a plugin (i.e., scrolls the page down to the plugin)
         * @param { string | Object} plugin the plugin to be selected, given as plugin name, or plugin object
         */
        selectPlugin (plugin) {
            if (plugin) {
                if (typeof plugin === "string") {
                    jumpTo(plugin);
                } else if (typeof plugin.getName === "function") {
                    jumpTo(plugin.getName());
                }
            }
            return this;
        }
        /**
         * @description Enables a plugin
         * @param {object} plugin the plugin to be enabled
         * @returns {Promise} a promise that resolves when the plugin has been enabled
         */
        enablePlugin (plugin) {
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
                                $("#pluginToggle_" + plugin.getId()).toggles(true);
                                jumpTo(plugin.getId());
                                resolve(res);
                            }).catch(function (err) {
                                console.error(err);
                                reject(err);
                            });
                        }).catch(function (err) {
                            console.error(err);
                            reject(err);
                        });
                    } else {
                        plugin.initialise().then(function (res) {
                            $("#pluginToggle_" + plugin.getId()).toggles(true);
                            jumpTo(plugin.getId());
                            resolve(res);
                        }).catch(function (err) {
                            console.error(err);
                            reject(err);
                        });
                    }
                } else {
                    //plugin is already enabled
                    resolve(true);
                    jumpTo(plugin.getId());
                }
            });
        }
        /**
         * @description Disables a plugin.
         * @param {object} plugin the plugin to be disabled
         * @returns {Promise} a promise that resolves when the plugin has been disabled
         */
        disablePlugin (plugin) {
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
        }
        loadPlugin (pluginID, thePlugin, opt) {
            opt = opt || {};
            let pluginView = Handlebars.compile(pluginTemplate);
            d3.select("#" + pluginID).html(pluginView({
                id: pluginID,
                label: thePlugin.getInstance().getName()
            }));
            $('#pluginToggle_'+ pluginID).toggles();
            $('.toggle-inner').css("display", "flex"); // this is needed to fix a bug in jquery-toggles, which messes up the labels of the toggle buttons when zooming in/out in the browser
            $('#pluginToggle_'+ pluginID).on('toggle', function (e, active) {
                if (active) {
                    instance.enablePlugin(thePlugin.getInstance());
                } else {
                    instance.disablePlugin(thePlugin.getInstance());
                }
            });
            if (opt.autoload) {
                $('#pluginToggle_'+ pluginID).toggles(opt.autoload);
            }
            return this;
        }
        loadPlugins () {
            function load_aux(plugin) {
                return new Promise(function (resolve, reject) {
                    try {
                        require(["plugins/" + plugin.main], function (thePlugin) {
                            let pluginID = normalize.removeSpaceDash(plugin.id);
                            plugins[pluginID] = thePlugin;
                            let msg = plugin.id + " loaded successfully!";
                            console.log(msg);
                            if (document.getElementById("loading-info")) {
                                document.getElementById("loading-info").innerHTML = msg;
                            }
                            instance.loadPlugin(pluginID, thePlugin, { autoload: plugin.autoload });
                            resolve();
                        });
                    } catch (pluginError) {
                        console.log("unable to load " + plugin.main + " :((");
                    }
                });
            }
            function prepareHTML (pluginList) {
                pluginList.forEach(function (plugin) {
                    d3.select("#plugins-group").append("div").attr("id", normalize.removeSpaceDash(plugin.id));
                });
            }
            return new Promise(function (resolve, reject) {
                try {
                    console.log("loading plugins...");
                    var promises = [];
                    require(["text!" + pluginListFile], function (jsonFile) {
                        console.log(jsonFile);
                        let pluginList = JSON.parse(jsonFile);
                        prepareHTML(pluginList); // this is necessary to ensure the toolks are always displayed in the same order
                        pluginList.forEach(function (plugin) {
                            promises.push(
                                load_aux(plugin)
                            );
                        });
                        Promise.all(promises).then(function (res) {
                            resolve(res);
                        }).catch(function (err) {
                            reject(err);
                        });
                    });
                } catch (load_error) {
                    console.error(load_error);
                    return Promise.reject();
                }
            });
        }
        getEnabledPlugins () {
            return enabledPlugins;
        }
        isLoaded (p) {
            return enabledPlugins.indexOf(p) > -1 ? true : false;
        }
        init () {
            enabledPlugins = [];
            return this;
        }
    }
      
    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new PluginManager();
            }
            return instance;
        }
    };
});
