/**
 * Utility function to be used by UI tests
 */

/*global d3, $, expect, define, Promise, Event*/
define(function (require, exports, module) {
    "use strict";

    var util = {
        wait: function (milliseconds) {
            return function () {
                return new Promise(function (resolve, reject) {
                    setTimeout(function () {
                        resolve("time up");
                    }, milliseconds);
                });
            };
        },

        click: function (elid) {
            return function () {
                var el = d3.select(elid).node();
                return new Promise(function (resolve, reject) {
                    if (el) {
                        el.click();
                        setTimeout(function () {
                            resolve(elid);
                        }, 500);
                    } else {
                        reject(elid + " does not exist in the dom");
                    }
                });
            };
        },

        rightclick: function (el) {
            return function () {
                var element = d3.select(el).node();
                var event = new Event("contextmenu");
                element.dispatchEvent(event);
                return new Promise(function (resolve) {
                    setTimeout(function () {
                        resolve(event);
                    }, 200);
                });
            };
        },

        /**
            Utility function to select context menu item (i.e., right click menu) on an element
            @param {string} el html element selector (e.g., class, tag or id) for the element
                recieving the right click
            @param {string} menu the html element selector for the menu item to select
            @returns {Promise} a promise that is settled when the context menu item has been clicked
        */
        listViewContextMenu: function (el, menu) {
            return function () {
                el = el || "#pvsFiles";
                return util.rightclick(el)()
                    .then(util.click(".contextmenu " + menu));
            };
        },

        expectError: function (done) {
            return function (err) {
                expect(err).toBeFalsy();
                done();
            };
        },

        dialogCanBeDismissed: function (btnTrigger, title, done) {
            title = title || "dialog triggered by " + btnTrigger;
            var clickbutton = util.click(btnTrigger);
            clickbutton()
                .then(util.click("div.overlay #btnCancel"))
                .then(function () {
                    //expect overlay to have disappeared after clicking cancel
                    expect($("div.overlay").length).toEqual(0);
                    done();
                }).catch(util.expectError(done));
        },

        pressingButtonOpensDialog: function (btnTrigger, title, done) {
            title = title || "dialog triggered by " + btnTrigger;
            var clickbutton = util.click(btnTrigger);
            clickbutton()
                .then(function () {
                    var dialogTitle = $("div.overlay .panel-heading").html();
                    expect(dialogTitle).toEqual(title);
                    done();
                }).catch(util.expectError(done));
        },

        openSampleProject: function (projectName) {
            var clickOpenProject = util.click("#openProject"),
                clickProject = util.click("button[data-project='{}']".format(projectName));
            return clickOpenProject()
                    .then(clickProject);
        },

        loadPlugin: function (pluginId) {
            var clickPlugin = util.click("#plugin_{}".format(pluginId));
            return clickPlugin();
        },

        unloadPlugin: function (pluginId) {
            var clickPlugin = util.click("#plugin_{}".format(pluginId));
            return clickPlugin()//to load
                    .then(clickPlugin);
        },

        /**
            used to toggle (expand/collapse) a collapsible panel
        */
        togglePanel: function (pluginName) {
            var el = "div.collapsible-panel-parent[plugin-owner='" + pluginName + "'] span.toggle-collapse";
            return util.click(el);
        }
    };

    module.exports = util;
});
