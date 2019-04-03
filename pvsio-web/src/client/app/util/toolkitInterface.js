/**
 * Binds user interface elements to events
 * @author Patrick Oladimeji
 * @date 11/15/13 16:29:55 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true*/
/*global define, d3, $, Backbone, Handlebars, layoutjs */
define(function (require, exports, module) {
    "use strict";

    const Logger = require("util/Logger");
    const PluginManager = require("plugins/PluginManager").getInstance();
    const PVSioWeb = require("PVSioWebClient").getInstance();
    const ProjectManager = require("project/ProjectManager");
    const PreferenceDialog = require("preferences/PreferenceDialog");
    const Preferences = require("preferences/PreferenceStorage").getInstance();

    const displayQuestion = require("pvsioweb/forms/displayQuestion");
    const toolkitTemplate = require("text!./ui.toolkitInterface.handlebars");


    /**
     * @private
     * Shows a prompt to the user signalling that the connection to the server has broken.
     * Also starts a polling timer to check if the server is back up and running. The dialog
     * is automatically dismissed when the server is restarted.
     * @param opt {Object} Function options: silentMode (Boolean) enables silent reconnection to the server, i.e., does not show any dialog.
     */
    function reconnectToServer(opt) {
        var timerid,
            q,
            data = {
                header: "Reconnect to server",
                question: ["The WebServer went offline... " +
                           "Please restart the WebServer by running ./restart.sh from the pvsio-web folder."],
                buttons: ["Dismiss", "Reconnect"]
            };

        //don't create a new question form if one already exists
        if (d3.select(".overlay").empty()) {
            if (!opt || (opt && !opt.silentMode)) {
                q = displayQuestion.create(data).on("reconnect", function (e, view) {
                    if (!PVSioWeb.isWebSocketConnected()) {
                        ProjectManager.getInstance().reconnectToServer()
                            .then(function () {
                                view.remove();
                                clearTimeout(timerid);
                            }).catch(function (err) {
                                view.remove();
                            });
                    } else {
                        view.remove();
                    }
                }).on("dismiss", function (e, view) {
                    view.remove();
                });
            }
        }
    }

    /**
     * @private
     * Called when the pvs process has been disconnected. It sets the appropriate UI markers
     * that signifies that the process is disconnected.
     * @param {object|string} err The error message or object returned from the server signifying why the process disconnected
     */
    function pvsProcessDisconnected(err) {
        var pvsioStatus = d3.select("#lblPVSioStatus");
        pvsioStatus.select("span").remove();
        Logger.log(err);
        pvsioStatus.classed("disconnected", true)
            .append("span").attr("class", "glyphicon glyphicon-warning-sign");
        //style("background", "red");
        PVSioWeb.isPVSProcessConnected(false);
    }
    /**
     * @private
     * Called when the pvs process has been connected. It sets the appropriate UI markers
     * that signifies that the process is connected and ready.
     */
    function pvsProcessReady() {
        var pvsioStatus = d3.select("#lblPVSioStatus");
        pvsioStatus.select("span").remove();
        var msg = "PVSio process ready!";
        Logger.log(msg);
        pvsioStatus.append("span").attr("class", "glyphicon glyphicon-ok");
        PVSioWeb.isPVSProcessConnected(true);
    }
    /**
     * @private
     * Called when the websocket connection to the server has been established. It sets the appropriate UI markers
     * that signifies that the websocket connection is active.
     */
    function webSocketConnected() {
        var el = d3.select("#lblWebSocketStatus");
        Logger.log("connection to pvsio server established");
        d3.select("#btnCompile").attr("disabled", null);
        el.classed("disconnected", false)
            .select("span").attr("class", "glyphicon glyphicon-ok");//style("background", "rgb(8, 88, 154)");
        PVSioWeb.isWebSocketConnected(true);
    }
    /**
     * @private
     * Called when the websocket connection to the server has been disconnected. It sets the appropriate UI markers
     * that signifies that the connection is disconnected. It also triggers the disconnection of the pvs process since
     * connection to the pvs process  depends on connection to the server.
     */
    function webSocketDisconnected() {
        var el = d3.select("#lblWebSocketStatus");
        Logger.log("connection to pvsio server closed");
        d3.select("#btnCompile").attr("disabled", true);
        el.classed("disconnected", true)
            .select("span").attr("class", "glyphicon glyphicon-warning-sign");//.style("background", "red");
        PVSioWeb.isWebSocketConnected(false);
        pvsProcessDisconnected("Websocket connection closed");
    }


    var ToolkitInterface = Backbone.View.extend({
        initialize: function (data) {
            this.render(data);
        },
        render: function (data) {
            let mainView = Handlebars.compile(toolkitTemplate);
            this.$el.html(mainView(data));
            $("body").append(this.el);
            layoutjs({el: "#content", useFullHeight: true});
            d3.select("#header").style("opacity", 0);
            return this;
        },
        events: {
            "click .plugin-box": "pluginClicked",
            "click .plugin-box label": "pluginLabelClicked",
            "click a#preferences": "preferencesClicked"
        },
        pluginClicked: function (event) {
            if (event.target.tagName.toLowerCase() === "li") {
                PluginManager.selectPlugin(event.target.id.replace("pluginBox_", ""));
            }
        },
        pluginLabelClicked: function (event) {
            if (event && event.target && typeof event.target.id === "string") {
                PluginManager.selectPlugin(event.target.id.replace("pluginLabel_", ""));
            }
        },
        scriptClicked: function (event) {
            this.trigger("scriptClicked", $(event.target).attr("name"));
        },
        preferencesClicked: function (event) {
            PreferenceDialog.create()
                .on("ok", function (form, view) {
                    Object.keys(form.data).forEach(function (k) {
                        Preferences.set(k, form.data[k]);
                    });
                    view.remove();
                }).on("cancel", function (form, view) {
                    view.remove();
                });
        }
    });

    function createHtmlElements (data) {
        return new ToolkitInterface(data);
    }

    module.exports = {
        init: function () {
            PluginManager.init();
            if (this._view) { this.unload(); }
            this._view = createHtmlElements({
                version: PVSioWeb.version()
            });
            return this._view;
        },
        unload: function () {
            this._view.remove();
        },
        webSocketConnected: function () {
            d3.select("#btnReconnect").style("display", "none");
            webSocketConnected();
        },
        webSocketDisconnected: function () {
            d3.select("#btnReconnect").style("display", "block");
            webSocketDisconnected();
        },
        pvsProcessConnected: function () {
            pvsProcessReady();
        },
        pvsProcessDisconnected: function (reason) {
            pvsProcessDisconnected(reason);
        },
        reconnectToServer: function () {
            reconnectToServer();
        }
    };
});
