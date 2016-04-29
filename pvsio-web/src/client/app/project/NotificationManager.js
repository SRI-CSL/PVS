/**
 * @module NotificationManager
 * @description Specifies files and functionalities of a PVSio-web project.
 * @version 1.0
 * @author Patrick Oladimeji
 * @date 9/12/14 13:38:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";
    var Logger = require("util/Logger");
    var notifications = [], fadeDuration = 10000;
    function _showSelfDestructingNotification() {
        var notifyDiv = d3.select("#project-notifications"),
            alertDiv = notifyDiv.select("div.alert");
        notifyDiv.style("display", "block");
        notifyDiv.transition().duration(200).style("margin-top", "0px")
            .each("end", function () {
                notifyDiv.transition().delay(fadeDuration).duration(200)//.style("margin-top", (-1 * height) + "px")
                    .style("display", "none")
                    .each("start", function () {
                        if (notifyDiv.node().getBoundingClientRect().height < 1) {
                            notifyDiv.interrupt().transition();
                        }
                    })
                    .each("end", function () {
                        notifyDiv.style("display", "none");
                        alertDiv.remove();
                        notifyDiv.html("");
                    });
            });
    }

    // newest notifications are at the top
    function show(msg) {
        var date = new Date();
        notifications.push({time: date, message: msg});
        var notifyDiv = d3.select("#project-notifications").style("display", "block");
        var alertDiv = notifyDiv.insert("div", "div").attr("class", "alert alert-info").attr("role", "alert");
        alertDiv.append("button").attr("type", "button")
            .attr("class", "close").attr("data-dismiss", "alert")
            .append("span").attr("aria-hidden", "true").html("&times;");
        alertDiv.append("p").html(msg);
        Logger.log(date + "</br>" + msg);
        _showSelfDestructingNotification();
    }

    // newest notifications are at the top
    function error(msg) {
        var date = new Date();
        notifications.push({time: date, message: msg});
        var notifyDiv = d3.select("#project-notifications");
        var alertDiv = notifyDiv.insert("div", "div").attr("class", "alert alert-danger").attr("role", "alert");
        alertDiv.append("button").attr("type", "button")
            .attr("class", "close").attr("data-dismiss", "alert")
            .append("span").attr("aria-hidden", "true").html("&times;");
        alertDiv.append("p").html(msg);
        Logger.error(date + "</br>" + msg);
        _showSelfDestructingNotification();
    }

    module.exports = {
        show: show,
        error: error
    };
});
