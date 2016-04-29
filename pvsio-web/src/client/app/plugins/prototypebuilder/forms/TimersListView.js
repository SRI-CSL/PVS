/**
 * Renders the list of timers in the prototype
 * @author Paolo Masci
 * @date 2015/04/21
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, Event*/
define(function (require, exports, module) {
    "use strict";
    var WidgetManager = require("pvsioweb/WidgetManager").getWidgetManager();

    function TimersListView(timers) {
        var el = d3.select("#timersList").html("").append("ul");

        function labelFunction(timer) {
            return " " + timer.timerEvent();
        }

        function update(data) {
            var listItems = el.selectAll("li.list-group-item").data(data, function (timer) {
                return timer.id();
            });
            var enteredItems = listItems.enter();
            var exitedItems = listItems.exit();
            enteredItems.append("li")
                .attr("class", function(t){
                    return "list-group-item glyphicon glyphicon-time";
                })
                .attr("timer-id", function (t) {
                    d3.selectAll("#timersList ul li").classed("selected", false);
                    return t.id();
                }).classed("selected", true)
                .text(labelFunction)
                .on("click", function (t) {
                    var event = d3.event;
                    if (!event.shiftKey && !d3.select(this).classed("selected")) {
                        d3.selectAll("g.selected").classed("selected", false);
                        d3.selectAll("#timersList ul li").classed("selected", false);
                    } else if (d3.select(t.parentGroup()).classed("selected")) {
                        d3.selectAll(".subselected").classed("subselected", false);
                        d3.select(this).classed("subselected", true);
                        d3.select(t.parentGroup()).classed("subselected", true);
                    }
                    d3.select(this).classed("selected", true);
                    d3.select(t.parentGroup()).classed("selected", true);
                    event.preventDefault();
                    event.stopPropagation();
                }).on("dblclick", function (t) {
                    WidgetManager.editTimer(t);
                    event.preventDefault();
                    event.stopPropagation();
                });
            listItems.text(labelFunction);
            exitedItems.transition().duration(220).style("opacity", 0).remove();
        }

        update(timers);
        el.selectAll("li.list-group-item").classed("selected", false);

        WidgetManager.addListener("TimerModified", function (event) {
            switch (event.action) {
            case "create":
                timers.push(event.timer);
                break;
            case "remove":
                var index = timers.indexOf(event.timer);
                if (index > -1) {
                    timers.splice(index, 1);
                }
                break;
            default:
                break;
            }
            update(timers);
        }).addListener("TimerSelected", function (event) {
            var e = new Event("click");
            e.shiftKey = event.event.shiftKey;
            var node = el.select("li[timer-id='" + event.timer.id() + "']").node();
            node.dispatchEvent(e);
        }).addListener("TimerSelectionCleared", function (event) {
            d3.selectAll("#timersList ul li").classed("selected", false);
        });
    }


    module.exports = {
        create: function () {
            var timers = WidgetManager.getAllTimers();
            return new TimersListView(timers);
        }
    };
});
