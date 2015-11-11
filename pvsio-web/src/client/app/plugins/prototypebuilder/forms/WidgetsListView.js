/**
 * View that renders the list of widgets currently on the simulator
 * @author Patrick Oladimeji
 * @date 9/17/14 14:40:29 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, Event*/
define(function (require, exports, module) {
    "use strict";
    var WidgetManager = require("pvsioweb/WidgetManager").getWidgetManager();

    function WidgetsListView(widgets) {
        var el = d3.select("#widgetsList").html("").append("ul");

        function labelFunction(widget) {
            var label = widget.type() + ": ";
            if (widget.type() === "display") {
                label += widget.displayKey();
            } else { label += widget.functionText(); }
            return label;
        }

        function update(data) {
            var listItems = el.selectAll("li.list-group-item").data(data, function (widget) {
                return widget.id();
            });
            var enteredItems = listItems.enter();
            var exitedItems = listItems.exit();
            enteredItems.append("li").attr("class", "list-group-item")
                .attr("widget-id", function (w) {
                    d3.selectAll("#widgetsList ul li").classed("selected", false);
                    return w.id();
                }).classed("selected", true)
                .text(labelFunction)
                .on("click", function (w) {
                    var event = d3.event;
                    if (!event.shiftKey && !d3.select(this).classed("selected")) {
                        d3.selectAll("g.selected").classed("selected", false);
                        d3.selectAll("#widgetsList ul li").classed("selected", false);
                    } else if (d3.select(w.parentGroup()).classed("selected")) {
                        d3.selectAll(".subselected").classed("subselected", false);
                        d3.select(this).classed("subselected", true);
                        d3.select(w.parentGroup()).classed("subselected", true);
                    }
                    d3.select(this).classed("selected", true);
                    d3.select(w.parentGroup()).classed("selected", true);
                    event.preventDefault();
                    event.stopPropagation();
                }).on("dblclick", function (w) {
                    var event = d3.event;
                    var dblclick = new Event("dblclick");
                    w.element().node().dispatchEvent(dblclick);
                    event.preventDefault();
                    event.stopPropagation();
                });
            listItems.text(labelFunction);
            exitedItems.transition().duration(220).style("opacity", 0).remove();
        }

        update(widgets);
        el.selectAll("li.list-group-item").classed("selected", false);

        WidgetManager.addListener("WidgetModified", function (event) {
            switch (event.action) {
            case "create":
                widgets.push(event.widget);
                break;
            case "remove":
                var index = widgets.indexOf(event.widget);
                if (index > -1) {
                    widgets.splice(index, 1);
                }
                break;
            default:
                break;
            }
            update(widgets);
        }).addListener("WidgetSelected", function (event) {
            var e = new Event("click");
            e.shiftKey = event.event.shiftKey;
            var node = el.select("li[widget-id='" + event.widget.id() + "']").node();
            node.dispatchEvent(e);
        }).addListener("WidgetSelectionCleared", function (event) {
            d3.selectAll("#widgetsList ul li").classed("selected", false);
        });
    }


    module.exports = {
        create: function () {
            var widgets = WidgetManager.getAllWidgets();
            return new WidgetsListView(widgets);
        }
    };
});
