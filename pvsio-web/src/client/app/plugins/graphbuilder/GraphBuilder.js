/**
 *
 * @author Patrick Oladimeji
 * @date 11/22/13 9:03:14 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";
    const d3 = require("d3/d3");
    const PVSioWebClient = require("PVSioWebClient");
    const PluginManager = require("plugins/PluginManager").getInstance();
    const ProjectManager = require("project/ProjectManager");
    const normalize = require("util/Normalize").getInstance();

    let instance;
    let ws,
        nodesHash = {},
        edgesHash = {},
        w = 1130,
        h = 300,
        radius = 6,
        force,
        onGraphUpdate,
        canvas;

    function GraphBuilder() {
        var pvsioWebClient = PVSioWebClient.getInstance();
        ws  = pvsioWebClient.getWebSocket();
    }

    var name = "Interaction Log";

    GraphBuilder.prototype._init = function () {
        canvas = PVSioWebClient.getInstance().createCollapsiblePanel({ headerText: name, owner: this.getId(), showContent: true });
        canvas.classed("graph-container", true);
        var svg = canvas.append("svg").attr("width", w).attr("height", h).append("g")
            .call(d3.behavior.zoom().scaleExtent([0.4, 10]).on("zoom", function () {
                svg.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
            })).append("g");
        //create overlay so we can drag from anywhere on the canvas
        svg.append("rect").attr("class", "overlay").attr("height", h).attr("width", w)
            .on("mousedown", function () {
                d3.select(this).style("cursor", "move");
            }).on("mouseup", function () { d3.select(this).style("cursor", null); });
        var tooltip = canvas.append("div").attr("id", "tooltip");
        force = d3.layout.force().size([w, h]).linkDistance(20).nodes([]).links([]).charge(-150)
            .on("tick", function () {
                svg.selectAll("circle.node").attr("cx", function (d) {
                    return d.x;
                }).attr("cy", function (d) {
                    return d.y;
                });
                svg.selectAll("line.edge").attr("x1", function (d) { return d.source.x; })
                    .attr("y1", function (d) { return d.source.y; })
                    .attr("y2", function (d) {return d.target.y; })
                    .attr("x2", function (d) {return d.target.x; });

            });

        function updateGraph(nodes, edges, currentEdge) {
            svg.selectAll("line.edge").data(edges).enter()
                .insert("line", "circle.node").attr("class", "edge")
                .attr("x1", function (d) { return d.source.x; })
                .attr("y1", function (d) { return d.source.y; })
                .attr("y2", function (d) {return d.target.y; })
                .attr("y1", function (d) {return d.target.x; });
            var nodeEls = svg.selectAll("circle.node").data(nodes);
            nodeEls.enter()
                .append("circle")
                .attr("cx", function (d) {return d.x; })
                .attr("cy", function (d) {return d.y; })
                .attr("r", radius)
                .attr("class", function (d, i) {
                    var c = i === 0 ? "root" : "";
                    return "node " + c;
                })
                .on("mouseover", function (d, i) {
                    d3.event.preventDefault();
                    d3.event.stopPropagation();
                    tooltip.html(d.name).style("top", (d3.event.layerY + 10) + "px")
                        .style("left", (d3.event.layerX + 10) + "px").style("display", null);
                }).on("mouseout", function (d, i) {
                    tooltip.style("display", "none");
                }).call(force.drag);
            nodeEls.classed("selected", function (d, i) {
                return currentEdge.target === i;
            });
            force.start();
        }

        onGraphUpdate = function (event) {
            var transition = event.transition,
                source = event.source,
                target = event.target,
                edge,
                edgeString,
                nodes = force.nodes(),
                edges = force.links();
            if (nodesHash[source] === undefined) {
                nodesHash[source] = nodes.push({name: source}) - 1;
            }
            if (nodesHash[target] === undefined) {
                nodesHash[target] = nodes.push({name: target}) - 1;
            }
            edge = {source: nodesHash[source], target: nodesHash[target], transition: transition};
            edgeString = JSON.stringify(edge);
            if (!edgesHash[edgeString]) {
                edgesHash[edgeString] = edges.push(edge) - 1;
            }

            updateGraph(nodes, edges, edge);
        };

        ws.addListener("GraphUpdate", onGraphUpdate);
    };

    function clear() {
        canvas.html("");
        ws.removeListener("GraphUpdate", onGraphUpdate);
        nodesHash = {};
        edgesHash = {};
    }


    GraphBuilder.prototype.getName = function () {
        return name;
    };

    GraphBuilder.prototype.getId = function () {
        return normalize.removeSpaceDash(name);
    };

    GraphBuilder.prototype.reInitialise = function () {
        this.unload();
        this._init();
    };

    GraphBuilder.prototype.initialise = function () {
        var gb = this;
        this._init();
        ProjectManager.getInstance()
            .addListener("ProjectChanged", function (event) {
                if (PluginManager.isLoaded(gb)) {
                    gb.reInitialise();
                }
            });
        return Promise.resolve(true);
    };

    GraphBuilder.prototype.unload = function () {
        clear();
        PVSioWebClient.getInstance().removeCollapsiblePanel(canvas);
        return Promise.resolve(true);
    };

    GraphBuilder.prototype.getDependencies = function () {
        return [];
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new GraphBuilder();
            }
            return instance;
        }
    };
});
