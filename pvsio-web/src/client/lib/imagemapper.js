/**
 * Utility library for creating image map regions for interactive prototyping
 * @author Patrick Oladimeji
 * @contributors Paolo Masci
 * @date 10/21/13 21:42:17 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3, require, $, module, self, window, MouseEvent, setInterval, clearInterval*/
(function () {
    "use strict";
    var helperPos = ['tl', 'tr', 'br', 'bl'], hw = 5, cornerOffset = hw / 2,
        helperData = helperPos.map(function (d) {return {x: cornerOffset, y: cornerOffset, align: d}; });
    var imageIsLoaded = false;
    /**
        get client rect for the given element
    */
    function cr(el) {
        return {
            height: el.node().height,
            width: el.node().width
        };
    }

    function pos(el) {
        var x = parseFloat(el.attr("x")), y = parseFloat(el.attr("y")), w = parseFloat(el.attr("width")),
            h = parseFloat(el.attr("height"));
        return {x: x, y: y, height: h, width: w};
    }
    /**
        utility function to check the scale of an svg g element
    */
    function scale(svgel) {
        var s = svgel.attr("transform");
        if (s.indexOf("scale") > -1) {
            return +(s.replace("scale", "").replace("(", "").replace(")", ""));
        } else { return 1; }
    }

    function select(region, svg, add, dispatcher) {
        var g = d3.select(region.node().parentNode);

        // correct handling of mouse events requires moving the selected region on top of the others
        d3.selection.prototype.moveToFront = function() {
          return this.each(function(){
            this.parentNode.appendChild(this);
          });
        };
        g.moveToFront();

        //remove previous selections if shift key wasnt pressed and we are not selecting a previously selected region
        if (!add && !g.classed("selected")) {
            svg.selectAll("g.selected").classed("selected", false);
        } else if (g.classed("selected")) {
            svg.selectAll("g.subselected").classed("subselected", false);
            g.classed("subselected", true);
        }
        //higlight the region show it has been selected
        g.classed("selected", true);
        dispatcher.select({region: region, event: d3.event});
    }

    function updateRegion(r, d) {
        d.width = isNaN(d.width) || d.width === null ? parseFloat(r.attr("width")) : d.width;
        d.height = isNaN(d.height) || d.height === null ? parseFloat(r.attr("height")) : d.height;

        r.attr("width", Math.abs(d.width)).attr("height", Math.abs(d.height)).attr("x", d.x).attr("y", d.y);
        var g = d3.select(r.node().parentNode), corners = g.selectAll("rect.corner");
        //update corners
        var cdata = corners.data();
        cdata.forEach(function (cd, i) {
            cd.x = i === 0 || i === 3 ? d.x : d.x + Math.abs(d.width);
            cd.y = i === 0 || i === 1 ? d.y : d.y + Math.abs(d.height);
            cd.align = helperPos[i];
        });
        corners.data(cdata).attr("x", function (d) {return d.x - cornerOffset; })
            .attr("y", function (d) {return d.y - cornerOffset; });
    }

    function enableRegionDrag(region, svg, dispatcher) {
        region.on("mousedown", function () {
            var _scale = scale(svg.select("g"));
            var mdPos = {x: d3.mouse(this)[0], y: d3.mouse(this)[1]},
                rxStart = +region.attr("x"),
                ryStart = +region.attr("y");
            region.attr("startx", rxStart).attr("starty", ryStart);
            //cache the start pos in each element
            svg.selectAll("g.selected .region").attr("startx", function () {
                return d3.select(this).attr("x");
            }).attr("starty", function () {
                return d3.select(this).attr("y");
            });
            d3.event.stopPropagation();
            d3.event.preventDefault();
            //register mousemove for the svg when moused down on the region
            svg.on("mousemove.region", function () {
                var e = {x: d3.mouse(this)[0] / _scale, y: d3.mouse(this)[1] / _scale};
                var delta = {x: (e.x - mdPos.x), y: (e.y - mdPos.y)};
                svg.selectAll("g.selected .region").each(function (d) {
                    var r = d3.select(this);
                    updateRegion(r, {x: (+r.attr("startx") + delta.x), y: (+r.attr("starty") + delta.y)}, false);
                });
            });

            region.on("mouseup", function () {
                svg.on("mousemove.region", null);
                dispatcher.move({region: region, pos: pos(region), scale: _scale});
            });

            select(region, svg, d3.event.shiftKey, dispatcher);
        });
    }

    function enableRegionResize(region, svg, dispatcher) {
        var g = d3.select(region.node().parentNode), corners = g.selectAll("rect.corner");
        corners.on("mousedown", function (d, i) {
            select(region, svg, d3.event.shiftKey, dispatcher);
            var _scale = scale(svg.select("g"));
            d3.event.preventDefault();
            d3.event.stopPropagation();
            var mdPos = {x: d3.mouse(this)[0], y: d3.mouse(this)[1]}, rx = +region.attr("x"),
                ry = +region.attr("y"), rw = parseFloat(region.attr("width")),
                rh = parseFloat(region.attr("height"));
            var w, h, x, y;
            svg.on("mousemove.corner", function () {
                d3.event.preventDefault();
                d3.event.stopPropagation();
                var mmPos = {x: d3.mouse(this)[0] / _scale, y: d3.mouse(this)[1] / _scale};
                var dx = mmPos.x - mdPos.x, dy = mmPos.y - mdPos.y;
                if (d.align === "tl") {
                    x = mmPos.x < (rx + rw) ? mmPos.x : (rx + rw);
                    y = mmPos.y < (ry + rh) ? mmPos.y : (ry + rh);
                    w = rw - dx;
                    h = rh - dy;
                } else if (d.align === "tr") {
                    y = mmPos.y < (ry + rh) ? mmPos.y : (ry + rh);
                    x = mmPos.x > rx ? rx : mmPos.x;
                    w = rw + dx;
                    h = rh - dy;
                } else if (d.align === "br") {
                    w = rw + dx;
                    h = rh + dy;
                    x = mmPos.x > rx ? rx : mmPos.x;
                    y = mmPos.y > ry ? ry : mmPos.y;
                } else if (d.align === "bl") {
                    x = mmPos.x < (rx + rw) ? mmPos.x : (rx + rw);
                    y = mmPos.y > (ry) ? ry : (mmPos.y);
                    w = rw - dx;
                    h = rh + dy;
                }
                d3.select(this).attr("x", mmPos.x).attr("y", mmPos.y);
                updateRegion(region, {x: x, y: y, width: w, height: h});
            });

            d3.select(this).on("mouseup", function (d, i) {
                svg.on("mousemove.corner", null);
                //dispatch move event
                dispatcher.resize({region: region, old: {x: rx, y: ry, width: rw, height: rh},
                                pos: pos(region), scale: _scale});
            });
        });
    }

    function createRegion(svg, startPos, dispatcher) {
        var currentSelections = svg.selectAll("g.selected");
        //clear previous selections
        if (!currentSelections.empty()) {
            dispatcher.clearselection({regions: currentSelections});
            currentSelections.classed("selected", false);
        }
        var g = svg.select("g").append("g").attr("class", "selected"), moved = false, moveRegionStarted = false;
        var region = g.append("rect").attr("x", startPos.x).attr("y", startPos.y).attr("class", "region");
        var corners = g.selectAll("rect.corner").data(helperData).enter()
            .append("rect").attr("class", function (d) { return d.align + " corner"; })
            .attr("width", hw).attr("height", hw);

        function sortPoints(a, b) { return a.y === b.y ? a.x - b.x : a.y - b.y; }

        var _scale = scale(svg.select("g"));
        svg.on("mousemove", function () {
            moved = true;
            var e = {x: d3.mouse(this)[0] / _scale, y: d3.mouse(this)[1] / _scale};
            //calculate the delta movement and update rect with and height
            var w = e.x - startPos.x, h = e.y - startPos.y, x = w < 0 ? startPos.x + w : startPos.x,
                y = h < 0 ? startPos.y + h : startPos.y;
            updateRegion(region, {x: x, y: y, height: h, width: w});
            d3.event.stopPropagation();
            d3.event.preventDefault();
        }).on("mouseup", function () {
            if (!moved) {
                g.remove();
            } else {
                dispatcher.create({region: region, pos: pos(region), scale: _scale});//dispatch create event
            }
            svg.on("mousemove", null)
                .on("mouseup", null);
        });

        //create move listener for region
        enableRegionDrag(region, svg, dispatcher);
        //create listener for corners
        enableRegionResize(region, svg, dispatcher);
        return region;
    }

    function booya(config) {
        if (!config || !config.element) { throw new Error("element property of config must be set"); }
        config.parent = config.parent || "body";
        config.scale = config.scale || 1;
        //clear any previous svgs
        d3.select(config.parent).select("svg").remove();
        var imageEl = d3.select(config.element), props, mapLayer, svg,
            ed = d3.dispatch("create", "remove", "resize", "move", "select", "clearselection"), initTimer;
        props = cr(imageEl);

        function initialiseSVGLayer() {
            props = cr(imageEl);
            svg = d3.select(config.parent).style("position", "relative")
                .append("svg").attr("width", props.width).attr("height", props.height).attr("class", "image-map-layer")
                .style("position", "absolute").style("cursor", "crosshair").style("top", 0).style("left", 0);

            mapLayer = svg.append("g").attr("transform", "scale(" + config.scale + ")");
        }

        function getImageMapData() {
            var regions = mapLayer.selectAll("rect.region");
            var map = [];
            regions.each(function () {
                var r = d3.select(this), coords = [r.attr("x"), r.attr("y"), r.attr("width"), r.attr("height")];
                var data = {shape: "rect", coords: coords.join(",")};
                map.push(data);
            });

            return map;
        }

        function restoreRectRegion(data) {
            var r = createRegion(svg, data, ed);
            d3.select(r.node().parentNode).classed("selected", false);
            updateRegion(r, data);
            svg.on("mousemove", null)
                .on("mouseup", null);
            return r;
        }

        function selectRegion(element, add) {
            select(element, svg, add, ed);
        }

        function clearRegions() {
            mapLayer.html("");
        }

        /**
         * Handles the given keyboard event, performing the appropriate action on the selected area.
         * Classes creating an imagemapper should listen for key events and call this function when an event occurs.
         * The imagemapper itself does not register key listeners on the page to allow for flexibility in when they are
         * triggered and to avoid collisions between multiple imagemapper instances.
         * @param {event} e JavaScript event for the key press
         */
        function handleKeyDownEvent(e) {
            if ((e.which === 46 || e.which === 8) && e.target === d3.select("body").node()) {
                ed.remove({regions: mapLayer.selectAll("g.selected rect.region")});
                e.preventDefault();
                e.stopPropagation();
            }
        }

        var res = {
            clear: function () {
                d3.select(config.parent).select("svg").remove();
            },
            restoreRectRegion: restoreRectRegion,
            getImageMapData: getImageMapData,
            selectRegion: selectRegion,
            clearRegions: clearRegions,
            handleKeyDownEvent: handleKeyDownEvent,
            on: function (type, f) {
                ed.on(type, f);
                return this;
            }
        };

        var loadImage = function () {
            initialiseSVGLayer();
            svg.on("mousedown", function () {
                var e = d3.event;
                var _scale = scale(svg.select("g"));
                createRegion(svg, {x: d3.mouse(this)[0] / _scale, y: d3.mouse(this)[1] / _scale}, ed);
                e.preventDefault();
            });
            //initialisation complete
            if (config.onReady) {
                config.onReady(res);
            }
        };
        loadImage();
    }

    if (typeof define === "function") {
        define(function (require, exports, module) {
            module.exports = booya;
        });
    } else if (typeof module === "undefined") {
        self.mapper = booya;
    } else {
        module.exports = booya;
    }
}());
