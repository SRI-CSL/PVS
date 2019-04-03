/** @module EmuchartsEditor */
/**
 * EmuchartsEditor handles rendering of emuchart diagrams.
 * This is code is a re-engineered version of stateMachine.js implemented in branch emulink-commented
 * @author Paolo Masci
 * @date 14/05/14 5:16:13 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var d3 = require("d3/d3"),
        eventDispatcher = require("util/eventDispatcher"),
        //Emucharts = require("plugins/emulink/Emucharts"),
        EditorModeUtils = require("plugins/emulink/EmuchartsEditorModes");

    //layout manager
    // const elk = new ELK();
        
    var dbg = false;

    /**
     * Utility function to generate formatted labels for transitions
     * @returns labels, as a formatted string
     * @memberof EmuchartsEditor
     */
    var max_label_len = 96;
    function labelToString(label) {
        if (label.length > max_label_len) {
            return label.substring(0, max_label_len) + "...";
        }
        return label;
    }


    // constants for drawing states
//    var width  = 900;
//    var height = 800;
//    var colors = d3.scale.category10();
    var fontSize = 10;
    var defaultWidth = 32;
    var defaultHeight = 32;

    // constants for drawing transitions
    var stroke_width_large = "20px";
//    var stroke_width_highlighted = "1.5px";
    var stroke_width_normal = "1px";

    var drag_line; // drag line used when creating new transitions
    var sensitivity = { x: 8, y: 8 };
    // mouseOverControlPoint holds an edge object when the mouse is over the control point of an edge
    var mouseOverControlPoint = null;

    // editor modes
    var MODE = new EditorModeUtils();
    var editor_mode = MODE.BROWSE();

    function installHandlers(_this) {
        _this.emucharts.addListener("emuCharts_stateAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_stateRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_stateRenamed", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_stateColorChanged", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_constantAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_constantRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_constantRenamed", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_datatypeAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_datatypeRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_datatypeRenamed", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_variableAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_variableRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_variableRenamed", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_transitionAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_transitionRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_transitionRenamed", function (event) {
            _this.fire(event);
        });
        _this.emucharts.addListener("emuCharts_initialTransitionAdded", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_initialTransitionRemoved", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_initialTransitionRenamed", function (event) { _this.fire(event); });
        _this.emucharts.addListener("emuCharts_nodeFilterChanged", function (event) {
            _this._nodeFilter = event.filter;
            _this.render();
        });
    }

    /**
     * Constructor
     * @memberof EmuchartsEditor
     */
    function EmuchartsEditor(emucharts, opt) {
        opt = opt || {};
        this._nodeFilter = "";
        this.d3EventScale = 1;
        this.d3EventTranslate = [0, 0];
        this.mouseMovement = {
            ready: false,
            previousScreen: { x: 0, y: 0 },
            currentScreen: { x: 0, y: 0 }
        };
        if (!emucharts) {
            console.error("[Emucharts Editor] ERROR: null emucharts passed to constructor");
            return;
        }
        this.emucharts = emucharts;// || new Emucharts();
        this.dragged = false;
        this.SVGdragged = null;
        // mouse event vars used for identifying gestures like creating a new transition or dragging nodes/transitions/canvas
        this.mousedown = { node: null, edge: null, canvas: false };
        this.mouseover = { node: null, edge: null, canvas: false };
        this.mousedrag = { node: null, edge: null, canvas: false };
        this.container = opt.container || "#ContainerStateMachine";
        if (d3.select(this.container + " svg").node()) {
            d3.select(this.container).node().removeChild(d3.select(this.container + " svg").node());
        }
        installHandlers(this);
        eventDispatcher(this);
    }


    /**
     * Interface function for setting the editor mode
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.set_editor_mode = function (mode) {
        if (mode >= 0) {
            editor_mode = mode;
            var event = {type: "emuCharts_editorModeChanged", mode: editor_mode};
            this.fire(event);
        } else {
            console.log("dbg: warning, unknown editor mode " + mode);
            return;
        }
    };

    /**
     * Interface function for loading a new emucharts diagram in the editor
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.set_emucharts = function (emucharts) {
        if (emucharts) {
            this.emucharts = emucharts;
            installHandlers(this);
            this.fire({ type: "emuCharts_newEmuchartsLoaded" });
            return true;
        }
        return false;
    };


    /**
     * Utility function for trimming values so that they don't exceed a given range min-max
     * @returns trimmed value
     * @memberof EmuchartsEditor
     */
//    function trim(val, min, max) {
//        return (val < max) ? ((val > min) ? val : min) : max;
//    }
    function inc02(val, max) {
        return (val + 0.2 < max) ? val + 0.2 : val;
    }
    function dec02(val, min) {
        return (val - 0.2 > min) ? val - 0.2 : val;
    }

    /**
     * Interface functions for zooming in and out
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.zoomChart = function () {
        if (this.emucharts && !this.emucharts.empty()) {
            d3.select(this.container + " svg").select("#States")
                .attr("transform", "translate(" + this.d3EventTranslate + ") scale(" + this.d3EventScale + ")");
            d3.select(this.container + " svg").select("#Transitions")
                .attr("transform", "translate(" + this.d3EventTranslate + ") scale(" + this.d3EventScale + ")");
            d3.select(this.container + " svg").select("#InitialTransitions")
                .attr("transform", "translate(" + this.d3EventTranslate + ") scale(" + this.d3EventScale + ")");
            d3.select(this.container + " svg").select("#dragline")
                .attr("transform", "translate(" + this.d3EventTranslate + ") scale(" + this.d3EventScale + ")");
        }
    };
    EmuchartsEditor.prototype.zoom_in = function () {
        this.d3EventScale = inc02(this.d3EventScale, 4);
        this.zoomChart();
    };
    EmuchartsEditor.prototype.zoom_out = function () {
        this.d3EventScale = dec02(this.d3EventScale, 0.2);
        this.zoomChart();
    };
    EmuchartsEditor.prototype.zoom_reset = function () {
        this.d3EventScale = 1;
        this.zoomChart();
    };


    /**
     * Utility function to draw edges between nodes
     * @memberof EmuchartsEditor
     */
    var lineFunction = d3.svg.line()
                         .x(function (d) { return d.x; })
                         .y(function (d) { return d.y; })
                        //.interpolate("basic");
                         .interpolate("cardinal");

//    var lineFunction_bundle = d3.svg.line()
//                         .x(function (d) { return d.x; })
//                         .y(function (d) { return d.y; })
//                         .interpolate("bundle");


    /**
     * Utility function to obtain control points for edges
     * @params edge
     * @returns vector of control points (3 elements vector)
     * @memberof EmuchartsEditor
     */
    function getControlPoints(edge) {
        var sourceX = edge.source.x;
        var sourceY = edge.source.y;
        var targetX = edge.target.x;
        var targetY = edge.target.y;

        var offset = 0;
        var controlPoint1X = (edge.controlPoint) ? edge.controlPoint.x
                            : (edge.source.id === edge.target.id) ? (targetX + sourceX) / 2 + offset
                            : (targetX + sourceX) / 2 - offset;
        var controlPoint1Y = (edge.controlPoint) ? edge.controlPoint.y
                            : (edge.source.id === edge.target.id) ? (targetY + sourceY) / 2 + offset
                            : (targetY + sourceY) / 2 - offset;

        //var dx = edge.target.x - edge.source.x;
        //var dy = edge.target.y - edge.source.y;
        var dx_target = controlPoint1X - edge.target.x;
        var dy_target = controlPoint1Y - edge.target.y;
        var dx_source = controlPoint1X - edge.source.x;
        var dy_source = controlPoint1Y - edge.source.y;

        var targetWidth  = d3.select("#box_" + edge.target.id).attr("width");
        var targetHeight = d3.select("#box_" + edge.target.id).attr("height");
        var sourceWidth  = d3.select("#box_" + edge.source.id).attr("width");
        var sourceHeight = d3.select("#box_" + edge.source.id).attr("height");
//        var sourceWidth = edge.source.name.length * fontSize;
//        var sourceHeight = edge.source.height;
//        var targetWidth  = edge.target.name.length * fontSize;
//        var targetHeight = edge.target.height;
        var sourceWidth05 = sourceWidth / 2;
        var sourceHeight05 = sourceHeight / 2;
        var targetWidth05 = targetWidth / 2;
        var targetHeight05 = targetHeight / 2;


        // NOTE: SVG has the y axis inverted with respect to the Cartesian y axis
        if (dx_target >= targetWidth05 &&
                dy_target >= -targetHeight05 &&
                dy_target < targetHeight05) {
            //console.log("I");
            targetX += (targetWidth05 < 18) ? 18 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 - offset;
            }
        } else if (dx_target >= targetWidth05 &&
                dy_target >= targetHeight05) {
            //console.log("II");
            targetY += targetHeight05;
            targetX += (targetWidth05 < 18) ? 18 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target < targetWidth05 &&
                    dx_target >= -targetWidth05 &&
                    dy_target >= targetHeight05) {
            //console.log("III");
            targetY += targetHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target < targetWidth05 &&
                    dy_target >= targetHeight / 2) {
            //console.log("IV");
            targetX -= (targetWidth05 < 18) ? 18 : targetWidth05;
            targetY += sourceHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target < targetWidth05 &&
                    dy_target < targetHeight05 &&
                    dy_target >= -targetHeight05) {
            //console.log("V");
            targetX -= (targetWidth05 < 18) ? 18 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target < -targetWidth05 &&
                    dy_target < -targetHeight / 2) {
            //console.log("VI");
            targetX -= (targetWidth05 < 18) ? 18 : targetWidth05;
            targetY -= targetHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target < targetWidth05 &&
                    dx_target >= -targetWidth05 &&
                    dy_target < -targetHeight05) {
            //console.log("VII");
            targetY -= targetHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_target >= targetWidth05 &&
                    dy_target < -targetHeight05) {
            //console.log("VIII");
            targetY -= targetHeight05;
            targetX += (targetWidth05 < 18) ? 18 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
            }
        }

        //console.log("dx_source = " + dx_source + " dy_source = " + dy_source);
        if (dx_source >= sourceWidth05 &&
                dy_source >= -sourceHeight05 &&
                dy_source < sourceHeight05) {
            //console.log("I");
            sourceX += (sourceWidth05 < 18) ? 18 : sourceWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 - offset;
            }
        } else if (dx_source >= sourceWidth05 &&
                dy_source >= sourceHeight05) {
            //console.log("II");
            sourceY += sourceHeight05;
            sourceX += (sourceWidth05 < 18) ? 18 : sourceWidth05;
            if (!edge.controlPoint) {
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source < sourceWidth05 &&
                    dx_source >= -sourceWidth05 &&
                    dy_source >= sourceHeight05) {
            //console.log("III");
            sourceY += sourceHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source < sourceWidth / 2 &&
                    dy_source >= sourceHeight / 2) {
            //console.log("IV");
            sourceX -= (sourceWidth05 < 18) ? 18 : sourceWidth05;
            sourceY += sourceHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source < sourceWidth05 &&
                    dy_source < sourceHeight05 &&
                    dy_source >= -sourceHeight05) {
            //console.log("V");
            sourceX -= (sourceWidth05 < 18) ? 18 : sourceWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source < -sourceWidth05 &&
                    dy_source < -sourceHeight05) {
            //console.log("VI");
            sourceX -= (sourceWidth05 < 18) ? 18 : sourceWidth05;
            sourceY -= sourceHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source < sourceWidth05 &&
                    dx_source >= -sourceWidth05 &&
                    dy_source < -sourceHeight05) {
            //console.log("VII");
            sourceY -= sourceHeight05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
                controlPoint1Y = (targetY + sourceY) / 2 + offset;
            }
        } else if (dx_source >= sourceWidth05 &&
                    dy_source < -sourceHeight05) {
            //console.log("VIII");
            sourceY -= sourceHeight05;
            sourceX += (sourceWidth05 < 18) ? 18 : sourceWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2 + offset;
            }
        }

        return [{ "x": sourceX, "y": sourceY },
                { "x": controlPoint1X, "y": controlPoint1Y},
                { "x": targetX, "y": targetY }];
    }

    /**
     * Utility function to obtain virtual control points for self-edges
     * @params self-edge
     * @returns vector of control points (5 elements vector)
     * @memberof EmuchartsEditor
     * FIXME: return a structure rather than an array
     * FIXME: merge this function with the other function getControlPoints
     * FIXME: save extra control points so that the cpu time required for rendering is reduced
     */
    function getControlPoints_selfEdge(edge) {
        var sourceX = edge.source.x;
        var sourceY = edge.source.y;
        var targetX = edge.target.x;
        var targetY = edge.target.y;

        var controlPoint1X = (edge.controlPoint) ? edge.controlPoint.x : targetX + edge.target.name.length * fontSize;
        var controlPoint1Y = (edge.controlPoint) ? edge.controlPoint.y : targetY + edge.target.height * 1.4;
        edge.controlPoint = { x: controlPoint1X, y: controlPoint1Y };

        var extraControlPoints = [];
        //var dx = edge.target.x - edge.source.x;
        //var dy = edge.target.y - edge.source.y;
        var dx = edge.target.x - controlPoint1X;
        var dy = edge.target.y - controlPoint1Y;
        var offsetX = (dx < 0) ? -dx : dx;
        var offsetY = (dy < 0) ? -dy : dy;
        offsetX = (offsetX > edge.target.width) ? offsetX : edge.target.width / 2;
        offsetY = (offsetY > edge.target.height) ? offsetY : edge.target.height / 2;
        var targetWidth  = d3.select("#box_" + edge.target.id).attr("width");
        var targetHeight = d3.select("#box_" + edge.target.id).attr("height");
        var targetWidth05  = targetWidth / 2;
        var targetHeight05 = targetHeight / 2;

        // to identify control points, we split the space into four Cartesian quadrants:
        // the source node is at the center of the axes, and the target is in one of the quadrants
        //  II  |  I
        // -----s-----
        // III  |  IV
        // NOTE: SVG has the y axis inverted with respect to the Cartesian y axis
        if (dx >= 0 && dy < 0) {
            // target node is in quadrant I
            //console.log("Quadrant I");
            // for targets in quadrant I, round links draw convex arcs
            // --> place the arrow on the left side of the target
            targetX -= (targetWidth05 < 20) ? 20 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2;
                controlPoint1Y = (targetY + sourceY) / 2;
            }
            // move the first control point to quadrant II
            sourceY += targetHeight05;
            // create extra control points to avoid stiffy curves
            extraControlPoints[0] = { x: controlPoint1X + offsetX / 2,
                                      y: (offsetY > targetHeight) ?
                                            controlPoint1Y + offsetY / 4
                                            : controlPoint1Y + offsetY / 2 };
            extraControlPoints[1] = { x: (offsetX > targetWidth) ?
                                            controlPoint1X - offsetX / 16
                                            : controlPoint1X - offsetX / 4,
                                      y: controlPoint1Y - offsetY / 2 };
        } else if (dx < 0 && dy < 0) {
            //console.log("Quadrant II");
            // target node is in quadrant II
            // for targets in quadrant I, round links draw concave arcs
            // --> place the arrow at the bottom-right corner of the target
            targetY += targetHeight05;
            if (!edge.controlPoint) {
                controlPoint1Y = (targetY + sourceY) / 2;
            }
            // move the first control point to quadrant III
            sourceX += (targetWidth05 < 20) ? 20 : targetWidth05;
            // create extra control points to avoid stiffy curves
            extraControlPoints[0] = { x: (offsetX > targetWidth) ?
                                            controlPoint1X + offsetX / 16
                                            : controlPoint1X + offsetX / 4,
                                      y: controlPoint1Y - offsetY / 2 };
            extraControlPoints[1] = { x: controlPoint1X - offsetX * 0.6,
                                      y: (offsetY > targetHeight) ?
                                            controlPoint1Y - offsetY / 16
                                            : controlPoint1Y + offsetY / 2};
        } else if (dx < 0 && dy >= 0) {
            //console.log("Quadrant III");
            // target node is in quadrant III
            // for targets in quadrant IV, round links draw concave arcs
            // --> place arrow end on the top-right corner of the target
            targetX += (targetWidth05 < 20) ? 20 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2;
                controlPoint1Y = (targetY + sourceY) / 2;
            }
            // move the first control point to quadrant IV
            sourceY -= targetHeight05;
            // create extra control points to avoid stiffy curves
            extraControlPoints[0] = { x: controlPoint1X - offsetX / 2,
                                      y: (offsetY > targetHeight) ?
                                            controlPoint1Y - offsetY / 16
                                            : controlPoint1Y - offsetY / 2};
            extraControlPoints[1] = { x: controlPoint1X + offsetX / 16,
                                      y: controlPoint1Y + offsetY / 2 };
        } else if (dx >= 0 && dy >= 0) {
            //console.log("Quadrant IV");
            // target node is in quadrant IV
            // for targets in quadrant IV, round links draw convex arcs
            // --> place arrow end at the top-left corner of the target
            targetY -= targetHeight05;
            // move the first control point to quadrant I so that the self-edge looks round
            sourceX -= (targetWidth05 < 20) ? 20 : targetWidth05;
            if (!edge.controlPoint) {
                controlPoint1X = (targetX + sourceX) / 2;
            }
            // create extra control points to avoid stiffy curves
            extraControlPoints[0] = { x: controlPoint1X - offsetX / 16,
                                      y: controlPoint1Y + offsetY / 2};
            extraControlPoints[1] = { x: controlPoint1X + offsetX / 2,
                                      y: (offsetY > targetHeight) ?
                                            controlPoint1Y - offsetY / 4
                                            : controlPoint1Y - offsetY / 2 };// controlPoint1Y + offsetY / 16 };
        }

        return [{ "x": sourceX, "y": sourceY },
                { "x": extraControlPoints[0].x, "y": extraControlPoints[0].y },
                { "x": controlPoint1X, "y": controlPoint1Y},
                { "x": extraControlPoints[1].x, "y": extraControlPoints[1].y },
                { "x": targetX, "y": targetY }];
    }

    /**
     * Utility function to refresh rendered transitions.
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function refreshTransitions(_this, transitions, opt) {
        opt = opt || {};
        var prefix = opt.prefix || "";
        transitions = transitions ||
            d3.select(_this.container + " svg").select("#Transitions").selectAll(".transition");
        var label;
        var cpoints;
        // refresh paths and labels
        transitions.selectAll(".path").attr("d", function (edge) {
            // fetch control point
            var cp = null;
            edge = _this.emucharts.edges.get(edge.id); // important! always get info from the emucharts, rather than from the picture
            if (!edge) { return; }
            // refresh transition path
            if (edge.target && edge.source) {
                if (edge.target.id === edge.source.id) {
                    cp = getControlPoints_selfEdge(edge);
                    // this is a self-edge
                    // refresh transition label
                    label = d3.select(this.parentNode).select(".tlabel");
                    label.text(labelToString(edge.name));
                    // adjust text position
                    label.attr("x", function (edge) {
                        if (edge.target.id === edge.source.id) {
                            // self-edge
                            return (edge.controlPoint) ? edge.controlPoint.x + 18 : edge.source.x + 32;
                        }
                        // else do nothing -- textpath will take care of placing the text
                        return 0;
                    }).attr("y", function (edge) {
                        if (edge.target.id === edge.source.id) {
                            // self-edge
                            return (edge.controlPoint) ?
                                    (edge.source.y < edge.controlPoint.y) ? edge.controlPoint.y + 8 : edge.controlPoint.y
                                    : edge.source.y + 56;
                        }
                        // else do nothing -- textpath will take care of placing the text
                        return 0;
                    });
                    label = d3.select(this.parentNode).select(".tlabel_selection");
                    // adjust selection label position
                    label.attr("x", function (edge) {
                        if (edge.target.id === edge.source.id) {
                            // self-edge
                            return (edge.controlPoint) ? edge.controlPoint.x + 18 : edge.source.x + 32;
                        }
                        // else do nothing -- textpath will take care of placing the text
                        return 0;
                    }).attr("y", function (edge) {
                        if (edge.target.id === edge.source.id) {
                            // self-edge
                            var offsetY = d3.select("#" + prefix + "tlabel_" + edge.id).node().getBoundingClientRect().height;
                            var posY = (edge.controlPoint) ?
                                    (edge.source.y < edge.controlPoint.y) ? edge.controlPoint.y + 8 : edge.controlPoint.y
                                    : edge.source.y + 56;
                            return posY - offsetY;
                        }
                        // else do nothing -- textpath will take care of placing the text
                        return 0;
                    });
                    // clear the text of the other label
                    d3.select(this.parentNode.lastChild.firstChild).text(function (edge) { return ""; });
                    // refresh color, if needed
                    if (opt.color) {
                        d3.select(this.parentNode).select(".path").style("stroke", opt.color);
                        d3.select(this.parentNode).selectAll(".tlabel").style("fill", opt.color);
                        this.parentNode.firstChild.style.markerStart = this.parentNode.firstChild.style.markerStart.replace("-selected", "");
                        this.parentNode.firstChild.style.markerEnd = this.parentNode.firstChild.style.markerEnd.replace("-selected", "");
                    }
                    d3.select(this.parentNode).selectAll(".tlabel_selection").style("fill", "transparent");
                    // refresh control points
                    cpoints = d3.select(this.parentNode).select(".cpoints");
                    cpoints.attr("cx", cp[2].x).attr("cy", cp[2].y);
                    // refresh path
                    return lineFunction(cp);
                } else {
                    // not a self-edge
                    cp = getControlPoints(edge);
                    // refresh transition label
                    label = d3.select(this.parentNode).select("textpath");
                    label.text(labelToString(edge.name));
                    // clear the text of the other label
                    d3.select(this.parentNode).select(".tlabel").text(function (edge) { return ""; });
                    // refresh control points
                    cpoints = d3.select(this.parentNode).select(".cpoints");
                    // for now we use only the middle control point
                    cpoints.attr("cx", cp[1].x).attr("cy", cp[1].y);
                    // flip path if edge.source.x > edge.target.x
                    if (edge.source.id === edge.target.id ||
                            edge.source.x >= edge.target.x) {
                        // invert the direction of the path to avoid text being flipped upside down.
                        //The arrow pointer needs to be adjuted accordingly (see code below) otherwise the arrow direction will be inverted
                        var swap = cp[0];
                        cp[0] = cp[2];
                        cp[2] = swap;
                    }
                    d3.select(this.parentNode).select(".path")
                        .style("marker-start", function (edge) {
                            if (edge.source.id === edge.target.id ||
                                    edge.source.x >= edge.target.x) {
                                return "url(#end-arrow-rotated)";
                            } else { return ""; }
                        })
                        .style("marker-end", function (edge) {
                            if (edge.source.id === edge.target.id ||
                                    edge.source.x >= edge.target.x) {
                                return "";
                            } else { return "url(#end-arrow)"; }
                        });
                    // refresh color, if needed
                    if (opt.color) {
                        d3.select(this.parentNode.firstChild).style("stroke", opt.color);
                        d3.select(this.parentNode.children[3]).style("fill", opt.color);
                        d3.select(this.parentNode.children[4]).style("fill", opt.color);
                    }
                    // refresh path
                    return lineFunction(cp);
                }
            }
        });
        return transitions;
    }

    /**
     * Utility function to refresh rendered initial transitions.
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function refreshInitialTransitions(_this, transitions) {
        transitions = transitions ||
            d3.select(_this.container + " svg").select("#InitialTransitions").selectAll(".itransition");
        // refresh position
        transitions.attr("transform", function (edge) {
            return "translate(" + edge.target.x + "," + edge.target.y + ") scale(1)";
        });
        // refresh paths and labels
        transitions.selectAll(".ipath").attr("d", function (edge) {
            edge = _this.emucharts.initial_edges.get(edge.id); // important! always get info from the emucharts, rather than from the picture
            if (!edge) { return; }
            // refresh transition label
            var label = d3.select(this.parentNode).select(".itlabel");
            label.text(labelToString(edge.name));
            // adjust text position
            label.attr("x", function (edge) {
                return -8;
            }).attr("y", function (edge) {
                return -44;
            });
            // redraw edge
            return "M-32,-50 q 16 8 28 28";
        });
        return transitions;
    }

    /**
     * Utility function for removing transitions from the SVG
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function removeTransitions(exitedTransitions) {
        return exitedTransitions
                    .transition().duration(220)
                    .style("stroke-width", stroke_width_large)
                    .style("opacity", 0).remove();
    }

    /**
     * Utility function for creating an empty svg area and definitions
     * @param container reference to the DIV element that will contain the SVG. Only one container ("#ContainerStateMachine") will support editing, the others are lightweight SVGs for rendering a preview of the Emucharts diagram.
     * @returns reference to the transitions redrawn (svg element)
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.newSVG = function (container) {
        var _this = this;
        container = container || this.container || "#ContainerStateMachine";

        function getMouseMovement (event) {
            if (_this.mouseMovement.ready === false) {
                _this.mouseMovement.previousScreen.x = event.screenX;
                _this.mouseMovement.currentScreen.x = event.screenX;
                _this.mouseMovement.previousScreen.y = event.screenY;
                _this.mouseMovement.currentScreen.y = event.screenY;
                _this.mouseMovement.ready = true;
                return { x: 0, y: 0 };
            }
            _this.mouseMovement.previousScreen.x = _this.mouseMovement.currentScreen.x;
            _this.mouseMovement.previousScreen.y = _this.mouseMovement.currentScreen.y;
            _this.mouseMovement.currentScreen.x = event.screenX;
            _this.mouseMovement.currentScreen.y = event.screenY;
            return {
                x: (_this.mouseMovement.currentScreen.x - _this.mouseMovement.previousScreen.x),
                y: (_this.mouseMovement.currentScreen.y - _this.mouseMovement.previousScreen.y)
            };
        }

        // create canvas to be used for exporting svg area as picture
        d3.select("#ContainerStateMachineImage").append("canvas")
            .attr("style", "display: none");
        d3.select("#ContainerStateMachineImage").append("div")
            .attr("id", "svgdataurl").attr("class", "svgdataurl")
            .attr("style", "display: none");

        // create svg area
        d3.select(container)
            .append("svg")
            .attr("version", 1.1)
            .attr("xmlns", "http://www.w3.org/2000/svg")
            //.attr("xmlns:xlink", "http://www.w3.org/1999/xlink")
            .attr("width", "100%").attr("height", "100%")
            .style("background", "white")
            .append("svg:defs")
            .append("svg:marker")
            .attr("id", "end-arrow")
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 9)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto")
            .append("svg:path")
            .attr("d", "M4,0 L1,-3 L10,0 L1,3 L4,0")
            .attr("fill", "black");


//        var arrow_rotated =
        d3.select(container).select("svg").select("defs")
            .append("svg:marker")
            .attr("id", "end-arrow-rotated")
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 1)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto")
            .append("svg:path")
            .attr("d", "M1,0 L10,-3 L6,0 L10,3 L1,0")
            .attr("fill", "black");

//        var arrow_rotated_selected =
        d3.select(container).select("svg").select("defs")
            .append("svg:marker")
            .attr("id", "end-arrow-rotated-selected")
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 1)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto")
            .append("svg:path")
            .attr("d", "M1,0 L10,-3 L6,0 L10,3 L1,0")
            .attr("fill", "green");

//        var arrow =
        d3.select(container).select("svg").select("defs")
            // pointer for hiighlighed edges
            .append("svg:marker")
            .attr("id", "end-arrow-selected")
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 9)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto")
            .append("svg:path")
            .attr("d", "M4,0 L1,-3 L10,0 L1,3 L4,0")
            .attr("fill", "green");

        var bubble = d3.select(container).select("svg").select("defs")
            // bubble for initial state
            .append("svg:marker")
            .attr("id", "bubble")
            .attr("viewBox", "-5 -5 10 10")
            .attr("refX", -4)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto");
        bubble.append("svg:circle")
            .attr("r", 4)
            .attr("stroke", "black")
            .attr("fill", "black");
        bubble.append("svg:circle")
            .attr("r", 3.6)
            .attr("stroke", "white")
            .attr("stroke-width", "1");

        var selected_bubble = d3.select(container).select("svg").select("defs")
            // bubble for initial state
            .append("svg:marker")
            .attr("id", "bubble-selected")
            .attr("viewBox", "-5 -5 10 10")
            .attr("refX", -4)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto");
        selected_bubble.append("svg:circle")
            .attr("r", 4)
            .attr("stroke", "green")
            .attr("fill", "green");
        selected_bubble.append("svg:circle")
            .attr("r", 3.6)
            .attr("stroke", "white")
            .attr("fill", "green")
            .attr("stroke-width", "1");

        d3.select(container).select("svg").select("defs")
            // arrow for drag line
            .append("svg:marker")
            .attr("id", "drag-arrow")
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 9)
            .attr("markerWidth", 16)
            .attr("markerHeight", 16)
            .attr("orient", "auto")
            .append("svg:path")
            .attr("d", "M4,0 L1,-3 L10,0 L1,3 L4,0")
            .attr("fill", "black");

        drag_line = d3.select(container).select("svg")
            .append("svg:path")
            .attr("id", "dragline")
            .attr("class", "link dragline hidden")
            .attr("d", "M0,0L0,0");

        d3.select(container).select("svg").append("svg:g").attr("id", "InitialTransitions");
        d3.select(container).select("svg").append("svg:g").attr("id", "Transitions");
        d3.select(container).select("svg").append("svg:g").attr("id", "States");

        if (container === "#ContainerStateMachine") {
            var zoom = d3.behavior.zoom().scaleExtent([0.5, 4]).on("zoom", function () {
                if (_this.mousedown.canvas) {
                    d3.event.sourceEvent.stopPropagation();
                    if (editor_mode === MODE.ADD_TRANSITION() && _this.mousedrag.edge) {
                        var m = d3.mouse(d3.select("#ContainerStateMachine svg").select("#States").node());
                        // initial transition
                        drag_line.attr("d", "M" + _this.mousedrag.edge.x + "," + _this.mousedrag.edge.y +
                                        "L" + m[0] + "," + m[1]);
                    } else if (editor_mode !== MODE.ADD_TRANSITION() && !_this.mousedrag.node &&
                            editor_mode !== MODE.DELETE() && editor_mode !== MODE.RENAME()) {
                        d3.event.sourceEvent.stopPropagation();
                        var event = d3.event.sourceEvent;
                        if (event.type === "mousemove") {
        //                    note: movementX and movementY are not supported by all browser, therefore we are not using them for now
        //                    console.log(getMouseMovement(event));
        //                    var movementX = event.movementX || event.mozMovementX || event.webkitMovementX || 0;
        //                    var movementY = event.movementY || event.mozMovementY || event.webkitMovementY || 0;
        //                    _this.d3EventTranslate[0] += movementX * d3.behavior.zoom().scale();
        //                    _this.d3EventTranslate[1] += movementY * d3.behavior.zoom().scale();
        //                    console.log([movementX, movementY]);
                            var movement = getMouseMovement(event);
        //                    console.log(movement);
                            _this.d3EventTranslate[0] += movement.x * d3.behavior.zoom().scale();
                            _this.d3EventTranslate[1] += movement.y * d3.behavior.zoom().scale();
                            d3.select("#ContainerStateMachine svg").select("#States")
                                .attr("transform", "translate(" + _this.d3EventTranslate +
                                      ") scale(" + _this.d3EventScale + ")");
                            d3.select("#ContainerStateMachine svg").select("#Transitions")
                                .attr("transform", "translate(" + _this.d3EventTranslate +
                                      ") scale(" + _this.d3EventScale + ")");
                            d3.select("#ContainerStateMachine svg").select("#InitialTransitions")
                                .attr("transform", "translate(" + _this.d3EventTranslate +
                                      ") scale(" + _this.d3EventScale + ")");
                            d3.select("#ContainerStateMachine svg").select("#dragline")
                                .attr("transform", "translate(" + _this.d3EventTranslate +
                                      ") scale(" + _this.d3EventScale + ")");
                            if (_this.SVGdragged === null) {
                                _this.SVGdragged = [_this.d3EventTranslate[0], _this.d3EventTranslate[1]];
                            }
        //                    if (dbg) { console.log("Drag canvas"); }
                        }
                    }
                } else {
                    // disable zoom handler, so the web page can be scrolled
                    d3.select("#ContainerStateMachine svg").on(".zoom", null);
                }
            });
            var mouseUp = function () {
                if (editor_mode === MODE.ADD_TRANSITION()) {
                    // this is equivalent to drag end
                    // remove drag line
                    drag_line.classed("hidden", true)
                             .style("marker-end", "")
                             .style("marker-start", "")
                             .attr("d", "M0,0L0,0");
                    if (_this.mouseover.node && !_this.mousedrag.node) {
                        // fire event
                        _this.fire({
                            type: "emuCharts_addInitialTransition",
                            source: null,
                            target: _this.mouseover.node
                        });
                    }
                    _this.mousedrag.edge = null;
                }
                _this.mousedown.canvas = false;
                _this.mouseMovement.ready = false;
            };
            var mouseDown = function () {
                if (mouseOverControlPoint === null &&
                        editor_mode === MODE.ADD_TRANSITION()) {
                    d3.event.stopPropagation();
                    // this is equivalent to drag start for default-initial transitions
                    // create an arrow from the selected node to the cursor position
                    var m = d3.mouse(d3.select("#ContainerStateMachine svg").select("#States").node());
                    if (!_this.mousedrag.edge) {
                        _this.mousedrag.edge = { x: m[0], y: m[1] };
                    }
                    drag_line.classed("hidden", false)
                        .style("marker-end", "url(#drag-arrow)")
                        .style("marker-start", "url(#bubble)")
                        .attr("d", "M" + m[0] + "," + m[1] +
                                    "L" + m[0] + "," + m[1]);
                }
                _this.mousedown.canvas = true;
                _this.mouseMovement.ready = false;
            };
            var mouseClick = function () {
                if (editor_mode === MODE.ADD_STATE() && !_this.mouseover.node && !mouseOverControlPoint) {
                    if (_this.SVGdragged === null ||
                            (Math.abs(_this.SVGdragged[0] - _this.d3EventTranslate[0]) < sensitivity.x &&
                                 Math.abs(_this.SVGdragged[1] - _this.d3EventTranslate[1]) < sensitivity.y)) {
                        d3.event.stopPropagation();
                        var m = d3.mouse(d3.select("#ContainerStateMachine svg").select("#States").node());
                        _this.fire({
                            type: "emuCharts_addState",
                            mouse: m,
                            mouseover: _this.mouseover,
                            preventCreation: editor_mode !== MODE.ADD_STATE()
                        });
                    }
                }
                _this.SVGdragged = null;
                _this.mousedown.canvas = false;
                _this.mouseMovement.ready = false;
            };
            var mouseMove = function () {
                var m = d3.mouse(d3.select("#ContainerStateMachine svg").node());
    //            console.log(m);
    //            console.log(event);
                d3.selectAll("#MouseOverlayIcon").style("display", function () {
                    if ((this.getAttribute("placeholder") === "addStates" && editor_mode === MODE.ADD_STATE()) ||
                            (this.getAttribute("placeholder") === "addTransitions" && editor_mode === MODE.ADD_TRANSITION()) ||
                            (this.getAttribute("placeholder") === "rename" && editor_mode === MODE.RENAME()) ||
                            (this.getAttribute("placeholder") === "delete" && editor_mode === MODE.DELETE())) {
                        return "block";
                    }
                    return "none";
                });
                var style = "left: " + (m[0] + 20) + "px; top: " + (m[1] - 10) + "px;";
                d3.select("#MouseOverlay").attr("style", style);
                // enable zoom handler for scrolling the svg
                d3.select("#ContainerStateMachine svg").call(zoom);
            };
            var mouseOut = function () {
                d3.selectAll("#MouseOverlayIcon").style("display", "none");
                _this.mouseMovement.ready = false;
            };
            d3.select("#ContainerStateMachine svg")
                .on("click", mouseClick)
                .on("mousedown", mouseDown)
                .on("mouseup", mouseUp)
                .on("mousemove", mouseMove)
                .on("mouseout", mouseOut)
                .call(zoom);
        }

        // return reference to svg
        return d3.select(container).select("svg");
    };

    function moveToFront(id) {
        var g = d3.select("#" + id);
        if (g.node()) {
            d3.selection.prototype.moveToFront = function() {
              return this.each(function(){
                  if (this.parentNode) {
                    this.parentNode.appendChild(this);
                  }
              });
            };
            g.moveToFront();
        }
    }

    EmuchartsEditor.prototype.select_transition = function (id) {
        var g = d3.select("#" + id).node();
        if (g) {
            d3.select(g.firstChild)
                //.style("stroke-width", stroke_width_highlighted)
                .style("stroke", "green")
                .style("marker-end", function (edge) {
                    if (edge.source.id === edge.target.id ||
                            edge.source.x < edge.target.x) {
                        return "url(#end-arrow-selected)";
                    } else { return ""; }
                })
                .style("marker-start", function (edge) {
                    if (edge.source.id === edge.target.id) {
                        return g.style.markerStart;
                    } else if (edge.source.x < edge.target.x) {
                        return "";
                    } else { return "url(#end-arrow-rotated-selected)"; }
                });
            d3.select(g).select(".cpoints").attr("opacity", 0.6);
            d3.select(g).selectAll(".tlabel").style("fill", "green");
            d3.select(g).selectAll(".tlabel_selection").style("fill", "transparent");
            var edge = this.emucharts.edges.get(id);
            if (edge && edge.source && edge.target && edge.source.id === edge.target.id) {
                d3.select(g).select(".tlabel").text(function (edge) {
                    return edge.name;
                });
            }
            moveToFront(id);
        }
        return this;
    };

    EmuchartsEditor.prototype.deselect_transition = function (id) {
        var g = d3.select("#" + id).node();
        if (g) {
            d3.select(g.firstChild)
                //.style("stroke-width", stroke_width_normal)
                .style("stroke", "black")
                .style("marker-end", function (edge) {
                    if (edge.source.id === edge.target.id ||
                            edge.source.x < edge.target.x) {
                        return "url(#end-arrow)";
                    } else { return ""; }
                })
                .style("marker-start", function (edge) {
                    if (edge.source.id === edge.target.id) {
                        return g.style.markerStart;
                    } else if (edge.source.x < edge.target.x) {
                        return "";
                    } else { return "url(#end-arrow-rotated)"; }
                });
            d3.select(g).select(".cpoints").attr("opacity", 0);
            d3.select(g).selectAll(".tlabel").style("fill", "black");
            d3.select(g).selectAll(".tlabel_selection").style("fill", "transparent");
            var edge = this.emucharts.edges.get(id);
            if (edge && edge.source && edge.target && edge.source.id === edge.target.id) {
                d3.select(g).select(".tlabel").text(function (edge) {
                    return labelToString(edge.name);
                });
            }
        }
    };

    EmuchartsEditor.prototype.deselect_all_transition = function () {
        if (this.emucharts.edges && this.emucharts.edges.keys().length > 0) {
            var _this = this;
            this.emucharts.edges.keys().forEach(function (key) {
                _this.deselect_transition(key);
            });
        }
    };

    EmuchartsEditor.prototype.select_state = function (id) {
        moveToFront(id);
        return this;
    };

    /**
     * Utility function for drawing transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.renderTransitions = function (opt) {
        opt = opt || {};
        var _this = this;
        var container = _this.container || "#ContainerStateMachine";
        var svg = d3.select(container).select("svg");
        var prefix = opt.prefix || "";

        /**
         * Utility function for drawing transitions
         * @returns a reference to the entered transitions
         */
        var drawTransitions = function (enteredTransitions) {
            enteredTransitions = enteredTransitions.append("svg:g")
                .classed("transition", true)
                .attr("id", function (edge) { return prefix + edge.id; });

            // visiblePath is the actual path visible to the user
//            var visiblePath =
            enteredTransitions.append("svg:path").classed("path", true)
                .attr("id", function (edge) { return prefix + "path_" + edge.id; })
                .attr("fill", "none")
                .style("stroke", "black")
                .style("stroke-width", stroke_width_normal)
                .style("markerUnits", "userSpaceOnUse")
                .style("marker-end", "url(#end-arrow)");

            // selection path is used to ease selection with the mouse (it's wide)
//            var selectionPath =
            enteredTransitions.append("svg:path").classed("path", true)
                .attr("id", function (edge) {return prefix + "selectionPath_" + edge.id; })
                .style("opacity", "0")
                .attr("fill", "none")
                .style("stroke", "grey")
                .style("stroke-width", stroke_width_large)
                .style("markerUnits", "userSpaceOnUse")
                .style("cursor", "pointer");


            // labels are drawn using both text and textpath
            // the former is for self-edges, the latter for all other edges
//            var text =
            enteredTransitions.append("svg:text").classed("tlabel", true)
                .attr("id", function (d) { return prefix + "tlabel_" + d.id; })
                .style("font", (fontSize + "px sans-serif"))
                .style("text-rendering", "optimizeLegibility")
                .text(function (edge) {
                    if (edge.target.id === edge.source.id) {
                        // text for self edges is rendered as standard text field
                        return labelToString(edge.name);
                    }
                    // text for other edges is rendered as textpath
                    return "";
                });
            enteredTransitions.append("svg:rect").classed("tlabel_selection", true)
                .attr("id", function (d) { return prefix + "tlabel_selection_" + d.id; })
                .style("cursor", "pointer") // change cursor shape
                .style("width", function (edge) {
                    return d3.select("#" + prefix + "tlabel_" + edge.id).node().getBoundingClientRect().width + "px";
                }).style("height", function (edge) {
                    return d3.select("#" + prefix + "tlabel_" + edge.id).node().getBoundingClientRect().height + "px";
                });


//            var textPath =
            enteredTransitions.append("svg:text").classed("tlabel", true)
                .attr("id", function (edge) { return prefix + "tlabel_" + edge.id; })
                .style("font", (fontSize + "px sans-serif"))
                .style("text-rendering", "optimizeLegibility")
                .style("text-anchor", "middle")
                .attr("dy", -4)
                .append("textPath")
                .attr("xlink:href", function (edge) { return "#" + prefix + "path_" + edge.id; })
                .attr("startOffset", "50%")
                .style("cursor", "pointer") // change cursor shape
                .text(function (edge) {
                    if (edge.target.id === edge.source.id) {
                        // text for self edges is rendered as standard text field
                        return "";
                    }
                    // text for other edges is rendered here
                    return labelToString(edge.name);
                });

            // control points are used to adjust the shape of a path
//            var controlPoints =
            enteredTransitions.append("svg:circle").classed("cpoints", true)
                .attr("id", function (edge) { return prefix + "cpoints_" + edge.id; })
                .attr("cx", function (edge) {
                    if (edge.source && edge.target && edge.source.id === edge.target.id) {
                        return getControlPoints_selfEdge(edge)[2].x;
                    }
                    return getControlPoints(edge)[1].x;
                })
                .attr("cy", function (edge) {
                    if (edge.source && edge.target && edge.source.id === edge.target.id) {
                        return getControlPoints_selfEdge(edge)[2].y;
                    }
                    return getControlPoints(edge)[1].y;
                })
                .attr("fill", "#fff")
                .style("stroke", "steelblue")
                .style("stroke-width", "1.5px")
                .attr("opacity", 0)
                .attr("r", 10)
                .style("cursor", "move");


            return refreshTransitions(_this, enteredTransitions);
        };
        var mouseOver = function (edge) {
            if (!_this.mousedrag.edge && (!mouseOverControlPoint || mouseOverControlPoint.id === edge)) {
                d3.event.stopPropagation();
                _this.select_transition(edge.id);
                _this.fire({
                    type: "emuCharts_highlightTransition",
                    edge: edge
                });
            }
            if (dbg) { console.log("Transitions.mouseOver"); }
        };
        var mouseOut = function (edge) {
            if (!mouseOverControlPoint) {
                d3.event.stopPropagation();
                if (!d3.select("#emuCharts_rename_dialog").node()) {
                    _this.deselect_transition(edge.id);
                    _this.fire({
                        type: "emuCharts_deselectTransition",
                        edge: edge
                    });
                }
            } else if (mouseOverControlPoint && mouseOverControlPoint.id === edge.id) {
                var m = d3.mouse(d3.select("#ContainerStateMachine svg").select("#States").node());
                // update selected control point
                var cp = { x: m[0], y: m[1] };
                _this.emucharts.set_controlPoint(mouseOverControlPoint, cp);
                var transitionID = mouseOverControlPoint.id;
                var transitions = d3.select("#ContainerStateMachine")
                                    .select("#Transitions").selectAll(".transition")
                                    .filter(function (transition) { return transition.id === transitionID; });
                // refresh transitions
                return refreshTransitions(_this, transitions);
            }
            if (dbg) { console.log("Transitions.mouseOut"); }
        };
        var mouseClick = function (edge) {
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.stopPropagation();
            // update mouse variables
            _this.mousedown.edge = edge;
            if (editor_mode === MODE.RENAME()) {
                _this.fire({
                    type: "emuCharts_renameTransition",
                    edge: edge
                });
            } else if (editor_mode === MODE.DELETE()) {
                _this.fire({
                    type: "emuCharts_deleteTransition",
                    edge: edge
                });
            } else {
                _this.fire({
                    type: "emuCharts_selectTransition",
                    edge: edge
                });
            }
            if (dbg) { console.log("Transitions.mouseClick"); }
        };
        var mouseDown = function (edge) {
            d3.event.stopPropagation();
            // CAREFUL: do not attempt to select the transition if selection is also performed onMouseOver -- this will prevent dblclick events from firing
            // if (svg.node().children.length > 0) {
            //     _this.select_transition(edge.id);
            // }
        };
        var dragStart = function (node) {
            if (dbg) { console.log("Transitions.dragStart"); }
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.sourceEvent.stopPropagation();
        };
        var dragEdge = function (edge) {
            if (mouseOverControlPoint) {
                // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
                d3.event.sourceEvent.stopPropagation();
                var m = d3.mouse(d3.select("#ContainerStateMachine svg").select("#States").node());
                // update selected control point
                var cp = { x: m[0], y: m[1] };
                _this.emucharts.set_controlPoint(mouseOverControlPoint, cp);
                var transitionID = mouseOverControlPoint.id;
                var transitions = d3.select("#ContainerStateMachine")
                                    .select("#Transitions").selectAll(".transition")
                                    .filter(function (transition) { return transition.id === transitionID; });
                // refresh transitions
                return refreshTransitions(_this, transitions);
            }
            if (dbg) { console.log("Transitions.dragEdge"); }
        };
        var dragEnd = function (node) {
            if (dbg) { console.log("Transitions.dragEnd"); }
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.sourceEvent.stopPropagation();
        };
        function transitionOnMouseDoubleClick(_this) {
            return function (edge) {
                if (editor_mode !== MODE.DELETE()) {
                    d3.event.stopPropagation();
                    _this.fire({
                        type: "emuCharts_renameTransition",
                        edge: edge
                    });
                }
                if (dbg) { console.log("Transitions.mouseDoubleClick"); }
            };
        }

        if (!this.emucharts || !this.emucharts.getEdges()) { return; }
        // create svg element, if needed
        if (svg.empty()) { svg = this.newSVG(); }
        var edges = this.emucharts.getEdges().values().filter(_this._edgeFilterFunction());
        if (edges) {
            // create a group of svg elements for transitions, and bind them to data
            var transitions = svg.select("#Transitions").selectAll(".transition")
                                    .data(edges, function (edge) { return edge.id; });
            var enteredTransitions = drawTransitions(transitions.enter());
//            var exitedTransitions =
            removeTransitions(transitions.exit());
            if (container === "#ContainerStateMachine") {
                var drag = d3.behavior.drag().origin(function (edge) {
                    return edge;
                });
                drag.on("dragstart", dragStart)
                    .on("drag", dragEdge)
                    .on("dragend", dragEnd);
                enteredTransitions.call(drag)
                    .on("mousedown", mouseDown)
                    .on("mouseover", mouseOver)
                    .on("mouseout", mouseOut)
                    .on("click", mouseClick)
                    .on("dblclick", transitionOnMouseDoubleClick(_this));

                enteredTransitions.selectAll(".cpoints")
                    .on("mousedown", function (d) {
                        if (dbg) { console.log("mouseOverControlPoint"); }
                        mouseOverControlPoint = d;
                    }).on("mouseup", function (d) {
                        if (dbg) { console.log("mouseLeavingControlPoint"); }
                        mouseOverControlPoint = null;
                    });
            }
        }
    };


    /**
     * Utility function for drawing initial transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.renderInitialTransitions = function () {
        var _this = this;
        var container = _this.container || "#ContainerStateMachine";
        var svg = d3.select(container).select("svg");

        /**
         * Utility function for drawing transitions
         * @returns a reference to the entered transitions
         */
        var drawInitialTransitions = function (enteredTransitions) {
            enteredTransitions = enteredTransitions.append("svg:g")
                .classed("itransition", true)
                .attr("id", function (edge) { return edge.id; });

            // visiblePath is the actual path visible to the user
//            var visiblePath =
            enteredTransitions.append("svg:path").classed("ipath", true)
                .attr("id", function (edge) { return "ipath_" + edge.id; })
                .attr("fill", "none")
                .style("stroke", "black")
                .style("stroke-width", stroke_width_normal)
                .style("markerUnits", "userSpaceOnUse")
                .style("marker-start", "url(#bubble)")
                .style("marker-end", "url(#end-arrow)");

            // selection path is used to ease selection with the mouse (it's wide)
//            var selectionPath =
            enteredTransitions.append("svg:path").classed("ipath", true)
                .attr("id", function (edge) {return "iselectionPath_" + edge.id; })
                .style("opacity", "0")
                .attr("fill", "none")
                .style("stroke", "grey")
                .style("stroke-width", stroke_width_large)
                .style("markerUnits", "userSpaceOnUse")
                .style("cursor", "pointer");

            // labels are drawn using both text and textpath
            // the former is for self-edges, the latter for all other edges
//            var text =
            enteredTransitions.append("svg:text").classed("itlabel", true)
                .attr("id", function (d) { return "itlabel_" + d.id; })
                .style("font", (fontSize + "px sans-serif"))
                .style("text-rendering", "optimizeLegibility")
                .style("cursor", "pointer") // change cursor shape
                .text(function (edge) {
                    return labelToString(edge.name);
                });

            return refreshInitialTransitions(_this, enteredTransitions);
        };
        var mouseOver = function (edge) {
            d3.select(this.firstChild)
                //.style("stroke-width", stroke_width_highlighted)
                .style("stroke", "green")
                .style("marker-start", "url(#bubble-selected)")
                .style("marker-end", "url(#end-arrow-selected)");
            d3.select(this.children[2]).style("fill", "green");
        };
        var mouseOut = function (edge) {
            //d3.select(this.firstChild).style("stroke-width", stroke_width_normal);
            d3.select(this.firstChild)
                //.style("stroke-width", stroke_width_highlighted)
                .style("stroke", "black")
                .style("marker-start", "url(#bubble)")
                .style("marker-end", "url(#end-arrow)");
            d3.select(this.children[2]).style("fill", "black");
        };
        var mouseClick = function (edge) {
            // update mouse variables
            _this.mousedown.edge = edge;
            if (editor_mode === MODE.RENAME()) {
                _this.fire({
                    type: "emuCharts_renameInitialTransition",
                    edge: edge
                });
            } else if (editor_mode === MODE.DELETE()) {
                _this.fire({
                    type: "emuCharts_deleteInitialTransition",
                    edge: edge
                });
            }
        };
        function initialTransitionOnMouseDoubleClick(_this) {
            return function (edge) {
                if (editor_mode !== MODE.DELETE()) {
                    _this.fire({
                        type: "emuCharts_renameInitialTransition",
                        edge: edge
                    });
                }
            };
        }

        if (!this.emucharts || !this.emucharts.getInitialEdges()) { return; }
        // create svg element, if needed
        if (svg.empty()) { svg = this.newSVG(); }
        var edges = this.emucharts.getInitialEdges().values().filter(_this._edgeFilterFunction());
        if (edges) {
            // create a group of svg elements for transitions, and bind them to data
            var initial_transitions = svg.select("#InitialTransitions").selectAll(".itransition")
                                    .data(edges, function (edge) { return edge.id; });
            var enteredTransitions = drawInitialTransitions(initial_transitions.enter());
//            var exitedTransitions =
            removeTransitions(initial_transitions.exit());
            if (container === "#ContainerStateMachine") {
                enteredTransitions
                    .on("mouseover", mouseOver)
                    .on("mouseout", mouseOut)
                    .on("click", mouseClick)
                    .on("dblclick", initialTransitionOnMouseDoubleClick(_this));
            }
        }
    };

    function newBoxWidth(nodeID) {
        return 18 + d3.select("#label_" + nodeID).node().getBoundingClientRect().width;
    }

    /**
     * Utility function to refresh rendered states.
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function refreshStates(_this, states) {
        states = states || d3.select(_this.container + " svg").select("#States").selectAll(".state");
        // refresh state position
        states.attr("transform", function (node) {
            return "translate(" + node.x + ", " + node.y + ") scale(1.0)";
        });
        // refresh labels
        states.select(".state_label").text(function (node) {
            return node.name;
        });
        // refresh box size, position, and color
        states.select(".state_box").attr("width", function (node) {
            return newBoxWidth(node.id);
        }).attr("x", function (node) {
            return -(newBoxWidth(node.id) / 2);
        }).style("fill", function (node) {
            return node.color;
        }).style("stroke", function (node) { // draw a frame around the box
            return d3.rgb(node.color).darker().toString();
        });

        // refresh move tool, if needed
        states.select(".state_move").attr("x", function (node) {
            return (newBoxWidth(node.id) / 2) - 18;
        });
        return states;
    }

    /**
     * Utility function for drawing states
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function drawStates(enteredStates) {
        function nodeHeight(node) {
            return node.height || defaultHeight;
        }
        function nodeWidth(node) {
            return node.width || defaultWidth;
        }


        enteredStates = enteredStates.append("svg:g").classed("state", true)
            .attr("id", function (node) { return node.id; })
            .attr("transform", function (node) {
                return "translate(" + node.x + ", " + node.y + ") scale(1.0)";
            });
        // draw states (selectAll will automatically iterate for all states)
//        var state =
        enteredStates.append("svg:rect").classed("state_box", true)
            .attr("id", function (node) { return "box_" + node.id; })
            .attr("width", function (node) { return nodeWidth(node); })
            .attr("height", function (node) { return nodeHeight(node); })
            // translate x,y so that the box is centered there
            .attr("x", function (node) { return -(nodeWidth(node) / 2); })
            .attr("y", function (node) { return -(nodeHeight(node) / 2); })
            .attr("rx", 6).attr("ry", 6) // draw rounded corners
            .style("opacity", "0.9") // make the node slightly transparent
            .style("cursor", "pointer") // change cursor shape on mouse over
            .style("fill", function (node) {
                return node.color;
            })
            .style("stroke", function (node) { // draw a frame around the box
                return d3.rgb(node.color).darker().toString();
            });
        // draw move tool for boxes
//        var moveTool =
        enteredStates.append("svg:rect").classed("state_move", true)
            .attr("id", function (node) { return "resize_" + node.id; })
            .attr("width", 20).attr("height", 20)
            // place the resize tool at the lower right corner of the box
            .attr("x", function (node) { return nodeWidth(node) / 2 - 18; })
            .attr("y", function (node) { return nodeHeight(node) / 2 - 18; })
            .attr("rx", 2).attr("ry", 2) // draw rouded corners
            .style("stroke", "gray") // set border colour
            .style("stroke-width", "2") // set border size
            .style("fill", "white") // set fill colour
            .style("opacity", "0.4") // make the resize tool slightly transparent
            .style("cursor", "pointer"); // change cursor shape on mouse over
        // draw state names
//        var label =
        enteredStates.append("svg:text").classed("state_label", true)
            .attr("id", function (node) { return "label_" + node.id; })
            .attr("text-anchor", "middle")
            .style("font", (fontSize + "px sans-serif"))
            .style("text-rendering", "optimizeLegibility")
            .text(function (node) { return node.name; });

        return enteredStates;
    }

    /**
     * Utility function for removing states from the SVG
     * @returns reference to the updated svg elements
     * @memberof EmuchartsEditor
     */
    function removeStates(exitedStates) {
        return exitedStates
                    .transition().duration(220)
                    .style("opacity", 0).remove();
    }

    /**
     * Utility function used to match node names with a filter regex
     * @returns {Boolean} true if the node matches the regular expression or false otherwise
     */
    EmuchartsEditor.prototype._nodeFilterFunction = function () {
        var filter = this._nodeFilter;
        return function (n) {
            try {
                var regex = new RegExp("^" + filter, "gi");
                return n.name.search(regex) >= 0;
            } catch (syntaxError) {
                return false; //syntax error
            }
        };
    };

    /**
     * Utility function used to match edges whose source and/or target names matches a filter regex
     * @returns {Boolean} true if the edge source/target node matches the regular expression or false otherwise
     */
    EmuchartsEditor.prototype._edgeFilterFunction = function () {
        var nodeFilterFunction = this._nodeFilterFunction();
        return function (e) {
            if (e.source && e.target) {
                return nodeFilterFunction(e.source) && nodeFilterFunction(e.target);
            } else if (e.target) {
                return nodeFilterFunction(e.target);
            }
        };
    };


    /**
     * Utility function for drawing states
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.renderStates = function (opt) {
        var _this = this;
        var container = _this.container || "#ContainerStateMachine";
        var svg = d3.select(container).select("svg");

        // mouse event handlers
        var dragStart = function (node) {
            if (dbg) { console.log("State.dragStart"); }
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.sourceEvent.stopPropagation();
            // update mouse variables
            _this.mousedrag.node = node;
            if (editor_mode === MODE.ADD_TRANSITION()) {
                // create an arrow from the selected node to the cursor position
                drag_line.classed("hidden", false)
                    .style("marker-end", "url(#drag-arrow)")
                    .attr("d", function (node) {
                        var edge = { source: _this.mousedrag.node, target: _this.mousedrag.node };
                        // refresh control points
                        var cp = getControlPoints_selfEdge(edge);
                        // refresh path
                        return lineFunction(cp);
                    });
//                    .attr("d", "M" + mousedrag.node.x + "," + mousedrag.node.y +
//                                "L" + (mousedrag.node.x + d3.mouse(this)[0]) +
//                                "," + (mousedrag.node.y + d3.mouse(this)[1]));
            }
        };
        var dragNode = function (node) {
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.sourceEvent.stopPropagation();
            if (dbg) { console.log("State.dragNode"); }
            function computeControlPoint(edge) {
                var dx1 = edge.target.x - edge.source.x;
                var dy1 = edge.target.y - edge.source.y;
                var dist1 = Math.sqrt(dx1 * dx1 + dy1 * dy1);
                var controlPoint = getControlPoints(edge)[1];
                var dx2 = controlPoint.x - edge.source.x;
                var dy2 = controlPoint.y - edge.source.y;
                var dist2 = Math.sqrt(dx2 * dx2 + dy2 * dy2);
                var viscosity = (edge.target.id === edge.source.id) ? 1 : 0.8;
                return (dist1 > dist2) ?
                        { x: controlPoint.x + d3.event.dx * dist2 / dist1 * viscosity,
                            y: controlPoint.y + d3.event.dy * dist2 / dist1 * viscosity}
                        : { x: controlPoint.x + d3.event.dx * viscosity,
                              y: controlPoint.y + d3.event.dy * viscosity };
            }

            if (editor_mode !== MODE.ADD_TRANSITION() && editor_mode !== MODE.DELETE() &&
                    editor_mode !== MODE.RENAME()) {
                // correct handling of mouse events requires moving the selected transition on top of the others
                if (svg.node().children.length > 0) {
                    _this.select_state(node.id);
                }
                var draggedNode = _this.emucharts.nodes.get(node.id);
                // update node position
                draggedNode.x = node.x + d3.event.dx;
                draggedNode.y = node.y + d3.event.dy;
                _this.emucharts.nodes.set(node.id, draggedNode);
                refreshStates(_this,
                    d3.select(container).select("#States").selectAll(".state").filter(function (n) {
                        return n.id === node.id;
                    })
                );
                // update all edges connected to this node
                var updatedTransitions = d3.select(container)
                        .select("#Transitions").selectAll(".transition")
                        .filter(function (edge) {
                            var draggedEdge = _this.emucharts.edges.get(edge.id);
                            if (edge.source.id === node.id) {
                                draggedEdge.source.x = draggedNode.x;
                                draggedEdge.source.y = draggedNode.y;
                                draggedEdge.controlPoint = computeControlPoint(draggedEdge);
                                _this.emucharts.edges.set(edge.id, draggedEdge);
                                //console.log("updated x,y coords of edge " + edge.id);
                                return true;
                            }
                            if (edge.target.id === node.id) {
                                draggedEdge.target.x = draggedNode.x;
                                draggedEdge.target.y = draggedNode.y;
                                draggedEdge.controlPoint = computeControlPoint(draggedEdge);
                                _this.emucharts.edges.set(edge.id, draggedEdge);
                                //console.log("updated x,y coords of edge " + edge.id);
                                return true;
                            }
                            return false;
                        });
                refreshTransitions(_this, updatedTransitions);
                var updatedInitialTransitions = d3.select(container)
                        .select("#InitialTransitions").selectAll(".itransition")
                        .filter(function (edge) {
                            var draggedEdge = _this.emucharts.initial_edges.get(edge.id);
                            if (edge.target.id === node.id) {
                                edge.target.x = draggedNode.x;
                                edge.target.y = draggedNode.y;
                                _this.emucharts.initial_edges.set(edge.id, draggedEdge);
                                return true;
                            }
                            return false;
                        });
                refreshInitialTransitions(_this, updatedInitialTransitions);
            } else if (editor_mode === MODE.ADD_TRANSITION() && _this.mousedrag.node) {
                if (_this.mousedrag.node && _this.mouseover.node && _this.mousedrag.node.id === _this.mouseover.node.id) {
                    drag_line.attr("d", function (node) {
                        var edge = { source: _this.mousedrag.node, target: _this.mousedrag.node };
                        // refresh control points
                        var cp = getControlPoints_selfEdge(edge);
                        // refresh path
                        return lineFunction(cp);
                    });
                } else {
                    drag_line.attr("d", "M" + _this.mousedrag.node.x + "," + _this.mousedrag.node.y +
                                    "L" + (_this.mousedrag.node.x + d3.mouse(this)[0]) +
                                    "," + (_this.mousedrag.node.y + d3.mouse(this)[1]));
                }
            }
        };
        var dragEnd = function (node) {
            if (dbg) { console.log("State.dragEnd"); }
            // stopPropagation is essential here to avoid messing up with state variables of the SVG drag/zoom events
            d3.event.sourceEvent.stopPropagation();
            if (editor_mode === MODE.ADD_TRANSITION()) {
                if (_this.mousedrag.node && _this.mouseover.node) {
                    _this.fire({
                        type: "emuCharts_addTransition",
                        source: _this.mousedrag.node,
                        target: _this.mouseover.node
                    });
                }
                // hide drag arrow & reset mouse vars
                drag_line.classed("hidden", true)
                         .style("marker-end", "")
                         .style("marker-start", "")
                         .attr("d", "M0,0L0,0");
            } else {
                if (_this.mousedrag.node.x === node.x && _this.mousedrag.node.y === node.y) {
                    // click event
                    //console.log("click");
                    if (!_this.dragged) {
                        if (editor_mode === MODE.DELETE() && _this.mouseover.node) {
                            _this.mouseover.node = null;
                            _this.fire({
                                type: "emuCharts_deleteState",
                                node: node
                            });
                        } else if (editor_mode === MODE.RENAME() && _this.mouseover.node) {
                            _this.fire({
                                type: "emuCharts_renameState",
                                node: node
                            });
                        }
                    } else { _this.dragged = false; }
                }
            }
            // update mouse variables
            _this.mousedrag.node = null;
        };
        var mouseOver = function (node) {
            if (mouseOverControlPoint === null) {
                d3.event.stopPropagation();
                // update mouse variables
                _this.mouseover.node = node;
                // highlight node
                d3.select(this).attr("stroke-width", 2);
//                this.setAttribute("transform", this.getAttribute("transform").replace("scale(1.0)", "scale(1.1)"));
                if (editor_mode === MODE.ADD_TRANSITION() && _this.mousedrag.node) {
                    if (_this.mousedrag.node.id !== node.id) {
                        // change colour of drag arrow to give a cue that a mouse release will trigger the creation of a new transition between nodes
                        drag_line.style("stroke", node.color);
                        d3.select("#drag-arrow path").style("fill", node.color);
                    } else {
                        drag_line.style("stroke", "black");
                        d3.select("#drag-arrow path").style("fill", "black");
                    }
                }
            }
        };
        var mouseOut = function (node) {
            if (mouseOverControlPoint === null) {
                d3.event.stopPropagation();
                // update mouse variables
                _this.mouseover.node = null;
                // restore node size
                d3.select(this).attr("stroke-width", 1);
//                this.setAttribute("transform", this.getAttribute("transform").replace("scale(1.1)", "scale(1.0)"));
                if (editor_mode === MODE.ADD_TRANSITION()) {
                    // change colour of drag arrow to the default (black)
                    drag_line.style("stroke", "black");
                    d3.select("#drag-arrow path").style("fill", "black");
                }
            }
        };
        var mouseMove = function (node) {
            if (mouseOverControlPoint) {
                d3.event.stopPropagation();
                //console.log("mouseover control point");
                var m = d3.mouse(d3.select(container + " svg").select("#States").node());
                // update selected control point
                var cp = { x: m[0], y: m[1] };
                //console.log("(" + m[0] + "," + m[1] + ")");
                _this.emucharts.set_controlPoint(mouseOverControlPoint, cp);
                var transitionID = mouseOverControlPoint.id;
                var transitions = d3.select(container)
                                    .select("#Transitions").selectAll(".transition")
                                    .filter(function (transition) { return transition.id === transitionID; });
                // refresh transitions
                return refreshTransitions(_this, transitions);
            }
        };
        function stateOnMouseDoubleClick(_this) {
            return function (node) {
                if (editor_mode !== MODE.DELETE()) {
                    d3.event.stopPropagation();
                    _this.fire({
                        type: "emuCharts_renameState",
                        node: node
                    });
                }
                if (dbg) { console.log("State.mouseDoubleClick"); }
            };
        }

        if (!this.emucharts || !this.emucharts.getNodes()) { return; }

        var nodes = this.emucharts.getNodes().values().filter(_this._nodeFilterFunction());
        if (nodes) {
            if (svg.empty()) { svg = this.newSVG(container); }
            // create a group of svg elements for states, and bind them to data
            var states = svg.select("#States").selectAll(".state")
                            .data(nodes, function (node) { return node.id; });
            var enteredStates = drawStates(states.enter());
//            var exitedStates  =
            removeStates(states.exit());
            if (container === "#ContainerStateMachine") {
                var drag = d3.behavior.drag().origin(function (node) {
                    return node;
                });
                drag.on("dragstart", dragStart)
                    .on("drag", dragNode)
                    .on("dragend", dragEnd);
                enteredStates.call(drag)
                    .on("mouseover", mouseOver)
                    .on("mouseout", mouseOut)
                    .on("mousemove", mouseMove)
                    .on("dblclick", stateOnMouseDoubleClick(_this));
            }
        }
    };

    /**
     * Interface function for rendering the emuchart
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.render = function (opt) {
        opt = opt || {};
        if (opt.keep_transformations) {
            opt.trans = "translate(" + this.d3EventTranslate + ") scale(" + this.d3EventScale + ")";
        }
        var container = opt.container || this.container || "#ContainerStateMachine";
        if (d3.select(container + " svg").node()) {
            // FIXME! investigate why this is needed! with storyboard editor, if the emuchart is generated twice, the second time the emuchart is not visible without the following instruction
            d3.select(container).node().removeChild(d3.select(container + " svg").node());
        }
        this.renderStates();
        this.renderTransitions(opt);
        this.renderInitialTransitions();
        if (opt.trans) {
            d3.select(container + " svg").select("#States").attr("transform", opt.trans);
            d3.select(container + " svg").select("#Transitions").attr("transform", opt.trans);
            d3.select(container + " svg").select("#InitialTransitions").attr("transform", opt.trans);
            d3.select(container + " svg").select("#dragline").attr("transform", opt.trans);
        }
        var _this = this;
        refreshStates(_this);
        refreshTransitions(_this, null, opt);
        refreshInitialTransitions(_this);
        return this;
    };


    EmuchartsEditor.prototype.getTransformation = function () {
        if (!this.emucharts.nodes.empty()) {
            return d3.select("#ContainerStateMachine").select("svg").select("#States").attr("transform");
        }
        return "";
    };

    EmuchartsEditor.prototype.layOutChart = function () {
        var nodes = this.emucharts.nodes.values();
        var links = this.emucharts.edges.values();
        var width = 640;
        var height = 640;

        var extraNodes = [];
        var extendedLinks = [];
        links.forEach(function (link) {
            if (link.controlPoint) {
                extendedLinks = extendedLinks.concat({ source: link.source, target: link.controlPoint });
                extendedLinks = extendedLinks.concat({ source: link.controlPoint, target: link.target });
                extraNodes = extraNodes.concat(link.controlPoint);
            }
        });

        var initial_edges = this.emucharts.getInitialEdges();

        var force = d3.layout.force()
            .nodes(nodes.concat(extraNodes))
            .links(extendedLinks)
            .size([width, height])
            .charge(-4096)
            .gravity(0.6)
            .linkDistance(120)
            .on("tick", function() {
                // pull initial state towards left-up
                if (initial_edges.size() > 0) {
                    initial_edges.values().forEach(function (initial_edge) {
                        initial_edge.target.y -= 1;
                        initial_edge.target.x -= 2;
                    });
                }
            });

        force.start();
        for (var i = 0; i < 2048; i++) {
            force.tick();
        }
        force.stop();

        // Set control points of self-loops on a same node so the emucharts is more visually appealing
        var offsetX = 48, offsetY = 200;
        var done = d3.map();
        links.forEach(function(link) {
            if (link.source === link.target && !done.get(link.target)) {
                // find all self-loops of the target node
                var worklist = links.filter(function (l) {
                    return l.target === link.target && l.source === l.target;
                });
                if (worklist.length === 1) {
                    worklist[0].controlPoint = worklist[0].controlPoint || {};
                    worklist[0].controlPoint.x = worklist[0].target.x + offsetX;
                    worklist[0].controlPoint.y = worklist[0].target.y - offsetX * 2;
                } else {
                    // put control points on a diagonal line, evenly spaced
                    var dx = 0, dy = 0, reversed = false;
                    worklist.forEach(function (l) {
                        l.controlPoint = l.controlPoint || {};
                        l.controlPoint.x = l.target.x + dx + offsetX;
                        l.controlPoint.y = l.target.y + dy - offsetY;

                        if (!reversed && Math.abs(dy - offsetY) < 48) {
                            dy += 64;
                            reversed = true;
                        }
                        dy += 16;
                        dx = (dy > offsetY) ? dx - 20 : dx + 20;
                        done.set(l.target);
                    });
                }
            }
        });
        return this;
    };
    /**
     * @function layOutChart
     * @description Lays out the chart using a force-directed layout
     * @memberof module:EmuchartsEditor
     * @instance
     */
    EmuchartsEditor.prototype.layOutChart_nath = function () {
        var nodes = this.emucharts.nodes.values();
        var links = this.emucharts.edges.values();
        var width = 500;
        var height = 500;

        var force = d3.layout.force()
            .nodes(nodes)
            .links(links)
            .size([width, height])
            .charge(-5000)
            .gravity(0.4)
            .linkDistance(function(d){
                // Nodes with lots of links are put close to their siblings. Helps reduce crossings (a little)
                return Math.max((1/d.target.weight)*600, 100);
            })
            .on("tick", function(e) {
                // Push each node up or down depending on its balance of incoming and outgoing edges.
                // This will push leaf nodes down and root nodes up, giving a slightly more logical layout
                var dist = 50 * e.alpha;

                links.forEach(function(link) {
                    link.source.y -= dist;
                    link.target.y += dist;
                });
            });

        force.start();
        for (var i = 0; i < 100; ++i) {
            force.tick();
        }
        force.stop();

        // Move the anchor point of each link to be more visually appealing
        links.forEach(function(link) {
            if (link.source !== link.target) {
                if (!link.controlPoint) {
                    link.controlPoint = {x: 0, y: 0};
                }

                var dx = link.target.x - link.source.x;
                var dy = link.target.y - link.source.y;

                link.controlPoint.x = link.source.x + (dx * 0.2);
                link.controlPoint.y = link.target.y - (dy * 0.2);
            } else {
                // let self-loops deal with layout themselves
                link.controlPoint = null;
            }
        });
    };




    EmuchartsEditor.prototype.preview = function (opt) {
        opt = opt || {};
        this.render({
            trans: "scale(" + opt.scale_zoom + ")",
            prefix: "preview-"
        });
        return this;
    };


    /**
     * Returns a fresh state name
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getFreshStateName = function () {
        return this.emucharts.getFreshStateName();
    };

    /**
     * Returns a fresh transition name
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getFreshTransitionName = function () {
        return this.emucharts.getFreshTransitionName();
    };

    /**
     * Returns a fresh name for initial transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getFreshInitialTransitionName = function () {
        return this.emucharts.getFreshInitialTransitionName();
    };

    /**
     * Returns an array containing the current set of states in the diagram
     * Each states is given as a pair { name, id }
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getStates = function () {
        return this.emucharts.getStates();
    };

    /**
     * @description Returns the descriptor of a state.
     * @param id {String} The identifier of the state.
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getState = function (id) {
        return this.emucharts.getState(id);
    };

    /**
     * Returns an array containing the current set of constants defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getConstants = function () {
        return this.emucharts.getConstants();
    };

    /**
     * Returns the descriptor of the variable whose ID is the function argument
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getConstant = function (constantID) {
        return this.emucharts.getConstant(constantID);
    };

    /**
     * Returns an array containing the current set of datatypes defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getDatatypes = function () {
        return this.emucharts.getDatatypes();
    };

    /**
     * Returns the descriptor of the datatype whose ID is the function argument
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getDatatype = function (datatypeID) {
        return this.emucharts.getDatatype(datatypeID);
    };
    /**
     * Returns an array containing the current set of variables defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getVariables = function () {
        return this.emucharts.getVariables();
    };

    /**
     * Returns the descriptor of the variable whose ID is the function argument
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getVariable = function (variableID) {
        return this.emucharts.getVariable(variableID);
    };

    /**
     * Returns an array containing the current set of input variables defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getInputVariables = function () {
        return this.emucharts.getInputVariables();
    };

    /**
     * Returns an array containing the current set of output variables defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getOutputVariables = function () {
        return this.emucharts.getOutputVariables();
    };

    /**
     * Returns an array containing the current set of local variables defined in the diagram
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getLocalVariables = function () {
        return this.emucharts.getLocalVariables();
    };

    /**
     * Returns an array specifying the supported variable scopes
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getVariableScopes = function () {
        return this.emucharts.getVariableScopes();
    };

    /**
     * Returns an array containing the current set of transitions in the diagram
     * Each transition is given as a 4-tuple { name, id, source, target }
     * where source and target are pairs { name, id }
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getTransitions = function () {
        return this.emucharts.getTransitions();
    };

    /**
     * @description Returns the descriptor of a transition.
     * @param id {String} The identifier of the transition.
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getTransition = function (id) {
        return this.emucharts.getTransition(id);
    };

    /**
     * Returns an array containing the current set of initial transitions in the diagram
     * Each transition is given as a 3-tuple { name, id, target }
     * where target is a pair { name, id }
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.getInitialTransitions = function () {
        return this.emucharts.getInitialTransitions();
    };

    /**
     * utility function to rename transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.rename_transition = function (transitionID, newLabel) {
        var _this = this;
        if (newLabel !== null && newLabel !== undefined) {
            this.emucharts.rename_edge(transitionID, newLabel);
        }
        var transitions = d3.select("#ContainerStateMachine")
                        .select("#Transitions").selectAll(".transition")
                        .filter(function (transition) { return transition.id === transitionID; });
        // refresh transitions
        refreshTransitions(_this, transitions, { color: "black"});
        return this;
    };

    /**
     * utility function to refresh transition style (e.g., color)
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.refresh_transition = function (transitionID, opt) {
        var _this = this;
        var transitions = d3.select("#ContainerStateMachine")
                        .select("#Transitions").selectAll(".transition")
                        .filter(function (transition) { return transition.id === transitionID; });
        // refresh transitions
        refreshTransitions(_this, transitions, opt);
        return this;
    };

    /**
     * utility function to rename initial transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.rename_initial_transition = function (transitionID, newLabel) {
        var _this = this;
        this.emucharts.rename_initial_edge(transitionID, newLabel);
        var itransitions = d3.select("#ContainerStateMachine")
                        .select("#InitialTransitions").selectAll(".itransition")
                        .filter(function (itransition) { return itransition.id === transitionID; });
        // refresh transitions
        refreshInitialTransitions(_this, itransitions);
    };

    /**
     * utility function to edit states
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.edit_state = function (stateID, data) {
        this.emucharts.edit_node(stateID, data);
        // refresh states
        var states = d3.select("#ContainerStateMachine")
            .select("#States").selectAll(".state")
            .filter(function (state) { return state.id === stateID; });
        refreshStates(this, states);

        // TODO: temporary fix for transitions not being redrawn after renaming a state.
        this.renderTransitions();
       /*
        // refresh all incoming and outgoing transitions of the renamed state
        var transitions = d3.select("#ContainerStateMachine")
            .select("#Transitions").selectAll(".transition")
            .filter(function (transition) {
                return (transition.target && transition.target.id === stateID) ||
                        (transition.source && transition.source.id === stateID);
            });

        transitions = transitions ||
            d3.select("#ContainerStateMachine svg").select("#Transitions").selectAll(".transition");
        // refresh labels
        transitions.selectAll(".path").attr("d", function (edge) {
            // refresh transition label
            var label = d3.select(this.parentNode).select(".tlabel");
            label.text(labelToString(edge.name));
        });
       */
    };

    /**
     * Interface function for adding states
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_state = function (stateName, position) {
        var newNode = { name: stateName };
        if (position) {
            newNode.x = position.x;
            newNode.y = position.y;
        }
        this.emucharts.add_node(newNode);
        return this.renderStates();
    };

    /**
     * Interface function for deleting states and all transitions incoming/outgoing to this state
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_state = function (stateID) {
        var _this = this;
        var edges = [];
        if (this.emucharts && this.emucharts.edges) {
            this.emucharts.edges.forEach(function (key) {
                var edge = _this.emucharts.edges.get(key);
                if ((edge.source && edge.source.id === stateID) ||
                        (edge.target && edge.target.id === stateID)) {
                    edges.push(edge.id);
                }
            });
            edges.forEach(function (edge) {
                _this.emucharts.remove_edge(edge);
            });
        }
        var initial_edges = [];
        if (this.emucharts && this.emucharts.initial_edges) {
            this.emucharts.initial_edges.forEach(function (key) {
                var initial_edge = _this.emucharts.initial_edges.get(key);
                if (initial_edge.target && initial_edge.target.id === stateID) {
                    initial_edges.push(initial_edge.id);
                }
            });
            initial_edges.forEach(function (initial_edge) {
                _this.emucharts.remove_initial_edge(initial_edge);
            });
        }

        if (this.emucharts && this.emucharts.nodes) {
            this.emucharts.remove_node(stateID);
        }

        // refresh editor
        this.renderTransitions();
        this.renderInitialTransitions();
        return this.renderStates();
    };

    /**
     * Interface function for deleting transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_transition = function (transitionID) {
        this.emucharts.remove_edge(transitionID);
        return this.renderTransitions();
    };

    /**
     * Interface function for deleting initial transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_initial_transition = function (transitionID) {
        this.emucharts.remove_initial_edge(transitionID);
        return this.renderInitialTransitions();
    };

    /**
     * Interface function for deleting a constant
     * @param constantID is the unique constant identifier
     * @returns true if constant removed successfully; otherwise returns false
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_constant = function (constantID) {
        return this.emucharts.remove_constant(constantID);
    };

    /**
     * Interface function for deleting a datatype
     * @param constantID is the unique datatype identifier
     * @returns true if datatype removed successfully; otherwise returns false
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_datatype = function (datatypeID) {
        return this.emucharts.remove_datatype(datatypeID);
    };

    /**
     * Interface function for deleting a variable
     * @param variableID is the unique variable identifier
     * @returns true if variable removed successfully; otherwise returns false
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_variable = function (variableID) {
        return this.emucharts.remove_variable(variableID);
    };

    /**
     * Interface function for adding transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_transition = function (transitionName, from, to) {
        var source = this.emucharts.getState(from);
        var target = this.emucharts.getState(to);
        if (source && target) {
            var edge = {
                    name: transitionName,
                    source: source,
                    target: target,
                    controlPoint: null
                };
            var controlPoints = (source.id === target.id) ? getControlPoints_selfEdge(edge) : getControlPoints(edge);
            edge.controlPont = (source.id === target.id) ? controlPoints[2] : controlPoints[1];
            // FIXME: need to adjust the position in the case svg is translated
            this.emucharts.add_edge(edge);
            this.renderTransitions();
            return edge;
        } else {
            // FIXME: improve interaction & feedback
            alert("invalid nodes");
        }
    };

    /**
     * Interface function for adding initial transitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_initial_transition = function (transitionName, to) {
        var target = this.emucharts.getState(to);
        if (target) {
            // FIXME: need to force one initial transition per state
            var edge = {
                name: transitionName,
                target: target
            };
            this.emucharts.add_initial_edge(edge);
            this.renderInitialTransitions();
            return edge;
        } else {
            // FIXME: improve interaction & feedback
            alert("invalid nodes");
        }
    };

    /**
     * Interface function for adding new constant definitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_constant = function (newConstant) {
        return this.emucharts.add_constant(newConstant);
    };

    /**
     * Interface function for adding new datatype definitions
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_datatype = function (newDatatype) {
        return this.emucharts.add_datatype(newDatatype);
    };

    /**
     * Interface function for adding new state variables
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.add_variable = function (newVariable) {
        return this.emucharts.add_variable(newVariable);
    };

    /**
     * Interface function for editing constants
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.rename_constant = function (constantID, newData) {
        return this.emucharts.rename_constant(constantID, newData);
    };

    /**
     * Interface function for editing datatypes
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.rename_datatype = function (datatypeID, newData) {
        return this.emucharts.rename_datatype(datatypeID, newData);
    };

    /**
     * Interface function for editing state variables
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.rename_variable = function (variableID, newData) {
        return this.emucharts.rename_variable(variableID, newData);
    };

    /**
     * Interface function for deleting charts
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.delete_chart = function () {
        var _this = this;
        if (this.emucharts.constants) {
            this.emucharts.constants.forEach(function (key) {
                _this.delete_constant(key);
            });
        }
        if (this.emucharts.variables) {
            this.emucharts.variables.forEach(function (key) {
                _this.delete_variable(key);
            });
        }
        var transitions = this.emucharts.getTransitions();
        if (transitions) {
            transitions.forEach(function (transition) {
                _this.delete_transition(transition.id);
            });
        }
        var states = this.emucharts.getStates();
        if (states) {
            states.forEach(function (state) {
                _this.delete_state(state.id);
            });
        }
        return this;
    };

    /**
     * Interface function for checking whether the current chart is empty
     * @memberof EmuchartsEditor
     */
    EmuchartsEditor.prototype.empty_chart = function () {
        return this.emucharts && this.emucharts.nodes && this.emucharts.nodes.empty() &&
                this.emucharts.edges && this.emucharts.edges.empty() &&
                this.emucharts.constants && this.emucharts.constants.empty() &&
                this.emucharts.variables && this.emucharts.variables.empty();
    };

    /** PIM **/

    /**
     * Convert the current Emuchart to a PIM (or if from a PIM).
     * @returns {boolean} True Emuchart became a PIM or a PIM became an Emuchart.
     */
    EmuchartsEditor.prototype.toPIM = function (toPIM) {
        return this.emucharts.toPIM ? this.emucharts.toPIM(toPIM) : false;
    };

    /**
     * Returns if this emuchart is a PIM.
     * @returns {boolean} If this emuchart is a PIM.
     */
    EmuchartsEditor.prototype.getIsPIM = function () {
        return this.emucharts.getIsPIM ? this.emucharts.getIsPIM() : false;
    };

    /**
     *
     * @param behaviour
     * @returns If no behaviour provided returns all PMR as a set,
     * If behaviour could be found then returns the relation (behaviour, operation),
     * else returns null.
     */
    EmuchartsEditor.prototype.getPMR = function (behaviour, isSave) {
        return this.emucharts.getPMR ? this.emucharts.getPMR(behaviour, isSave) : d3.map();
    };

    /**
     * Add a PMR (overrites any existing PMR for the given behaviour).
     * ({behaviour (string), operation (string)}).
     * @param pmr
     * @returns boolean true if successfully added.
     */
    EmuchartsEditor.prototype.addPMR = function (pmr) {
        return this.emucharts.addPMR ? this.emucharts.addPMR(pmr) : false;
    };

    /**
     * Saves the new PMRs into the pool of all PMRs
     * @param newPMRs
     * @returns {boolean}
     */
    EmuchartsEditor.prototype.mergePMR = function (newPMRs) {
        return this.emucharts.mergePMR ? this.emucharts.mergePMR(newPMRs) : false;
    };

    EmuchartsEditor.prototype.getEmuchartsVersion = function (newPMRs) {
        return this.emucharts.getEmuchartsVersion();
    };


    module.exports = EmuchartsEditor;
});
