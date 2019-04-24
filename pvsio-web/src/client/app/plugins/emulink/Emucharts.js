/**
 * @module Emucharts
 * @version 2.0
 * @description
 * Emucharts encodes an emuchart diagram into a graphs. This is code is a re-engineered
 * version of stateMachine.js implemented in branch emulink-commented
 * @author Paolo Masci
 * @date 14/05/14 2:53:03 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher"),
        PIMs = require("plugins/emulink/models/pim/PIMs"),
        d3 = require("d3/d3"),
        Colors = require("plugins/emulink/tools/Colors");

    var emuchartsVersion = "2.0";

    var defaultValues = {
            x: 100,
            y: 100,
            width: 36,
            height: 36,
            fontSize: 10,
            color: Colors.getColor
    };

    var getFreshNodeID = function (_this) {
        function newNodeID () {
            var i = 1;
            while(_this.nodes && _this.nodes.get("X" + i)) { i++; }
            return i;
        }
        return "X" + newNodeID();
    };
    var getFreshEdgeID = function (_this) {
        function newEdgeID () {
            var i = 1;
            while(_this.edges && _this.edges.get("T" + i)) { i++; }
            return i;
        }
        return "T" + newEdgeID();
    };
    var getFreshInitialEdgeID = function (_this) {
        function newInitialEdgeID () {
            var i = 1;
            while(_this.initial_edges && _this.initial_edges.get("IT" + i)) { i++; }
            return i;
        }
        return "IT" + newInitialEdgeID();
    };

    var createVariableID = function (variable) {
        var id = "VAR_" + variable.name + ":" + variable.type + "(" + variable.scope + ")";
        return id;
    };
    var createConstantID = function (constant) {
        var id = "CONST_" + constant.name + ":" + constant.type;
        return id;
    };
    var createDatatypeID = function (datatype) {
        var id = "DATATYPE_" + datatype.name;
        return id;
    };


    /**
     * @function Emucharts
     * @description Constructor.
     * @memberof module:Emucharts
     * @instance
     */
    function Emucharts(emuchart) {
        function cloneWidgets(widgets) {
            var widgetsClone = [];
            if (widgets) {
                widgets.forEach(function (widget) {
                    var w = {
                        category: widget.category,
                        name: widget.name,
                        behaviours: []
                    };
                    widget.behaviours.forEach(function (b) {
                        w.behaviours.push(b);
                    });
                    widgetsClone.push(w);
                });
            }
            return widgetsClone;
        }
        function cloneChart(c) {
            var chart = { nodes: d3.map(), edges: d3.map(), initial_edges: d3.map(),
                          variables: d3.map(), constants: d3.map(), datatypes: d3.map() };
            if (c.states) {
                c.states.forEach(function (state) {
                    chart.nodes.set(state.id, {
                        id:     state.id,
                        name:   state.name || state.id,
                        color:  state.color || Colors.getColor(state.id),
                        width:  state.width || 36,
                        height: state.height || 36,
                        x:      state.x || 100,
                        y:      state.y || 100,
                        enter:  state.enter || "",
                        exit:   state.exit || "",
                        // the following elements are for PIMs
                        pmr:    state.pmr || [],
                        widgets: cloneWidgets(state.widgets),
                        components: state.components || []
                    });
                });
            }
            if (c.transitions) {
                c.transitions.forEach(function (trans) {
                    var source = null, target = null;
                    if (trans.source) {
                        source = chart.nodes.get(trans.source.id);
                        if (!source) {
                            console.error("WARNING: corrupted emucharts file, edge " + trans.name +
                                            " points to a node" + trans.source.id +
                                            " that is not part of the set of nodes listed in the emucharts.");
                        }
                    }
                    if (trans.target) {
                        target = chart.nodes.get(trans.target.id);
                        if (!target) {
                            console.error("WARNING: corrupted emucharts file, edge " + trans.name +
                                            " points to a node" + trans.target.id +
                                            " that is not part of the set of nodes listed in the emucharts.");
                        }
                    }
                    chart.edges.set(trans.id, {
                        id:     trans.id,
                        name:   trans.name || "",
                        source: source,
                        target: target,
                        controlPoint: (trans.controlPoint) ? { x: trans.controlPoint.x, y: trans.controlPoint.y } : null
                    });
                });
            }
            if (c.initial_transitions) {
                c.initial_transitions.forEach(function (trans) {
                    if (trans.target) {
                        var initialNode = chart.nodes.get(trans.target.id);
                        if (!initialNode) {
                            console.error("WARNING: corrupted emucharts file, edge " + trans.name +
                                            " points to a node" + trans.target.id +
                                            " that is not part of the set of nodes listed in the emucharts.");
                        }
                        chart.initial_edges.set(trans.id,{
                            id:     trans.id,
                            name:   trans.name,
                            target: initialNode
                        });
                    }
                });
            }
            if (c.variables) {
                c.variables.forEach(function (variable) {
                    chart.variables.set(variable.id, {
                        id: variable.id,
                        name: variable.name,
                        type: variable.type,
                        value: variable.value,
                        scope: variable.scope
                    });
                });
            }
            if (c.constants) {
                c.constants.forEach(function (constant) {
                    chart.constants.set(constant.id, {
                        id: constant.id,
                        name: constant.name,
                        type: constant.type,
                        value: constant.value
                    });
                });
            }
            if (c.datatypes) {
                c.datatypes.forEach(function (datatype) {
                    var cons = [];
                    datatype.constructors.forEach(function (c) {
                        cons.push(c);
                    });
                    chart.datatypes.set(datatype.id, {
                        id: datatype.id,
                        name: datatype.name,
                        constructors: cons
                    });
                });
            }
            //TODO: check if the following copy of pmr is a deep copy
            if (c.pmr) {
                chart.pmr = d3.map();
                for (var behaviour in c.pmr) {
                    if (c.pmr.hasOwnProperty(behaviour)) {
                        chart.pmr.set(behaviour, c.pmr[behaviour]);
                    }
                }
            }
            if (c.isPIM) {
                chart.isPIM = true;
            }
            return chart;
        }
        if (emuchart) {
            var clone = cloneChart(emuchart);
            this.nodes = clone.nodes || d3.map();
            this.edges = clone.edges || d3.map();
            this.initial_edges = clone.initial_edges || d3.map();
            this.constants = clone.constants || d3.map();
            this.variables = clone.variables || d3.map();
            this.datatypes = clone.datatypes || d3.map();
            this.pmr = clone.pmr || d3.map();
            this.isPIM = clone.isPIM && clone.isPIM === true;
            this.pim = new PIMs(this.isPIM);
        } else {
            this.nodes = d3.map();
            this.edges = d3.map();
            this.initial_edges = d3.map();
            this.variables = d3.map();
            this.constants = d3.map();
            this.datatypes = d3.map();
            this.pmr = d3.map();
            this.isPIM = false;
            this.pim = new PIMs();
        }
        eventDispatcher(this);
        return this;
    }

    Emucharts.prototype.getEdges = function () {
        return this.edges;
    };
    Emucharts.prototype.getInitialEdges = function () {
        return this.initial_edges;
    };
    Emucharts.prototype.getNodes = function () {
        return this.nodes;
    };

    Emucharts.prototype.getDefaultValues = function () {
        return defaultValues;
    };

    /**
     * @function edit_node
     * @description Edits a node (i.e., a state) in the emuchart diagram.
     * @param id {String} Identifier of the node that shall be renamed.
     * @param data {Object} Structured type containing one field
     *      - name (String), specifying the new name of the node
     *      - color (String)
     * @returns {Boolean} true if node renamed successfully; otherwise returns false
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.edit_node = function (id, data) {
        var _this = this;
        if (!id || !_this.nodes || !_this.nodes.get(id)) { return false; }
        // get node and rename it
        var node = _this.nodes.get(id);
        node.name = data.name || node.name || "X" + node.id;
        node.color = data.color || node.color || defaultValues.color(id);
        node.enter = data.enter || "";
        node.exit = data.exit || "";
        _this.nodes.set(node.id, node);
        // we need to rename also nodes cached in the edge structure
        // this can be quite expensive in term of time, but renaming is unlikely to be a frequent operation
        // so the time cost is acceptable (given that caching is quite useful to speed up rendering)
        if (_this.edges) {
            _this.edges.forEach(function (key) {
                var edge = _this.edges.get(key);
                var dirty = false;
                if (edge.source.id === id) {
                    edge.source.name = node.name;
                    dirty = true;
                }
                if (edge.target.id === id) {
                    edge.target.name = node.name;
                    dirty = true;
                }
                if (dirty) { _this.edges.set(key, edge); }
            });
        }
        _this.fire({
            type: "emuCharts_stateRenamed",
            state: {
                id: node.id,
                name: node.name,
                color: node.color,
                enter: node.enter,
                exit: node.exit
            }
        });
        return true;
    };

    /**
     * @function add_node
     * @description Adds a new node (i.e., a new state) to the emucharts diagram.
     * @param node {Object} The characteristics of the node:
     *           <li> name (String): the name of the node. This property is optional. If omitted, the node identifier automatically assigned by this function will be used as node name.</li>
     *           <li> x (real): node position (x coordinate). This property is optional. If omitted, the node will be placed in a default position.</li>
     *           <li> y (real): node position (y coordinate). This property is optional. If omitted, the node will be placed in a default position.</li>
     * @returns {Object} Descriptor fo the created node. Nodes descriptors have the following properties:
     *           <li> id (String): the unique identifier of the node.</li>
     *           <li> name (String): the name of the node.</li>
     *           <li> x (real): node position (x coordinate).</li>
     *           <li> y (real): node position (y coordinate).</li>
     *           <li> width (real): node width.</li>
     *           <li> height (real): node height.</li>
     *           <li> color (String): node color.</li>
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_node = function (node) {
        if (!node) { return null; }
        // create a new node with a unique ID
        var id = node.id || getFreshNodeID(this);
        var name = node.name || id;
        var enter = node.enter || "";
        var exit = node.exit || "";
        var estimatedTextWidth = name.length * defaultValues.fontSize / 4;
        var width = (estimatedTextWidth < defaultValues.width) ? defaultValues.width : estimatedTextWidth;
        var newNode = {
                id  : id, // nodes have unique IDs
                name: name,
                enter: enter,
                exit: exit,
                x: node.x || defaultValues.x,
                y: node.y || defaultValues.y,
                width : width,
                height: defaultValues.height,
                color: node.color || defaultValues.color(id)
            };

        if (this.getIsPIM()) {
            newNode = this.pim.getState(newNode);
            newNode.color = newNode.color || defaultValues.color(newNode.id);
        }

        // add the new node to the diagram
        this.nodes.set(newNode.id, newNode);
        // fire event
        this.fire({
            type: "emuCharts_stateAdded",
            state: {
                id: newNode.id,
                name: newNode.name,
                color: newNode.color,
                enter: newNode.enter,
                exit: newNode.exit
            }
        });
        return newNode;
    };

    /**
     * @function remove_node
     * @description Removes a node from the diagram
     * @param node {Object} The descriptor of the node that shall be removed.
     * @returns {Boolean} true if the node has been removed successfully, false otherwhise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_node = function (node) {
        var rem = this.nodes.get(node);
        if (rem && this.nodes.remove(node)) {
            // fire event
            this.fire({
                type: "emuCharts_stateRemoved",
                state: {
                    id: rem.id,
                    name: rem.name
                }
            });
            return true;
        }
        return false;
    };

    /**
     * @function move_node
     * @description Changes the position of a node in the diagram
     * @param node {Object} The descriptor of the node that shall be moved.
     * @param new_pos {Object} The new position of the node.
     * @returns {Boolean} true if the node has been moved successfully, false otherwhise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.move_node = function (node, new_pos) {
        var move = this.nodes.get(node);
        if (move) {
            // fire event
            this.fire({
                type: "emuCharts_stateMoved",
                state: {
                    id: move.id,
                    name: move.name,
                    position: {
                        old: { x: move.x, y: move.y },
                        new: { x: new_pos.x, y: new_pos.y }
                    }
                }
            });
            move.x = new_pos.x;
            move.y = new_pos.y;
            this.nodes.set(move.id, move);
            return true;
        }
        return false;
    };

    /**
     * @function rename_edge
     * @description Renames a edge in the diagram.
     * @param id {String} The identifier of the edge that shall be renamed.
     * @param newName {String} The new name that shall be assigned to the edge.
     * @returns {Boolean} true if edge renamed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_edge = function (id, newName) {
        if (!id || !this.edges || !this.edges.get(id)) { return false; }
        // get edge and rename it
        var edge = this.edges.get(id);
        edge.name = newName;
        this.edges.set(edge.id, edge);
        // fire event
        this.fire({
            type: "emuCharts_transitionRenamed",
            transition: {
                id: edge.id,
                name: edge.name,
                source: {
                    id: edge.source.id,
                    name: edge.source.name
                },
                target: {
                    id: edge.target.id,
                    name: edge.target.name
                }
            }
        });
        return true;
    };

    /**
     * @function rename_initial_edge
     * @description Renames an initial edge in the diagram.
     * @param id {String} The identifier of the initial edge that shall be renamed.
     * @param newName {String} The new name that shall be assigned to the initial edge.
     * @returns {Boolean} true if initial edge renamed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_initial_edge = function (id, newName) {
        if (!id || !this.initial_edges || !this.initial_edges.get(id)) { return false; }
        // get edge and rename it
        var initial_edge = this.initial_edges.get(id);
        initial_edge.name = newName;
        this.initial_edges.set(initial_edge.id, initial_edge);
        // fire event
        this.fire({
            type: "emuCharts_initialTransitionRenamed",
            transition: {
                id: initial_edge.id,
                name: initial_edge.name,
                target: {
                    id: initial_edge.target.id,
                    name: initial_edge.target.name
                }
            }
        });
        return true;
    };

    /**
     * @function set_controlPoint
     * @description Adds a new control point to an edge of the diagram.
     * @param edge {Object} The edge descriptor that whose control point shall be set.
     * @param cp {Object} A control point descriptor.
     * @returns {Object} The descriptor of the edge.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.set_controlPoint = function (edge, cp) {
        if (!edge || !cp) { return false; }
        var ed = this.edges.get(edge.id);
        if (ed) {
            ed.controlPoint = cp;
            this.edges.set(edge.id, ed);
            return true;
        } else {
            console.log("dbg: warning - control point associated to unknown edge");
        }
        return false;
    };

    /**
     * @function add_edge
     * @description Adds a new edge to the diagram
     * @param edge {Object} The characteristics of the edge:
     *           <li> target (String): the identifier of the target node where the edge enters into. This property is mandatory.
     *           <li> source (String): the identifier of the source node where the edge originates from. This property is mandatory. FIXME: check why we are accepting edges with undefined source.
     *           <li> name (String): the name of the edge. This property is optional. If omitted, the edge identifier automatically assigned by this function will be used as edge name.</li>
     *           <li> controlPoint (Object): the control point of the edge.</li>
     * @returns {Object} Descriptor fo the created edge. Edge descriptors have the following properties:
     *           <li> id (String): the unique identifier of the node.</li>
     *           <li> name (String): the name of the node.</li>
     *           <li> source (Object): the source node.</li>
     *           <li> target (Object): the target node.</li>
     *           <li> controlPoint (Object): the control point.</li>
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_edge = function (edge) {
        if (!edge || !edge.target) {
            return null;
        }
        if (!this.nodes.has(edge.target.id)) {
            console.log("dbg: warning, target ID not found in emuchart data. New transitions not added.");
            return null;
        }

        var target = this.nodes.get(edge.target.id);
        var source = (edge.source) ? this.nodes.get(edge.source.id) : null;
        // create a new node with a unique ID
        var id = getFreshEdgeID(this);
        var newEdge = {
                id  : id, // nodes have unique IDs
                name: edge.name || id,
                source: source,
                target: target,
                controlPoint: edge.controlPoint
            };
        // add the new edge to the diagram
        this.edges.set(newEdge.id, newEdge);
        // fire event
        this.fire({
            type: "emuCharts_transitionAdded",
            transition: {
                id: newEdge.id,
                name: newEdge.name,
                source: {
                    id: newEdge.source.id,
                    name: newEdge.source.name
                },
                target: {
                    id: newEdge.target.id,
                    name: newEdge.target.name
                },
                controlPoint: edge.controlPoint
            }
        });
        return newEdge;
    };

    /**
     * @function add_initial_edge
     * @description Adds a new initial edge to the diagram. Initial edges originate from an implicit source node (which need not to be specified).
     * @param edge {Object} The characteristics of the edge:
     *           <li> target (String): the identifier of the target node where the edge enters into. This property is mandatory.
     *           <li> name (String): the name of the edge. This property is optional. If omitted, the edge identifier automatically assigned by this function will be used as edge name.</li>
     * @returns {Object} Descriptor fo the created edge. Edge descriptors have the following properties:
     *           <li> id (String): the unique identifier of the node.</li>
     *           <li> name (String): the name of the node.</li>
     *           <li> source (Object): the source node.</li>
     *           <li> target (Object): the target node.</li>
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_initial_edge = function (edge) {
        if (!edge || !edge.target) {
            return null;
        }
        if (!this.nodes.has(edge.target.id)) {
            console.log("dbg: warning, target ID not found in emuchart data. New transitions not added.");
            return null;
        }

        var target = this.nodes.get(edge.target.id);
        // create a new node with a unique ID
        var id = getFreshInitialEdgeID(this);
        var newEdge = {
                id  : id, // nodes have unique IDs
                name: edge.name || id,
                target: target
            };
        // add the new edge to the diagram
        this.initial_edges.set(newEdge.id, newEdge);
        // fire event
        this.fire({
            type: "emuCharts_initialTransitionAdded",
            transition: {
                id: newEdge.id,
                name: newEdge.name,
                target: {
                    id: newEdge.target.id,
                    name: newEdge.target.name
                }
            }
        });
        return newEdge;
    };

    /**
     * @function remove_edge
     * @description Removes an edge from the diagram.
     * @param edge {Object} The descriptor of the edge that shall be removed.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_edge = function (edge) {
        var rem = this.edges.get(edge);
        if (rem && this.edges.remove(edge)) {
            // fire event
            this.fire({
                type: "emuCharts_transitionRemoved",
                transition: {
                    id: rem.id,
                    name: rem.name,
                    source: {
                        id: rem.source.id,
                        name: rem.source.name
                    },
                    target: {
                        id: rem.target.id,
                        name: rem.target.name
                    }
                }
            });
            return true;
        }
        return false;
    };

    /**
     * @function remove_initial_edge
     * @description Removes an initial edge from the diagram.
     * @param initial_edge {Object} The descriptor of the initial edge that shall be removed.
     * @returns {Boolean} true if the descriptor has been removed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_initial_edge = function (initial_edge) {
        var rem = this.initial_edges.get(initial_edge);
        if (rem && this.initial_edges.remove(initial_edge)) {
            // fire event
            this.fire({
                type: "emuCharts_initialTransitionRemoved",
                transition: {
                    id: rem.id,
                    name: rem.name,
                    target: {
                        id: rem.target.id,
                        name: rem.target.name
                    }
                }
            });
            return true;
        }
        return false;
    };

    /**
     * @function getFreshStateName
     * @description Returns a fresh (i.e., not used by other nodes in the diagram) node name.
     * @returns {String} A fresh node name.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getFreshStateName = function () {
        return getFreshNodeID(this);
    };

    /**
     * @function getFreshTransitionName
     * @description Returns a fresh (i.e., not used by other transitions in the diagram) transitions name.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getFreshTransitionName = function () {
        return getFreshEdgeID(this);
    };

    /**
     * @function getFreshInitialTransitionName
     * @description Returns a fresh (i.e., not used by other initial transitions in the diagram) initial transitions name
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getFreshInitialTransitionName = function () {
        return getFreshInitialEdgeID(this);
    };

    /**
     * @function getStates
     * @description Returns the states in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the states in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getStates = function () {
        // If this emuchart is a pim return the pim combatable transition.
        var _this = this;
        if (this.getIsPIM()) {
            var ans = this.pim.getStates(_this.nodes) || [];
            ans.forEach(function (state) {
                state.color = state.color || defaultValues.color(state.id);
            });
            return ans;
        }

        var states = [];
        _this.nodes.forEach(function (key) {
            var node = _this.nodes.get(key);
            states.push({
                name: node.name,
                id: node.id,
                x: node.x,
                y: node.y,
                width : node.width,
                height: node.height,
                color: node.color,
                enter: node.enter,
                exit: node.exit
            });
        });
        return states.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function getState
     * @description Returns the descriptor of a state.
     * @param id {String} The identifier of the state.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getState = function (id) {
        if (this.getIsPIM()) {
            var state = this.pim.getState(this.nodes.get(id));
            if (state) {
                state.color = state.color || defaultValues.color(state.id);
            }
            return state;
        }
        // The state should already be a PIM state if required.
        return this.nodes.get(id);
    };

    /**
     * @function getTransitions
     * @description Returns the descriptor of a transition.
     * @param id {String} The identifier of the transition.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getTransition = function (id) {
        // If this emuchart is a pim return the pim combatable transition.
        if (this.getIsPIM()) {
            return this.pim.getTransition(this.edges.get(id));
        }
        return this.edges.get(id);
    };

    /**
     * @function getTransitions
     * @description Returns the transitions in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the transitions in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getTransitions = function () {
        // If this emuchart is a pim return the pim combatable transition.
        if (this.getIsPIM()) {
            return this.pim.getTransitions(this.edges);
        }

        var transitions = [], _this = this;
        _this.edges.forEach(function (key) {
            var trans = _this.edges.get(key);
            transitions.push({
                name: trans.name,
                id: key,
                source: {
                    name: trans.source.name,
                    id: trans.source.id
                },
                target: {
                    name: trans.target.name,
                    id: trans.target.id
                },
                controlPoint: (trans.controlPoint) ? {
                    x: trans.controlPoint.x,
                    y: trans.controlPoint.y
                } : null
            });
        });
        return transitions.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function getInitialTransition
     * @description Returns an initial transition.
     * @param id {String} The identifier of the initial transition.
     * @returns {Object} The descriptor of the initial transition.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getInitialTransition = function (id) {
        return this.initial_edges.get(id);
    };

    /**
     * @function getInitialTransitions
     * @description Returns the initial transitions in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the transitions in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getInitialTransitions = function () {
        var initial_transitions = [], _this = this;
        _this.initial_edges.forEach(function (key) {
            var trans = _this.initial_edges.get(key);
            initial_transitions.push({
                name: trans.name,
                id: key,
                target: {
                    name: trans.target.name,
                    id: trans.target.id
                }
            });
        });
        return initial_transitions.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function add_constant
     * @description Interface function for adding new constants definitions.
     * @param constant {Object} The characteristics of the constants that shall be added to the diagram.
     * @returns {Object} The descriptor of the constant.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_constant = function (constant) {
        // we use name and type as ID so that we automatically avoid duplicated constants
        // (those three fields together identify constants uniquely)
        // note: value can be undefined
        var id = createConstantID(constant);
        var newConstant = {
            id: id,
            name: constant.name,
            type: constant.type,
            value: constant.value
        };
        this.constants.set(id, newConstant);
        // fire event
        this.fire({
            type: "emuCharts_constantAdded",
            constant: {
                id: id,
                name: constant.name,
                type: constant.type,
                value: constant.value
            }
        });
        return newConstant;
    };

    /**
     * @function add_datatype
     * @description Interface function for adding new datatype definitions.
     * @param datatype {Object} The characteristics of the datatype that shall be added to the diagram.
     * @returns {Object} The descriptor of the datatype.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_datatype = function (datatype) {
        // we use the datatype name as ID for the datatype
        var id = createDatatypeID(datatype);
        var newDatatype = {
            id: id,
            name: datatype.name,
            constructors: datatype.constructors
        };
        this.datatypes.set(id, newDatatype);
        // fire event
        this.fire({
            type: "emuCharts_datatypeAdded",
            constant: {
                id: id,
                name: datatype.name,
                constructors: datatype.constructors
            }
        });
        return newDatatype;
    };

    /**
     * @function add_variable
     * @description Interface function for adding new state variables definitions to the diagram.
     * @param variable {Object} The characteristics of the variable that shall be added to the diagram.
     * @returns {Object} The descriptor of the variable.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_variable = function (variable) {
        // we use name type and scope as ID so that we automatically avoid duplicated variables
        // (those three fields together identify variables uniquely)
        var id = createVariableID(variable);
        var newVariable = {
            id: id,
            name: variable.name,
            type: variable.type,
            value: variable.value,
            scope: variable.scope
        };
        this.variables.set(id, newVariable);
        // fire event
        this.fire({
            type: "emuCharts_variableAdded",
            variable: {
                id: id,
                name: variable.name,
                type: variable.type,
                value: variable.value,
                scope: variable.scope
            }
        });
        return newVariable;
    };

    /**
     * @function remove_constant
     * @description Interface function for removing a constant from the diagram.
     * @param constantID {String} The identifier of the constant that shall be removed.
     * @returns {Boolean} true if constant removed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_constant = function (constantID) {
        var rem = this.constants.get(constantID);
        if (rem && this.constants.remove(constantID)) {
            // fire event
            this.fire({
                type: "emuCharts_constantRemoved",
                constant: rem
            });
            return true;
        }
        return false;
    };

    /**
     * @function remove_datatype
     * @description Interface function for removing a datatype definition.
     * @param datatypeID {String} The identifier of the datatype that shall be removed.
     * @returns {Boolean} true if datatype removed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_datatype = function (datatypeID) {
        var rem = this.datatypes.get(datatypeID);
        if (rem && this.datatypes.remove(datatypeID)) {
            // fire event
            this.fire({
                type: "emuCharts_datatypeRemoved",
                constant: rem
            });
            return true;
        }
        return false;
    };

    /**
     * @function remove_variable
     * @description Interface function for removing a state variable from the diagram.
     * @param variableID {String} The identifier of the variable that shall be removed.
     * @returns {Boolean} true if variable removed successfully, false otherwise.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.remove_variable = function (variableID) {
        var rem = this.variables.get(variableID);
        if (rem && this.variables.remove(variableID)) {
            // fire event
            this.fire({
                type: "emuCharts_variableRemoved",
                variable: rem
            });
            return true;
        }
        return false;
    };

    /**
     * @function rename_constant
     * @description Interface function for renaming (i.e., editing) a constant
     * @param constantID {String} A unique identifier for the constant.
     * @param newData {Object} Constants data, an object containing the following fields
     *           <li> type (String): the constant type (e.g., "real"). This field is mandatory.</li>
     *           <li> name (String): the constant name (e.g., "max"). This field is mandatory.</li>
     *           <li> value (String): the constant value (e.g., "5"). This field is optional.</li>
     * @returns {Boolean} true if the constant was renamed successfully; otherwise returns false.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_constant = function (constantID, newData) {
        if (!constantID || !this.constants || !this.constants.get(constantID)) { return false; }
        // get the constant, delete it from the constants list,
        // rename fields, and put it back in the constants list
        var theConstant = this.constants.get(constantID);
        this.constants.remove(constantID);
        var newConstant = {
            type: newData.type || theConstant.type,
            name: newData.name || theConstant.name,
            value: newData.value || theConstant.value
        };
        // update constantID
        var newConstantID = createConstantID(newConstant);
        newConstant.id = newConstantID;
        this.constants.set(newConstantID, newConstant);
        this.fire({
            type: "emuCharts_constantRenamed",
            pre: {
                id: theConstant.id,
                type: theConstant.type,
                name: theConstant.name,
                value: theConstant.value
            },
            post: {
                id: newConstant.id,
                type: newConstant.type,
                name: newConstant.name,
                value: newConstant.value
            }
        });
        return true;
    };

    /**
     * @function rename_datatype
     * @description Interface function for renaming (i.e., editing) a datatype
     * @param constantID {String} A unique identifier for the datatype.
     * @param newData {Object} Datatype data, an object containing the following fields
     *           <li> name (String): the datatype name (e.g., "MultiDisplay"). This field is mandatory.</li>
     *           <li> type (Array): the datatype constructors (e.g., "[ rate(x: real), vtbi(x:real) ]"). This field is mandatory.</li>
     *           <li> value (String): the initial value (e.g., "rate(0)"). This field is mandatory.</li>
     * @returns {Boolean} true if the datatype was renamed successfully; otherwise returns false.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_datatype = function (datatypeID, newData) {
        if (!datatypeID || !this.datatypes || !this.datatypes.get(datatypeID)) { return false; }
        // get the datatype, delete it from the datatype list,
        // rename fields, and put it back in the datatypes list
        var theDatatype = this.datatypes.get(datatypeID);
        this.datatypes.remove(datatypeID);
        var newDatatype = {
            name: newData.name || theDatatype.name,
            constructors: newData.constructors || theDatatype.constructors
        };
        // update datatypeID
        var newDatatypeID = createDatatypeID(newDatatype);
        newDatatype.id = newDatatypeID;
        this.datatypes.set(newDatatypeID, newDatatype);
        this.fire({
            type: "emuCharts_datatypeRenamed",
            pre: {
                id: theDatatype.id,
                constructors: theDatatype.constructors,
                name: theDatatype.name
            },
            post: {
                id: newDatatype.id,
                constructors: newDatatype.type,
                name: newDatatype.name
            }
        });
        return true;
    };

    /**
     * @function rename_variable
     * @description Interface function for renaming (i.e., editing) a state variable.
     * @param variableID {String} is the unique variable identifier
     * @param newData {Object} The new characteristics of the variable: { type: (string), name: (string), scope: (string) }
     * @returns {Boolean} true if variable renamed successfully; otherwise returns false
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_variable = function (variableID, newData) {
        if (!variableID || !this.variables || !this.variables.get(variableID)) { return false; }
        // get the varable, delete it from the variables list,
        // rename fields, and put it back in the variables list
        var theVariable = this.variables.get(variableID);
        this.variables.remove(variableID);
        var newVariable = {
            type: newData.type || theVariable.type,
            name: newData.name || theVariable.name,
            value: newData.value || theVariable.value,
            scope: newData.scope || theVariable.scope
        };
        // update variableID
        var newVariableID = createVariableID(newVariable);
        newVariable.id = newVariableID;
        this.variables.set(newVariableID, newVariable);
        this.fire({
            type: "emuCharts_variableRenamed",
            pre: {
                id: theVariable.id,
                type: theVariable.type,
                name: theVariable.name,
                value: theVariable.value,
                scope: theVariable.scope
            },
            post: {
                id: newVariable.id,
                type: newVariable.type,
                name: newVariable.name,
                value: newVariable.value,
                scope: newVariable.scope
            }
        });
        return true;
    };

    /**
     * @function getConstants
     * @description Returns the constants defined in the diagram.
     * @returns {Array(Object)} An array of constants descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getConstants = function () {
        var ans = [], _this = this;
        _this.constants.forEach(function (key) {
            var c = _this.constants.get(key);
            ans.push({
                id: c.id,
                name: c.name,
                type: c.type,
                value: c.value
            });
        });
        return ans.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function getConstant
     * @descriptionb Returns the constant with ID given by the function argument
     * @param constantID Constant identifier
     * @returns The constant descriptor
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getConstant = function (constantID) {
        return this.constants.get(constantID);
    };


    /**
     * @function getDatatypes
     * @description Returns all datatypes defined in the diagram.
     * @returns {Array(Object)} An array of datatype descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getDatatypes = function () {
        var ans = [], _this = this;
        _this.datatypes.forEach(function (key) {
            var c = _this.datatypes.get(key);
            ans.push({
                id: c.id,
                name: c.name,
                constructors: c.constructors,
                value: c.value
            });
        });
        return ans.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function getDatatype
     * @descriptionb Returns the datatype with ID given by the function argument
     * @param datatypeID Datatype identifier
     * @returns The datatype descriptor
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getDatatype = function (datatypeID) {
        return this.datatypes.get(datatypeID);
    };

    /**
     * @function getVariables
     * @descriptionb Returns the variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getVariables = function (scope) {
        var ans = [], _this = this;
        _this.variables.forEach(function (key) {
            var v = _this.variables.get(key);
            if (!scope || scope === v.scope) {
                ans.push({
                    id: v.id,
                    name: v.name,
                    type: v.type,
                    value: v.value,
                    scope: v.scope
                });
            }
        });
        return ans.sort(function (a, b) {
            return a.name.localeCompare(b.name);
        });
    };

    /**
     * @function getVariable
     * @descriptionb Returns the variable with ID given by the function argument
     * @param variableID Variable identifier
     * @returns The variable descriptor
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getVariable = function (variableID) {
        return this.variables.get(variableID);
    };

    /**
     * @function getInputVariables
     * @description Returns the input variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getInputVariables = function () {
        return this.getVariables("Input");
    };

    /**
     * @function getOutputVariables
     * @description Returns the output variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getOutputVariables = function () {
        return this.getVariables("Output");
    };

    /**
     * @function getLocalVariables
     * @description Returns the local variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getLocalVariables = function () {
        return this.getVariables("Local");
    };

    /**
     * @function getVariableScopes
     * @description Returns the supported variable scopes.
     * @returns {Array(String)} An array of Strings specifying the variable scopes supported by the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getVariableScopes = function () {
        return ["Local", "Input", "Output"];
    };

    /**
     * @function empty
     * @description Utility function that checks whether the diagram is empty (i.e., 0 nodes, 0 edges)
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.empty = function () {
        return this.nodes.empty() && this.edges.empty();
    };

    /** PIM **/

    /**
     * Convert the current Emuchart to a PIM (or if from a PIM).
     * @returns {boolean} True Emuchart became a PIM or a PIM became an Emuchart.
     */
    Emucharts.prototype.toPIM = function (toPIM) {
        return this.pim && this.pim.toPIM ? this.pim.toPIM(toPIM) : false;
    };

    /**
     * Returns if this emuchart is a PIM.
     * @returns {boolean} If this emuchart is a PIM.
     */
    Emucharts.prototype.getIsPIM = function () {
        return this.pim && this.pim.getIsPIM ? this.pim.getIsPIM() : false;
    };

    /**
     *
     * @param behaviour
     * @returns If no behaviour provided returns all PMR as a set,
     * If behaviour could be found then returns the relation (behaviour, operation),
     * else returns null.
     */
    Emucharts.prototype.getPMR = function (behaviour, isSave) {
        return this.pim && this.pim.getPMR ? this.pim.getPMR(this.pmr, behaviour, isSave) : d3.map();
    };

    /**
     * Add a PMR (overrites any existing PMR for the given behaviour).
     * ({behaviour (string), operation (string)}).
     * @param pmr
     * @returns boolean true if successfully added.
     */
    Emucharts.prototype.addPMR = function (pmr) {
        return this.pim && this.pim.addPMR ? this.pim.addPMR(this.pmr, pmr) : false;
    };

    /**
     * Saves the new PMRs into the pool of all PMRs
     * @param newPMRs
     * @returns {boolean}
     */
    Emucharts.prototype.mergePMR = function (newPMRs) {
        return this.pim.mergePMR ? this.pim.mergePMR(this.pmr, newPMRs) : false;
    };

    Emucharts.prototype.getEmuchartsVersion = function () {
        return emuchartsVersion;
    };

    module.exports = Emucharts;
});
