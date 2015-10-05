/**
 * @module Emucharts
 * @version 1.0
 * @description
 * Emucharts encodes an emuchart diagram into a graphs. This is code is a re-engineered
 * version of stateMachine.js implemented in branch emulink-commented
 * @author Paolo Masci
 * @date 14/05/14 2:53:03 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var eventDispatcher = require("util/eventDispatcher");
    var _this;

    var defaultValues = { x: 100, y: 100, width: 36, height: 36, fontSize: 10 };

    var getFreshNodeID = function () {
        function newNodeID () {
            var i = 1;
            while(_this.nodes && _this.nodes.get("X" + i)) { i++; }
            return i;
        }
        return "X" + newNodeID();
    };
    var getFreshEdgeID = function () {
        function newEdgeID () {
            var i = 1;
            while(_this.edges && _this.edges.get("T" + i)) { i++; }
            return i;
        }
        return "T" + newEdgeID();
    };
    var getFreshInitialEdgeID = function () {
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

    /**
     * @function Emucharts
     * @description Constructor.
     * @memberof module:Emucharts
     * @instance
     */
    function Emucharts(emuchart) {
        if (emuchart) {
            this.nodes = emuchart.nodes || d3.map();
            this.edges = emuchart.edges || d3.map();
            this.initial_edges = emuchart.initial_edges || d3.map();
            this.constants = emuchart.constants || d3.map();
            this.variables = emuchart.variables || d3.map();
        } else {
            this.nodes = d3.map();
            this.edges = d3.map();
            this.initial_edges = d3.map();
            this.variables = d3.map();
            this.constants = d3.map();
        }
        eventDispatcher(this);
        _this = this;
        return this;
    }

    Emucharts.prototype.getEdges = function () {
        return _this.edges;
    };
    Emucharts.prototype.getInitialEdges = function () {
        return _this.initial_edges;
    };
    Emucharts.prototype.getNodes = function () {
        return _this.nodes;
    };

    Emucharts.prototype.getDefaultValues = function () {
        return defaultValues;
    };


    /**
     * @function rename_node
     * @description Renames a node (i.e., a state) in the emuchart diagram.
     * @param id {String} Identifier of the node that shall be renamed.
     * @param newName {String} New name that shall be assigned to the node.
     * @returns {Boolean} true if node renamed successfully; otherwise returns false
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_node = function (id, newName) {
        if (!id || !_this.nodes || !_this.nodes.get(id)) { return false; }
        // get node and rename it
        var node = _this.nodes.get(id);
        node.name = newName;
        _this.nodes.set(node.id, node);
        // we need to rename also nodes cached in the edge structure
        // this can be quite expensive in term of time, but renaming is unlikely to be a frequent operation
        // so the time cost is acceptable (given that caching is quite useful to speed up rendering)
        if (_this.edges) {
            _this.edges.forEach(function (key) {
                var edge = _this.edges.get(key);
                var dirty = false;
                if (edge.source.id === id) {
                    edge.source.name = newName;
                    dirty = true;
                }
                if (edge.target.id === id) {
                    edge.target.name = newName;
                    dirty = true;
                }
                if (dirty) { _this.edges.set(key, edge); }
            });
        }
        _this.fire({
            type: "emuCharts_stateRenamed",
            state: {
                id: node.id,
                name: node.name
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
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.add_node = function (node) {
        if (!node) { return null; }
        // create a new node with a unique ID
        var id = getFreshNodeID();
        var name = node.name || id;
        var estimatedTextWidth = name.length * defaultValues.fontSize / 4;
        var width = (estimatedTextWidth < defaultValues.width) ? defaultValues.width : estimatedTextWidth;
        var newNode = {
                id  : id, // nodes have unique IDs
                name: name,
                x: node.x || defaultValues.x,
                y: node.y || defaultValues.y,
                width : width,
                height: defaultValues.height
            };
        // add the new node to the diagram
        _this.nodes.set(newNode.id, newNode);
        // fire event
        _this.fire({
            type: "emuCharts_stateAdded",
            state: {
                id: id,
                name: name
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
        var rem = _this.nodes.get(node);
        if (rem && _this.nodes.remove(node)) {
            // fire event
            _this.fire({
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
        var move = _this.nodes.get(node);
        if (move) {
            // fire event
            _this.fire({
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
            _this.nodes.set(move.id, move);
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
        if (!id || !_this.edges || !_this.edges.get(id)) { return false; }
        // get edge and rename it
        var edge = _this.edges.get(id);
        edge.name = newName;
        _this.edges.set(edge.id, edge);
        // fire event
        _this.fire({
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
        if (!id || !_this.initial_edges || !_this.initial_edges.get(id)) { return false; }
        // get edge and rename it
        var initial_edge = _this.initial_edges.get(id);
        initial_edge.name = newName;
        _this.initial_edges.set(initial_edge.id, initial_edge);
        // fire event
        _this.fire({
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
        var ed = _this.edges.get(edge.id);
        if (ed) {
            ed.controlPoint = cp;
            _this.edges.set(edge.id, ed);
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
        if (!_this.nodes.has(edge.target.id)) {
            console.log("dbg: warning, target ID not found in emuchart data. New transitions not added.");
            return null;
        }

        var target = _this.nodes.get(edge.target.id);
        var source = (edge.source) ? _this.nodes.get(edge.source.id) : null;
        // create a new node with a unique ID
        var id = getFreshEdgeID();
        var newEdge = {
                id  : id, // nodes have unique IDs
                name: edge.name || id,
                source: source,
                target: target,
                controlPoint: edge.controlPoint
            };
        // add the new edge to the diagram
        _this.edges.set(newEdge.id, newEdge);
        // fire event
        _this.fire({
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
        if (!_this.nodes.has(edge.target.id)) {
            console.log("dbg: warning, target ID not found in emuchart data. New transitions not added.");
            return null;
        }

        var target = _this.nodes.get(edge.target.id);
        // create a new node with a unique ID
        var id = getFreshInitialEdgeID();
        var newEdge = {
                id  : id, // nodes have unique IDs
                name: edge.name || id,
                target: target
            };
        // add the new edge to the diagram
        _this.initial_edges.set(newEdge.id, newEdge);
        // fire event
        _this.fire({
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
        var rem = _this.edges.get(edge);
        if (rem && _this.edges.remove(edge)) {
            // fire event
            _this.fire({
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
        var rem = _this.initial_edges.get(initial_edge);
        if (rem && _this.initial_edges.remove(initial_edge)) {
            // fire event
            _this.fire({
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
        return getFreshNodeID();
    };

    /**
     * @function getFreshTransitionName
     * @description Returns a fresh (i.e., not used by other transitions in the diagram) transitions name.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getFreshTransitionName = function () {
        return getFreshEdgeID();
    };

    /**
     * @function getFreshInitialTransitionName
     * @description Returns a fresh (i.e., not used by other initial transitions in the diagram) initial transitions name
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getFreshInitialTransitionName = function () {
        return getFreshInitialEdgeID();
    };

    /**
     * @function getStates
     * @description Returns the states in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the states in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getStates = function () {
        var states = [];
        _this.nodes.forEach(function (key) {
            var node = _this.nodes.get(key);
            states.push({
                name: node.name,
                id: node.id,
                x: node.x,
                y: node.y,
                width : node.width,
                height: node.height
            });
        });
        return states;
    };

    /**
     * @function getState
     * @description Returns the descriptor of a state.
     * @param id {String} The identifier of the state.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getState = function (id) {
        return _this.nodes.get(id);
    };

    /**
     * @function getTransitions
     * @description Returns the descriptor of a transition.
     * @param id {String} The identifier of the transition.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getTransition = function (id) {
        return _this.edges.get(id);
    };

    /**
     * @function getTransitions
     * @description Returns the transitions in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the transitions in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getTransitions = function () {
        var transitions = [];
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
        return transitions;
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
        return _this.initial_edges.get(id);
    };

    /**
     * @function getInitialTransitions
     * @description Returns the initial transitions in the diagram.
     * @returns {Array(Object)} An array containing the descriptors of the transitions in the diagram.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getInitialTransitions = function () {
        var initial_transitions = [];
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
        return initial_transitions;
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
        _this.constants.set(id, newConstant);
        // fire event
        _this.fire({
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
        _this.variables.set(id, newVariable);
        // fire event
        _this.fire({
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
        var rem = _this.constants.get(constantID);
        if (rem && _this.constants.remove(constantID)) {
            // fire event
            _this.fire({
                type: "emuCharts_constantRemoved",
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
        var rem = _this.variables.get(variableID);
        if (rem && _this.variables.remove(variableID)) {
            // fire event
            _this.fire({
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
        if (!constantID || !_this.constants || !_this.constants.get(constantID)) { return false; }
        // get the constant, delete it from the constants list,
        // rename fields, and put it back in the constants list
        var theConstant = _this.constants.get(constantID);
        _this.constants.remove(constantID);
        var newConstant = {
            type: newData.type || theConstant.type,
            name: newData.name || theConstant.name,
            value: newData.value || theConstant.value
        };
        // update constantID
        var newConstantID = createConstantID(newConstant);
        newConstant.id = newConstantID;
        _this.constants.set(newConstantID, newConstant);
        _this.fire({
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
     * @function rename_variable
     * @description Interface function for renaming (i.e., editing) a state variable.
     * @param variableID {String} is the unique variable identifier
     * @param newData {Object} The new characteristics of the variable: { type: (string), name: (string), scope: (string) }
     * @returns {Boolean} true if variable renamed successfully; otherwise returns false
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.rename_variable = function (variableID, newData) {
        if (!variableID || !_this.variables || !_this.variables.get(variableID)) { return false; }
        // get the varable, delete it from the variables list,
        // rename fields, and put it back in the variables list
        var theVariable = _this.variables.get(variableID);
        _this.variables.remove(variableID);
        var newVariable = {
            type: newData.type || theVariable.type,
            name: newData.name || theVariable.name,
            value: newData.value || theVariable.value,
            scope: newData.scope || theVariable.scope
        };
        // update variableID
        var newVariableID = createVariableID(newVariable);
        newVariable.id = newVariableID;
        _this.variables.set(newVariableID, newVariable);
        _this.fire({
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
        var ans = [];
        _this.constants.forEach(function (key) {
            var c = _this.constants.get(key);
            ans.push({
                id: c.id,
                name: c.name,
                type: c.type,
                value: c.value
            });
        });
        return ans;
    };

    /**
     * @function getVariables
     * @descriptionb Returns the variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getVariables = function (scope) {
        var ans = [];
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
        return ans;
    };

    /**
     * @function getInputVariables
     * @description Returns the input variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getInputVariables = function () {
        return _this.getVariables("Input");
    };

    /**
     * @function getOutputVariables
     * @description Returns the output variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getOutputVariables = function () {
        return _this.getVariables("Output");
    };

    /**
     * @function getLocalVariables
     * @description Returns the local variables defined in the diagram.
     * @returns {Array(Object)} An array of variables descriptors.
     * @memberof module:Emucharts
     * @instance
     */
    Emucharts.prototype.getLocalVariables = function () {
        return _this.getVariables("Local");
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
        return _this.nodes.empty() && _this.edges.empty();
    };

    module.exports = Emucharts;
});
