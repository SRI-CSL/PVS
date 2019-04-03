/**
 *
 * @author Paolo Masci
 * @date Oct 31, 2016
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3*/
define(function (require, exports, module) {
    "use strict";

    var UppaalParser = require("plugins/emulink/tools/UppaalParser");

    var instance;

    function UppaalConverter() {
        return this;
    }

    /**
     * @function openUppaalV4
     * @memberof module:UppaalConverter
     * @instance
     * @description Returns a descriptor of the Uppaal model
     * @param XMLFile The XML file of the Uppaal model
     * @returns {JSON Object} A JSON object representing the Uppaal model
     */
    UppaalConverter.prototype.openUppaalV4 = function (XMLFile) {
        var name = XMLFile.name.substring(0, XMLFile.name.lastIndexOf(".xml"));
        var parser = new DOMParser();
        var xmlDoc = parser.parseFromString(XMLFile.content,"text/xml");
        var init = xmlDoc.getElementsByTagName("init");
        var locations = xmlDoc.getElementsByTagName("location");
        var transitions = xmlDoc.getElementsByTagName("transition");
        var declarations = xmlDoc.getElementsByTagName("declaration");
        var parameters = xmlDoc.getElementsByTagName("parameter");
        return { name: name,
                 init: init,
                 locations: locations,
                 transitions: transitions,
                 declarations: declarations,
                 parameters: parameters };
    };

    /**
     * @function Uppaal2Emucharts
     * @memberof module:UppaalConverter
     * @instance
     * @description Converts an Uppaal file descriptor into an Emucharts file descriptor
     * @param {JSON Object} uDesc The Uppaal file descriptor
     * @returns {JSON Object} Uppaal descriptor
     */
    UppaalConverter.prototype.Uppaal2Emucharts = function (uDesc) {
        if (!this._parser) {
            this._parser = new UppaalParser();
        }
        var parser = this._parser;
        var emuDesc = {
            states: [],
            transitions: [],
            initial_transitions: [],
            variables: [],
            constants: [],
            author: "",
            name: uDesc.name
        };
        var i = 0, j = 0;
        //--- states
        var states = d3.map();
        for (i = 0; i < uDesc.locations.length; i++) {
            states.set(uDesc.locations[i].id, {
                id: uDesc.locations[i].id,
                name: uDesc.locations[i].childNodes[0].childNodes[0].nodeValue,
                x: parseFloat(uDesc.locations[i].getAttribute("x")),
                y: parseFloat(uDesc.locations[i].getAttribute("y"))
            });
        }
        //--- initial state
        emuDesc.states = states.values();
        for (i = 0; i < uDesc.init.length; i++) {
            emuDesc.initial_transitions.push({
                id: "init",
                name: "",
                target: states.get(uDesc.init[i].getAttribute("ref"))
            });
        }
        //--- transitions
        for (i = 0; i < uDesc.transitions.length; i++) {
            var name = "", guard = "", assignment = "";
            var source = null, target = null;
            for (j = 0; j < uDesc.transitions[i].childNodes.length; j++) {
                var tagName = uDesc.transitions[i].childNodes[j].tagName;
                if (tagName === "source") {
                    var sourceID = uDesc.transitions[i].childNodes[j].getAttribute("ref");
                    source =  states.get(sourceID);
                } else if (tagName === "target") {
                    var targetID = uDesc.transitions[i].childNodes[j].getAttribute("ref");
                    target =  states.get(targetID);
                } else if (tagName === "label") {
                    var kind = uDesc.transitions[i].childNodes[j].getAttribute("kind");
                    if (kind === "guard") {
                        guard = "[ " + uDesc.transitions[i].childNodes[j].childNodes[0].nodeValue + " ]";
                        guard = guard.replace(/==/g, "="); // == in Uppaal guards corresponds to = in Emucharts conditions
                    } else if (kind === "assignment") {
                        assignment = "{ " + uDesc.transitions[i].childNodes[j].childNodes[0].nodeValue + " }";
                        assignment = assignment.replace(/=/g, ":=").replace(/,/g, ";"); // = in Uppaal assignment corresponds to := in Emucharts actions
                    }
                }
            }
            emuDesc.transitions.push({
                id: "T_" + i,
                name: name + guard + assignment,
                source: source,
                target: target
            });
        }
        //--- declarations
        for (i = 0; i < uDesc.declarations.length; i++) {
            var declarations = uDesc.declarations[i].childNodes[0].nodeValue.split("\n");
            declarations = declarations.filter(function (dec) { return dec.indexOf("//") < 0 && dec.trim() !== ""; }); // this is needed to skip comments in the Uppaal file
            for (j = 0; j < declarations.length; j++) {
                var theDeclaration = parser.parseDeclaration(declarations[j]);
                if (theDeclaration.err) {
                    console.log(theDeclaration.err);
                } else {
                    var theVariable = {
                            id: "",
                            type: "",
                            name: "",
                            value: "",
                            scope: "Local"
                        };
                    if (theDeclaration.res.type === "TypeDeclaration") {
                        // if (theDeclaration.res.val.type && theDeclaration.res.val.type.typeID && theDeclaration.res.val.type.typeID.val)
                        // TODO: we are missing a field to encode this information in the emucharts descriptor
                    } else if (theDeclaration.res.type === "VariableDeclaration") {
                        theVariable.type = theDeclaration.res.val.type.typeID.val;
                        theVariable.name = theDeclaration.res.val.variableID.val;
                        theVariable.id = "VAR_" + theVariable.type + "_" + theVariable.name;
                        //theVariable.value = ...
                        emuDesc.variables.push(theVariable);
                    }
                }
            }
        }
        // return the emucharts descriptor
        return emuDesc;
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new UppaalConverter();
            }
            return instance;
        }
    };
});
