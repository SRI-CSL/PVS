/**
 * wrapper around the generic websocket client specifically for pvsio websocket functions
 * @author Patrick Oladimeji
 * @date 6/4/13 21:58:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise*/
define(function (require, exports, module) {
    "use strict";
    var wsclient            = require("websockets/wsClient"),
        eventDispatcher     = require("util/eventDispatcher"),
        property            = require("util/property"),
        Logger				= require("util/Logger"),
        PVSioStateParser	= require("util/PVSioStateParser"),
        wsSingleton,
        keepAlive = false;

    function createWebSocket() {
        var wscBase = wsclient();
        var o  = eventDispatcher({});
        /**
            Get or set the server url for the websocket connection
        */
        o.serverUrl = function (url) {
            if (url) {
                wscBase.url(url);
                return o;
            }
            return wscBase.url();
        };
        /**
            Get or set the port for the websocket connection
        */
        o.port = function (port) {
            if (port) {
                wscBase.port(port);
                return o;
            }
            return wscBase.port();
        };

        /**
            Get or set the last state of the model being executed in pvsio
        */
        o.lastState = property.call(o, "init(0)");
        /**
            Initiates a connection to the pvsioweb websocket server.
            Returns a promise that resolves when the connection is opened
        */
        o.logon = function () {
            return new Promise(function (resolve, reject) {
                wscBase.logon().then(function (res) {
                    if (keepAlive) {
                        setInterval(function () {
                            wscBase.send({ type: "keepAlive" });
                            // console.log("Sending keepalive message...");
                        }, 6000);
                    }
                    resolve(o);
                }).catch(function (err) { console.log(err); reject(err); });
            });
        };

        /**
            Closes the connection to the pvsioweb server
        */
        o.close = function () {
            wscBase.close();
        };

        /**
            Starts the pvsio process with the parameters supplied
            @param {{name: string, projectName: ?string, demoName: ?string}} data This contains information about the file to start and the folder containing that file relative to the server public folder
            @param {callback} cb The callback function to invoke when the process has started
        */
        o.startPVSProcess = function (data, cb) {
            if (data && data.name) {
                var sourceFile = data.name.split(".pvs")[0];
                wscBase.send({
                    type: "startProcess",
                    data: {
                        name: sourceFile,
                        projectName: data.projectName,
                        demoName: data.demoName
                    }
                }, cb);
            } else {
                console.log("ERROR: Failed to load pvs file " + data.demoName + "/" + data.name);
            }
        };

        /**
         * @function java
         * @desc Executes a java command
         * @param {Function} cb Callback function invoked when the command execution completes
         */
        o.java = function (javaFile, data, cb) {
            if (javaFile) {
                wscBase.send({
                    type: "java",
                    data: {
                        javaFile: javaFile,
                        argv: data.argv,
                        javaOptions: data.javaOptions,
                        basePath: data.basePath
                    }
                }, function (err, res) {
                    if (err) {
                        console.log(err);
                    } else {
                        console.log(res);
                        if (cb && typeof cb === "function") {
                            cb(res.stderr, res.stdout);
                        }
                    }
                });
            }
        };


        /**
         * @function ctrl
         * @desc Sends input control commands
         * @param {Function} cb Callback function invoked when the command execution completes
         */
        o.ctrl = function (data, cb) {
            wscBase.send({
                type: "ctrl",
                data: data
            }, function (err, res) {
                if (err) {
                    console.error(err);
                } else {
                    // console.log(res);
                    if (cb && typeof cb === "function") {
                        cb(res.stderr, res.stdout);
                    }
                }
            });
        };

        /**
            Closes the pvsio process attributed to this websocket connection if there is one
            @param {function} cb The function to invoke when process has been closed
        */
        o.closePVSProcess = function (cb) {
            wscBase.send({type: "closeProcess"}, cb);
        };

        /**
            Sends a user interface command to be executed by the pvsio process. This method fires a "GraphUpdate" event whenever there is a response from the server due to the callback
            @param {string} action The action to send to the process
            @param {callback} cb The function to invoke with the results of performing the passed action on the process
        */
        o.sendGuiAction = function (action, cb) {
            if (action === "<ping>" || action === "<pong>") {
                var type = action.replace(/</,"").replace(/>/,"");
                wscBase.send({type: type, data: { command: action }}, function (err, res) {
                    if (cb && typeof cb === "function") {
                        cb(err, res);
                    }
                });
            } else {
                wscBase.send({type: "sendCommand", data: {command: action}}, function (err, res) {
                    console.log("data received: ", res);
                    if (res) {
                        if (res.json) {
                            console.log("json data: ", res.json);
                        } 
                        if (res.data && res.data !== "") {
                            //do stuff to update the explored state graph and invoke the callback with the same parameters
                            wscBase.fire({type: "GraphUpdate", transition: action, target: res.data, source: o.lastState()});
                            //update the lastState if it was a valid pvsio state
                            if (PVSioStateParser.isState(res.data)) {
                                o.lastState(res.data);
                            }
                        } else {
                            console.error("Warning: expression received from PVS is invalid: ", res);
                        }
                        if (cb && typeof cb === "function") {
                            cb(err, res);
                        }
                    } else {
                        Logger.log("Warning: PVSio was not able to evalute expression ", action);
                        Logger.log(res.data);
                        //update res.data with previous valid state
                        res.data = o.lastState();
                    }
                });
            }
            wscBase.fire({type: "InputUpdated", data: action});
            return o;
        };
        o.send = function (action, cb) {
            return o.sendGuiAction(action, cb);
        };

        o.registerReceiver = function (channelID, cb) {
            return wscBase.registerReceiver(channelID, cb);
        };

        /**
            Gets the content of the file passed in the parameter
            @param {string} path The relative path (from the base project dir) to the file whose content is desired
            @param {callback} cb The function to invoke when content has been loaded. res parameter contains file content.
        */
        //DEPRECATED: we can use this function only for utf8 files --> this function has been replaced by readFile
        o.getFile = function (path, cb) {
            var token = {type: "readFile", path: path};
            token.name = token.path.split("/").slice(-1).join("");
            wscBase.send(token, cb);
            return o;
        };
        /**
            Reads the content of the file whose filename is specified in the descriptor
            @param {{path: String, encoding: String}} token The details of the file to read.
            @param {callback} cb The function to invoke when content has been loaded. res parameter contains file content.
        */
        o.readFile = function (token, cb) {
            token.type = "readFile";
            token.name = token.path.split("/").slice(-1).join("");
            wscBase.send(token, cb);
            return o;
        };
        /**
            Writes the content passed using the specified name
            @param {{path: String, content: String, projectName: String}} token The details of the file to write.
                Note that file path is relative to the base project directory
            @param {callback} cb The callback to invoke with the result of the write operation.
        */
        o.writeFile = function (token, cb) {
            token.type = "writeFile";
            token.name = token.path.split("/").slice(-1).join("");
            wscBase.send(token, cb);
            return o;
        };

        /**
         * @function deleteFile
         * @description Deletes the file passed as argument. The file path must start with the project name.
         * @param token {Object{path: String, projectName: String}} The details of the file that shall be deleted.
         *        Note: the file path is relative to the base project directory
         * @param cb {function} The callback function that shall be invoked at the end, when the delete result is ready.
         */
        o.deleteFile = function (token, cb) {
            token.type = "deleteFile";
            token.name = token.path.split("/").slice(-1).join("");
            wscBase.send(token, cb);
            return o;
        };

        /**
         * @function deleteDirectory
         * @description Deletes the directory passed as argument.
         *      The directory path must start with the project name, and end with  "/"
         * @param path {String} The path of the directory that shall be deleted. The path is relative to the current project.
         *       Note: the file path is relative to the base project directory
         * @param cb {function} The callback function that shall be invoked at the end, when the delete result is ready.
         */
        o.deleteDirectory = function (path, cb) {
            wscBase.send({type: "deleteDirectory", path: path}, cb);
            return o;
        };

//        /**
//         * @function createProject
//         * @description Creates a new project on the basis of the information passed as argument.
//         * @param token {Object} The specification of the project that shall be created. The object has the following properties:
//         *      <li> projectName (String): the name of the project. This property is mandatory.</li>
//         *      <li> projectFiles (Array<Descriptor>): file descriptors of files contained in the project</li>
//         *      <li> overWrite (bool): a boolean flag that defines whether the project folder shall overwritten in the case the project is already present. This property is optional.</li>
//         *      <li> mainPVSFile (Descriptor): the descriptor of the main PVS file. This property is optional.</li>
//         *      <li> prototypeImage (Descriptor): the descriptor of the prototype image. This property is optional.</li>
//         *        Note: the project path will be a folder in the pvsioweb/example/project folder.
//         * @param cb {function} The callback function that shall be invoked at the end, when the project is ready.
//         */
//        o.createProject = function (token, cb) {
//            token.type = "createProject";
//            wscBase.send(token, cb);
//            return o;
//        };

        /**
            creates a directory with the specified path.
            @param {string} path the path to the directory to create. This path is relative to the base project directory
            @param {callback} cb the callback function to invoke when the function returns from the server
        */
        o.writeDirectory = function (path, cb) {
            wscBase.send({type: "writeDirectory", path: path}, cb);
            return o;
        };

        /**
            Sends a generic message to the server to call a function on the server
            @param {} token The JSON token to send to the server
            @param {callback} The callback function to invoke once the server responds with a message
        */
        o.send = function (token, cb) {
            wscBase.send(token, cb);
            return o;
        };

        /**
            Add an event listener of the specified type
            @param {string} type The string specifying the type of event
            @param {eventCallback} callback The function to invoke when event 'type' occurs
        */
        o.addListener = function (type, callback) {
            wscBase.addListener(type, callback);
            return o;
        };

        o.removeListener = function (type, method) {
            wscBase.removeListener(type, method);
            return o;
        };

        o.clearListeners = function () {
            wscBase.clearListeners();
            return o;
        };
        return o;
    }

    module.exports = function () {
        wsSingleton = wsSingleton || createWebSocket();
        return wsSingleton;
    };

/**
 * @callback callback
 * @param {object} err This value is set if any error occurs during the save operation.
 * @param {obj} res The response data. The type varies depending on the caller
 */
/**
 * @callback eventCallback
 * @param {object} event
 */
});
