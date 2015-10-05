//#!/usr/bin/env node
/**
Copyright (c) 2012

Patrick Oladimeji
This file is part of pvsio-web.

pvsio-web is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

pvsio-web is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * This file creates a connection to a pvsio process run locally or at specified host.
 * It also creates an express webserver to serve demo applications e.g. the infusion
 * pump pvsio demo.
 * The websocket connection started by this process listens for 3 commands:
 * sendCommand: used to send a pvsio command to the processs
 * startProcess: used to start the pvsio process
 * getSourceCode: used to get the source code of the pvs code being executed
 * @author patrick
 * @date 28 Jul 2012 21:52:31
 *
 */
/*jshint undef: true*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, undef: true, node: true*/
/*global __dirname*/

function run() {
    "use strict";

    var pvsio                   = require("./pvsprocess"),
        path                    = require("path"),
        ws                      = require("ws"),
        http                    = require("http"),
        fs                      = require("fs"),
        express                 = require("express"),
        webserver               = express(),
        open                    = require("open"),
        util                    = require("util"),
        procWrapper             = require("./processwrapper"),
        FileFilters             = require("./FileFilters"),
        port                    = process.env.PORT || 8082,
        pvsioProcessMap         = {},//each client should get his own process
        httpServer              = http.createServer(webserver),
        Promise                 = require("es6-promise").Promise,
        logger                  = require("tracer").console(),
        serverFuncs				= require("./serverFunctions"),
        baseProjectDir          = path.join(__dirname, "../../examples/projects/"),
        baseDemosDir			= path.join(__dirname, "../../examples/demos/"),
        clientDir				= path.join(__dirname, "../client");
    var clientid = 0, WebSocketServer = ws.Server;
    var fsWatchers = {};
    var writeFile = serverFuncs.writeFile,
        stat = serverFuncs.stat,
        renameFile = serverFuncs.renameFile,
        mkdirRecursive = serverFuncs.mkdirRecursive,
        openProject = serverFuncs.openProject,
        listProjects = serverFuncs.listProjects,
        getFolderTree = serverFuncs.getFolderTree,
        readFile = serverFuncs.readFile;

    /**
     * Utility function that dispatches responses to websocket clients
     * @param {{type:string, data}} token The token to send to the client
     * @param {Socket} socket The websocket to use to send the token
     */
    function processCallback(token, socket) {
        //called when any data is recieved from pvs process
        //if the type of the token is 'processExited' then send message to client if the socket is still open
        token.time  = token.time || {server: {}};
        token.time.server.sent = new Date().getTime();

        //always send relative directories to the client
        if (token.name && token.name.indexOf(baseProjectDir) === 0) {
            token.name = token.name.replace(baseProjectDir, "");
        }
        if (token.path && token.path.indexOf(baseProjectDir) === 0) {
            token.path = token.path.replace(baseProjectDir, "");
        }
        if (token.files) {
            token.files.forEach(function (f) {
                if (f.path && f.path.indexOf(baseProjectDir) === 0) {
                    f.path = f.path.replace(baseProjectDir, "");
                }
            });
        }
        if (token.err) {
            if (token.err.path) {
                token.err.path = token.err.path.replace(new RegExp(baseProjectDir, "g"), "");
            }
            if (token.err.message) {
                token.err.message = token.err.message.replace(new RegExp(baseProjectDir, "g"), "");
            }
        }


        if (socket && socket.readyState === 1) {
            socket.send(JSON.stringify(token));
        }
    }

    /**
     * reads and changes the settings in the .pvsioweb file in the project root
     * @param {string} projectName the name of the project
     * @param {string} key the key of the setting to write
     * @param {object} value the value of the setting to write
     * @returns {Promise} a promise that is resolved when the settings file has been written.
    */
    function changeProjectSetting(projectName, key, value) {
        var file = path.join(baseProjectDir, projectName, "/pvsioweb.json"),
            props = {};
        return new Promise(function (resolve, reject) {
            //if file does not exist, create it. Else read the property file and update just the key value specified
            fs.exists(file, function (exists) {
                if (!exists) {
                    props[key] = value;
                    writeFile(file, JSON.stringify(props, null, " "))
                        .then(resolve, reject);
                } else {
                    fs.readFile(file, {encoding: "utf8"}, function (err, res) {
                        props = {err: err};
                        if (!err) {
                            props =  JSON.parse(res) || props;
                            props[key] = value;
                            //write the file back
                            writeFile(file, JSON.stringify(props, null, " "), "utf8", { overWrite: true })
                                .then(resolve, reject);
                        } else {//there was an error so reject the promise
                            reject(err);
                        }
                    });
                }
            });
        });

    }

    //create logger
    webserver.use("/demos", function (req, res, next) {
        logger.log('Method: %s,  Url: %s, IP: %s', req.method, req.url, req.connection.remoteAddress);
        next();
    });


    //create the express static server serve contents in the client directory and the demos directory
    webserver.use(express.static(clientDir));
    webserver.use("/demos", express.static(baseDemosDir));
    webserver.use("/projects", express.static(baseProjectDir));
    //creating a pathname prefix for client so that demo css and scripts can be loaded from the client dir
    webserver.use("/client", express.static(clientDir));

    function typeCheck(file, cb) {
        if (process.env.PORT) { // this is for the PVSio-web version installed on the heroku cloud
            procWrapper().exec({
                command: "/app/pvs6.0/proveit -T -l -v " + file,
                callBack: cb
            });
        } else if (process.env.pvsdir) {
            procWrapper().exec({
                command: path.join(process.env.pvsdir, "proveit") + " -T -l -v " + file,
                callBack: cb
            });        
        } else {
            procWrapper().exec({
                command: "proveit -T -l -v " + file,
                callBack: cb
            });
        }
    }
    
    function startSapereEE(cb) {
        var cmd = __dirname + "/lib/glassfish4/bin/asadmin restart-domain --force=true";
        procWrapper().exec({
            command: cmd,
            callBack: cb
        });
    }
    
    function stopSapereEE(cb) {
        var cmd = __dirname + "/lib/glassfish4/bin/asadmin stop-domain";
        procWrapper().exec({
            command: cmd,
            callBack: cb
        });
    }
    
    function startIVY(cb) {
        var cmd = "cd " + __dirname + "/ext/IVY" +
                  " && " +
                  "java -Dlog4j.configuration=file:log4j.properties -jar lib/jpf-boot.jar -interactive";
        console.log(cmd);
        // This delayed callback is a workaround to wait for IVY to start up.
        // We can remove this workaround as soon as the IVY tool implements a way to start IVY and return control to the caller.
        function delayedCallback() {
            setTimeout(cb, 2000);
        }
        procWrapper().exec({
            command: cmd,
            callBack: null
        });
        delayedCallback();
    }
    
    /**
        Creates a function that updates the path of the parameter object such that it is relative to the
        basePath specified
    */
    function toRelativePath(basePath) {
        basePath = basePath || baseProjectDir;
        return function (d) {
            if (d.path && d.path.indexOf(basePath) === 0) {
                d.path = d.path.replace(basePath, "");
            }
            return d;
        };
    }
    /**
        utility function for check if a path is absolute
        ///TODO replace with path.isAbsolute once we upgrade node version to 0.12+
    */
    function isAbsolute(thepath) {
        return thepath.trim().indexOf(path.sep) === 0;
    }

    /**
        Reads the contents of a directory.
        @param {string} folderPath
        @returns {Promise} a promise that resolves with a list of objects representing the files in the directory
            each object contains {name: <String>, path: <String>, isDirectory: <boolean>}
    */
    function readDirectory(folderPath) {
        return new Promise(function (resolve, reject) {
            fs.readdir(folderPath, function (err, files) {
                if (!err) {
                    //get stat attributes for all the files using an async call
                    var promises = files.map(function (f) {
                        return new Promise(function (resolve, reject) {
                            fs.stat(path.join(folderPath, f), function (err, res) {
                                if (err) {
                                    reject(err);
                                } else {
                                     var fileStats = {
                                        created: res.birthtime,
                                        modified: res.mtime,
                                        size: res.size,
                                        isDirectory: res.isDirectory()
                                    };
                                    resolve(fileStats);
                                }
                            });
                        });
                    });

                    Promise.all(promises)
                        .then(function (res) {
                            var result = res.map(function (d, i) {
                                return {
                                    name: files[i],
                                    path: path.join(folderPath, files[i]),
                                    isDirectory: d.isDirectory,
                                    stats: d
                                };
                            });
                            resolve(result);
                        }, function (err) {
                            reject(err);
                        });
                } else {
                    reject(err);
                }
            });
        });
    }

    function unregisterFolderWatcher(folderPath) {
        var watcher = fsWatchers[folderPath];
        if (watcher) {
            watcher.close();
            delete fsWatchers[folderPath];
//            logger.debug("unregistered watcher for " + folderPath);
        }
    }

    function unregisterFolderWatchers() {
        Object.keys(fsWatchers).forEach(function (path) {
            unregisterFolderWatcher(path);
        });
        fsWatchers = {};
    }

    /**
        Register a watcher for the specified folder. Sends updates to the client using the socket.
        @param {string} folderPath
        @param {socket} socket
    */
    function registerFolderWatcher(folderPath, socket) {
        // this initial check helps us to prevent the server from crashing
        // when the function is erroneously invoked with wrong arguments
        if (!folderPath || !socket) {
            console.log("(FolderWatcher) Warning: incorrect folderPath or socket (folderPath: " +
                            JSON.stringify(folderPath) + ", socket: " + JSON.stringify(socket) + ")");
            return;
        }
        var notificationDelay = 200;

        unregisterFolderWatcher(folderPath);
        if (folderPath.indexOf("pvsbin") > -1) { return; }

        var watch = function (folder) {
            if (folder.indexOf("pvsbin") > -1) { return; }
//            logger.debug("watching changes to .. " + folder);
            return fs.watch(folder, {persistent: false}, function (event, name) {
                var extension = path.extname(name).toLowerCase();
                if (name && name !== ".DS_Store" && (event === "rename" || event === "change")) {
                    var fullPath = path.join(folder, name);
                    var token = {
                        type: "FileSystemUpdate",
                        name: name,
                        path: fullPath,
                        event: event,
                        time: {server: {}}
                    };
                    stat(fullPath)
                        .then(function (res) {
                            if (res.isDirectory()) {
                                registerFolderWatcher(fullPath, socket);
                                token.isDirectory = true;
                            }
                            if (token.isDirectory || FileFilters.indexOf(extension) > -1) {
                                setTimeout(function () {
                                    if (token.isDirectory) {
                                        getFolderTree(fullPath).then(function (data) {
                                            token.subFiles = data.filter(function (f) {
                                                return (!f.isDirectory) &&
                                                    FileFilters.indexOf(path.extname(f.path).toLowerCase()) > -1;
                                            }).map(toRelativePath(fullPath));
                                            token.subFolders = data.filter(function (f) {
                                                return f.isDirectory;
                                            }).map(toRelativePath(fullPath));
                                            processCallback(token, socket);
                                        }).catch(function (err) {
                                            token.err = err;
                                            processCallback(token, socket);
                                        });
                                    } else {
                                        processCallback(token, socket);
                                    }
                                }, notificationDelay);
                            }
                        }).catch(function (err) {
                            token.event = (err.code === "ENOENT") ? "delete" : event;
                            if (token.event === "delete") {
                                setTimeout(function () {
                                    processCallback(token, socket);
                                }, notificationDelay);
                                unregisterFolderWatcher(fullPath);
                            }
                        });
                }
            });
        };
        try {
            fsWatchers[folderPath] = watch(folderPath);
            // watch the sub-directories too
            getFolderTree(folderPath).then(function (data) {
                var subFolders = data.filter(function (f) {
                    return f.isDirectory;
                });
                // watch all subfolders
                subFolders.forEach(function (subFolder) {
                    if (!fsWatchers[subFolder.path]) {
                        fsWatchers[subFolder.path] = watch(subFolder.path);
                    }
                });
            });
        } catch (err) {
            logger.error(err);
        }
    }

    function readFolderContent(token, socket) {
        return new Promise(function (resolve, reject) {
            var res = {
                type: "FileSystemUpdate",
                event: "refresh",
                name: token.path,
                path: token.path,
                isDirectory: true,
                time: {server: {}}
            };
            getFolderTree(token.path).then(function (data) {
                res.subFiles = data.filter(function (f) {
                    return (!f.isDirectory) &&
                        FileFilters.indexOf(path.extname(f.path).toLowerCase()) > -1;
                }).map(toRelativePath(token.path));
                res.subFolders = data.filter(function (f) {
                    return f.isDirectory;
                }).map(toRelativePath(token.path));
                processCallback(res, socket);
                resolve(token.path);
            }).catch(function (err) {
                token.err = {
                    code: err.code,
                    message: err.message,
                    path: err.path
                };
                token.type += "_error";
                processCallback(token, socket);
                reject(err);
            });
        });
    }


    /**
        get function maps for client sockets
    */
    function createClientFunctionMaps() {
        var initProcessMap = function (socketid) {
            pvsioProcessMap[socketid] = pvsioProcessMap[socketid] || pvsio();
//            logger.debug(socketid);
        };
        var map = {
            "keepAlive": function (token, socket, socketid) {
                // do nothing
                // console.log("Receiving keepAlive message...");
            },
            "setMainFile": function (token, socket, socketid) {
                initProcessMap(socketid);
                var mainFile = token.path.split("/").slice(1).join("/");
                if (mainFile !== "") {
                    changeProjectSetting(token.projectName, "mainPVSFile", mainFile)
                        .then(function (res) {
                            res.type = token.type;
                            res.id = token.id;
                            res.socketId = socketid;
                            res.time = token.time;
                            processCallback(res, socket);
                        });
                } else {
                    res.type = res.type + "_error";
                    res.err = {
                        message: "Invalid token " + JSON.stringify(token),
                        code: "ENOENT",
                        path: token.path
                    };
                    processCallback(res, socket);
                }
            },
            "changeProjectSetting": function (token, socket, socketid) {
                initProcessMap(socketid);
                if (token.key && token.value) {
                    changeProjectSetting(token.projectName, token.key, token.value)
                        .then(function (res) {
                            res.type = token.type;
                            res.id = token.id;
                            res.socketId = socketid;
                            res.time = token.time;
                            processCallback(res, socket);
                        });
                } else {
                    var res = {
                        type: token.type + "_error",
                        id: token.id,
                        socketId: socketid,
                        time: token.time
                    };
                    res.err = {
                        message: "Invalid token " + JSON.stringify(token),
                        code: "ENOENT",
                        path: token.projectName
                    };
                    processCallback(res, socket);
                }
            },
            "listProjects": function (token, socket, socketid) {
                initProcessMap(socketid);
                var result = {
                    type: token.type,
                    id: token.id,
                    socketId: socketid,
                    time: token.time
                };
                listProjects()
                    .then(function (projects) {
                        result.projects = projects;
                        processCallback(result, socket);
                    }).catch(function (err) {
                        result.type = token.type + "_error";
                        result.err = err;
                        processCallback(result, socket);
                    });
            },
            "openProject": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                openProject(token.name)
                    .then(function (data) {
                        res.project = data;
                        unregisterFolderWatchers();
                        registerFolderWatcher(path.join(baseProjectDir, token.name), socket);
                        processCallback(res, socket);
                    }).catch(function (err) {
                        res.type = token.type + "_error";
                        res.err = err;
                        processCallback(res, socket);
                    });
            },
            "typeCheck": function (token, socket, socketid) {
                initProcessMap(socketid);
                typeCheck(path.join(baseProjectDir, token.path), function (err, stdout, stderr) {
                    var res = {
                        id: token.id,
                        type: token.type,
                        err: err,
                        stdout: stdout,
                        stderr: stderr,
                        socketId: socketid,
                        time: token.time
                    };
                    processCallback(res, socket);
                });
            },
            "sendCommand": function (token, socket, socketid) {
                initProcessMap(socketid);
                pvsioProcessMap[socketid].sendCommand(token.data.command, function (data) {
                    var res = {
                        id: token.id,
                        data: [data],
                        socketId: socketid,
                        type: "commandResult",
                        time: token.time
                    };
                    processCallback(res, socket);
                });
            },
            "startProcess": function (token, socket, socketid) {
                initProcessMap(socketid);
                logger.info("Calling start process for client... " + socketid);
                var root = token.data.projectName ?
                            path.join(baseProjectDir, token.data.projectName)
                            : token.data.demoName ? path.join(baseDemosDir, token.data.demoName) : "";
                //close the process if it exists and recreate it
                if (pvsioProcessMap[socketid]) {
                    pvsioProcessMap[socketid].close('SIGTERM', true);
                    delete pvsioProcessMap[socketid];
                }
                //recreate the pvsio process
                pvsioProcessMap[socketid] = pvsio();
                //set the workspace dir and start the pvs process with a callback for processing process ready and exit
                //messages from the process
                pvsioProcessMap[socketid].workspaceDir(root)
                    .start(token.data.name,
                        function (res) {
                            res.id = token.id;
                            res.type = token.type;
                            res.socketId = socketid;
                            res.time = token.time;
                            processCallback(res, socket);
                        });
            },
            "closeProcess": function (token, socket, socketid) {//closes pvs process
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                if (pvsioProcessMap[socketid]) {
                    pvsioProcessMap[socketid].close();
                    delete pvsioProcessMap[socketid];
                    res.message = "process closed";
                } else {
                    res.type = token.type + "_error";
                    res.type = "attempting to close undefined process";
                }
                unregisterFolderWatchers();
                processCallback(res, socket);

            },
            "readFile": function (token, socket, socketid) {
                initProcessMap(socketid);
                var encoding = token.encoding || "utf8";
                token.path = isAbsolute(token.path) ? token.path : path.join(baseProjectDir, token.path);
                readFile(token.path, encoding)
                    .then(function (content) {
                        var res = {
                            id: token.id,
                            type: token.type,
                            encoding: encoding,
                            content: content,
                            name: token.name,
                            path: token.path,
                            socketId: socketid,
                            time: token.time
                        };
                        processCallback(res, socket);
                    }).catch(function (err) {
                        var tokenErr = {
                            type: token.type + "_error",
                            id: token.id,
                            socketId: socketid,
                            name: token.name,
                            path: token.path,
                            err: {
                                code: err.code,
                                message: err.message,
                                path: err.path
                            },
                            time: token.time
                        };
                        processCallback(tokenErr, socket);
                    });
            },
            "writeFile": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time,
                    name: token.name,
                    path: token.path
                };
                token.path = isAbsolute(token.path) ? token.path : path.join(baseProjectDir, token.path);
                writeFile(token.path, token.content, token.encoding, token.opt)
                    .then(function () {
                        processCallback(res, socket);
                    }, function (err) {
                        res.type = token.type + "_error";
                        res.err = {
                            code: err.code,
                            message: err.message,
                            path: err.path
                        };
                        processCallback(res, socket);
                    });
            },
            "deleteFile": function (token, socket, socketid) {
                initProcessMap(socketid);
                token.path = path.join(baseProjectDir, token.path);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time,
                    name: token.name,
                    path: token.path
                };
                try {
                    pvsioProcessMap[socketid].removeFile(token.path, function (err) {
                        if (!err) {
                            res.type = token.type;
                        } else {
                            res.type = token.type + "_error";
                            res.err = {
                                code: err.code,
                                message: err.message,
                                path: err.path
                            };
                        }
                        processCallback(res, socket);
                    });
                } catch (err) {
                    res.type = token.type + "_error";
                    res.err = err.message;
                    processCallback(res, socket);
                }
            },
            "renameFile": function (token, socket, socketid) {
                initProcessMap(socketid);
                renameFile(token.oldPath, token.newPath).then(function (res) {
                    processCallback({
                        type: token.type,
                        id: token.id,
                        socketId: socketid,
                        time: token.time
                    }, socket);
                }).catch(function (err) {
                    var msg = "Error while renaming " + token.oldPath + " into " + token.newPath;
                    if (err === "ENOTEMPTY") {
                        msg += " (file with same name already exists)";
                    } else { msg += " (" + err + ")"; }
                    logger.warn(msg);
                    processCallback({
                        type: token.type + "_error",
                        id: token.id,
                        socketId: socketid,
                        time: token.time,
                        err: {
                            oldPath: token.oldPath,
                            newPath: token.newPath,
                            message: err.message,
                            code: err.code,
                            path: err.path
                        }
                    }, socket);
                });
            },
            "fileExists": function (token, socket, socketid) {
                initProcessMap(socketid);
                token.path = path.join(baseProjectDir, token.path);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time,
                    name: token.name,
                    path: token.path
                };
                fs.exists(token.path, function (exists) {
                    res.exists = exists;
                    processCallback(res, socket);
                });
            },
            "readDirectory": function (token, socket, socketid) {
                initProcessMap(socketid);
                var absPath = token.path.indexOf("~") === 0 ? path.join(process.env.HOME, token.path.substr(1))
                    : isAbsolute(token.path) ? token.path : path.join(baseProjectDir, token.path);
                readDirectory(absPath)
                    .then(function (files) {
                        processCallback({
                            id: token.id,
                            type: token.type,
                            socketId: socketid,
                            files: files,
                            time: token.time
                        }, socket);
                    }).catch(function (err) {
                        processCallback({
                            type: token.type + "_error",
                            id: token.id,
                            socketId: socketid,
                            err: err.toString(),
                            time: token.time
                        }, socket);
                    });
            },
            "writeDirectory": function (token, socket, socketid) {
                initProcessMap(socketid);
                if (token && token.path) {
                    token.path = path.join(baseProjectDir, token.path);
                    stat(token.path).then(
                        function (res) {
                            //directory exists: refresh content and send message EEXIST to client
                            // Note: we refresh the content because this is an existing folder
                            // that is visible to the client (the client might not have info about it)
                            unregisterFolderWatcher(token.path, socket);
                            readFolderContent(token, socket).then(function (res) {
                                registerFolderWatcher(token.path, socket);
                            }).catch(function (err) {
                                console.log(err);
                            });
                            processCallback({
                                id: token.id,
                                type: token.type + "_error",
                                socketId: socketid,
                                time: token.time,
                                err: {
                                    path: token.path,
                                    message: "Directory Exists",
                                    code: "EEXIST"
                                }
                            }, socket);
                        },
                        function (err) {
                            if (err.code === "ENOENT") {
                                // create the directory
                                mkdirRecursive(token.path, function (err) {
                                    if (!err) {
                                        // and watch its content
                                        var callBackToken = {
                                            id: token.id,
                                            type: token.type,
                                            path: token.path,
                                            socketId: socketid,
                                            err: err,
                                            time: token.time
                                        };
                                        registerFolderWatcher(callBackToken.path, socket);
                                        processCallback(callBackToken, socket);
                                    } else {
                                        processCallback({
                                            id: token.id,
                                            type: token.type + "_error",
                                            socketId: socketid,
                                            time: token.time,
                                            err: {
                                                path: err.path,
                                                message: err.message,
                                                code: err.code
                                            }
                                        }, socket);
                                    }
                                });
                            }
                        }
                    ).catch(function (err) {
                        processCallback({
                            id: token.id,
                            type: token.type + "_error",
                            socketId: socketid,
                            time: token.time,
                            err: {
                                path: err.path,
                                message: err.message,
                                code: err.code
                            }
                        }, socket);
                    });
                } else {
                    processCallback({
                        type: token.type + "_error",
                        id: token.id,
                        socketId: socketid,
                        err: {
                            message: "Write directory error: property 'path' is undefined.",
                            code: "ENOENT",
                            path: token.path
                        },
                        time: token.time
                    }, socket);
                }

            },
            "deleteDirectory": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                try {
                    token.path = path.join(baseProjectDir, token.path);
                    res.path = token.path;
                    pvsioProcessMap[socketid].removeFile(token.path, function (err) {
                        if (!err) {
                            res.type = token.type;
                        } else {
                            res.type = token.type + "_error";
                            res.err = {
                                code: err.code,
                                message: err.message,
                                path: err.path
                            };
                        }
                        processCallback(res, socket);
                    });
                } catch (err) {
                    res.type = token.type + "_error";
                    res.err = err.message;
                    processCallback(res, socket);
                }
            },
            "renameProject": function (token, socket, socketid) {
                initProcessMap(socketid);
                var oldProjectPath = path.join(baseProjectDir, token.oldPath);
                var newProjectPath = path.join(baseProjectDir, token.newPath);
                unregisterFolderWatcher(oldProjectPath, socket);
                renameFile(token.oldPath, token.newPath).then(function (res) {
                    registerFolderWatcher(newProjectPath, socket);
                    processCallback({
                        type: token.type,
                        id: token.id,
                        socketId: socketid,
                        time: token.time
                    }, socket);
                }).catch(function (err) {
                    registerFolderWatcher(oldProjectPath, socket);
                    var msg = "Error while renaming " + token.oldPath + " into " + token.newPath;
                    if (err === "ENOTEMPTY") {
                        msg += " (directory with same name already exists)";
                    } else { msg += " (" + err + ")"; }
                    logger.warn(msg);
                    processCallback({
                        type: token.type + "_error",
                        id: token.id,
                        socketId: socketid,
                        time: token.time,
                        err: {
                            oldPath: token.oldPath,
                            newPath: token.newPath,
                            message: err.message,
                            code: err.code,
                            path: err.path
                        }
                    }, socket);
                });
            },
            "startSapereEE": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                try {
                    startSapereEE(function (err, stdout, stderr) {
                        res.stdout = stdout;
                        res.stderr = stderr;
                        console.log("glassfish err:" + err);
                        console.log("glassfish stdout:" + stdout);
                        console.log("glassfish stderr:" + stderr);
                        processCallback(res, socket);
                    });
                } catch (err) {
                    if (err.code === 1 && err.killed === false) {
                        // glassfish is already running, it's not an error
                        res.stdout = "PVSio-web Network Controller already started.";
                    } else {                    
                        res.type = token.type + "_error";
                        res.err = err.message;
                    }
                    processCallback(res, socket);
                }
            },
            "startIVY": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                try {
                    startIVY(function (err, stdout, stderr) {
                        res.stdout = stdout;
                        res.stderr = stderr;
                        console.log("IVY err:" + err);
                        console.log("IVY stdout:" + stdout);
                        console.log("IVY stderr:" + stderr);
                        processCallback(res, socket);
                    });
                } catch (err) {
                    res.type = token.type + "_error";
                    res.err = err.message;
                    processCallback(res, socket);
                }
            },
            "stopSapereEE": function (token, socket, socketid) {
                initProcessMap(socketid);
                var res = {
                    id: token.id,
                    type: token.type,
                    socketId: socketid,
                    time: token.time
                };
                try {
                    stopSapereEE(function (err, stdout, stderr) {
                        res = {
                            id: token.id,
                            type: token.type,
                            err: err,
                            stdout: stdout,
                            stderr: stderr,
                            socketId: socketid,
                            time: token.time
                        };
                        processCallback(res, socket);
                    });
                } catch (err) {
                    res.type = token.type + "_error";
                    res.err = err.message;
                    processCallback(res, socket);
                }
            }
        };

        return map;
    }

    var wsServer = new WebSocketServer({server: httpServer});
    wsServer.on("connection", function (socket) {
        var socketid =  clientid++;
        var functionMaps = createClientFunctionMaps();
        logger.info("opening websocket client " + socketid);
        socket.on("message", function (m) {
            try {
                var token = JSON.parse(m);
                token.time.server = {received: new Date().getTime()};
                var f = functionMaps[token.type];
                if (f && typeof f === 'function') {
                    //call the function with token and socket as parameter
                    f(token, socket, socketid);
                } else {
                    logger.warn("f is something unexpected -- I expected a function but got type " + typeof f);
                }
            } catch (error) {
                logger.error(error.message);
                logger.warn("Unexpected error while processing token " + JSON.stringify(m));
            }
        });

        socket.on("close", function () {
            logger.info("closing websocket client " + socketid);
            var _p = pvsioProcessMap[socketid];
            if (_p) {
                _p.close();
            }
            delete pvsioProcessMap[socketid];
        });
    });

    wsServer.on("error", function (err) {
        if (err.code === "EADDRINUSE") {
            console.log("\nError: Another instance of the PVSio-web server is already running.\nPlease shut down the other PVSio-web server instance before starting a new instance.");
        } else {
            logger.error(JSON.stringify(err));
        }
        console.log("----------------------------------------------");
    });

    httpServer.listen(port, function () {
        console.log("PVSio-web server ready!");
        console.log("----------------------------------------------");
        var restart = false;
        if (process.argv) {
            process.argv.forEach(function (val, index, array) {
                if (val.toLowerCase() === "restart") {
                    restart = true;
                }
                if (val.toLowerCase().indexOf("pvsdir:") === 0) {
                    process.env.pvsdir = path.join(__dirname, "../../" + val.toLowerCase().split(":")[1]);
					console.log("PVS directory is " + process.env.pvsdir);
                }
            });
        }
        if (!restart) {
            open(util.format("http://localhost:%s", port.toString()));
        }
    });
    logger.info(""); // this is used to print date and time in the console
    console.log("----------------------------------------------\nStarting up PVSio-web server on port " + port + "...");
}
run();
