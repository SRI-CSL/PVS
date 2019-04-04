/**
 * @module FileReader
 * @author Patrick Oladimeji, Paolo Masci
 * @date 25/06/2015 10:48:39 AM
 *
 * This module provides api access to interact with the filesystem. This includes
 * reading, writing and deleting files, as well as creating and removing directories.
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
define(function (require, exports, module) {

    var eventDispatcher       = require("util/eventDispatcher"),
        FileHandler           = require("filesystem/FileHandler");
    var PreferenceStorage     = require("preferences/PreferenceStorage").getInstance(),
        PreferenceKeys        = require("preferences/PreferenceKeys"),
        RemoteFileBrowser     = require("./RemoteFileBrowser"),
        Logger                = require("util/Logger"),
        openFilesForm         = require("pvsioweb/forms/openFiles"),
        displayQuestion       = require("pvsioweb/forms/displayQuestion"),
        MIME                  = require("util/MIME"),
        NotificationManager   = require("project/NotificationManager"),
        Descriptor            = require("project/Descriptor"),
        WSManager             = require("websockets/pvs/WSManager");

    var instance;

    function FileSystem() {
        eventDispatcher(this);
        return this;
    }

    /**
     * @function <hr><a name="writeFile">writeFile</a>
     * @description Writes a file to disk.
     * @param path {!String} The path of the file that shall be written.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     *              This parameter is mandatory: it must be a valid non-null String.
     * @param content {?String} The content of the file. If null, the empty String "" will be used as file content.
     * @param opt {?Object} Options
     *        <li>overWrite {bool} Enables file overwrite. If overWrite is false and a file with the same name
     *            exists, then the function returns an error code "EEXIST". Default: false.</li>
     *        <li>encoding {String} Defines the file encoding: "utf8" for text files; "base64" for images. Default: "utf8"</li>
     *        <li>silentMode {bool} Disables showing notifications up front within the PVSio-web user interface. Default: false.</li>
     * @returns {Promise(Descriptor)} A Promise that resolves to the descriptor of the file added.
     *           The main properties of file descriptors are (see <a href="#!">Descriptor</a>):
     *           <li> path {String} The file path. </li>
     *           <li> content {String} The content of the file.</li>
     *           <li> encoding {String} The file encoding.
     *                Text files have encoding "utf8". Image files have encoding "base64".</li>
     * @memberof module:FileSystem
     * @instance
     *
     *
     * @example <caption>Example 1: Using writeFile to add file main.pvs into directory test.</caption>
     * // fs is the FileSystem instance
     * fs.writeFile("test/main.pvs", "%-- this is a text file", { overWrite: false }).then(function (res) {
     *     //file successfully written to disk, do something...
     *     var path = res.path;
     *     var content = res.content;
     *     var encoding = res.encoding;
     *     //...
     * }).catch(function (err) {
     *     //file could not be added...
     *     console.log(err);
     * });
     *
     */
    FileSystem.prototype.writeFile = function (path, content, opt) {
        return new Promise(function (resolve, reject) {
            if (!path) {
                reject({
                    code: "INVALID_ARG",
                    message: "Invalid path " + path,
                    path: path
                });
            } else {
                let token = {
                    path: path,
                    name: path.split("/").slice(-1).join(""),
                    content: content || "",
                    encoding: (opt) ? (opt.encoding || "utf8") : "utf8",
                    opt: opt
                };
                WSManager.getWebSocket().writeFile(token, function (err, res) {
                    if (err || res.path !== token.path) { return reject(err); }
                    let notification = "File " + res.path + " correctly written to disk";
                    if (opt && opt.silentMode) {
                        Logger.log(notification);
                    } else {
                        NotificationManager.show(notification);
                        this.fire({ type: "filewritten", path: path });
                        //pvsFilesListView.selectItem(path);
                    }
                    return resolve(new Descriptor(token.path, token.content, { encoding: token.encoding }));
                });
            }
        });
    };

    /**
     * @function <hr><a name="writeFileDialog">writeFileDialog</a>
     * @description Writes a file to disk. This function is a variant of <a href="writeFile">writeFile</a>
     *              designed to show a confirmation dialog before overwriting files.
     * @param path {!String} The path of the file that shall be written.
     *              The provided file path will be used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     *              This parameter is mandatory: it must be a valid non-null String.
     * @param content {?String} The content of the file. If null, the empty String "" will be used as file content.
     * @param opt {?Object} Options
     *        <li>overWrite {bool} Enables file overwrite. This option is handled by the function
     *            so that directories are not overwritten without explicit confirmation form the user.</li>
     *        <li>encoding {String} Defines the file encoding: "utf8" for text files; "base64" for images. Default: "utf8"</li>
     *        <li>silentMode {bool} Disables up front notifications in the PVSio-web user interface. Default: false.</li>
     * @returns {Promise(Descriptor)} A Promise that resolves to the descriptor of the file added.
     *           The main properties of file descriptors are (see <a href="#!">Descriptor</a>):
     *           <li> path {String} The file path. </li>
     *           <li> content {String} The content of the file.</li>
     *           <li> encoding {String} The file encoding.
     *                Text files have encoding "utf8". Image files have encoding "base64".</li>
     * @memberof module:FileSystem
     * @instance
     *
     */
    FileSystem.prototype.writeFileDialog = function (path, content, opt) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            if (!path) {
                reject({
                    code: "INVALID_ARG",
                    message: "Invalid path " + path,
                    path: path
                });
            } else {
                opt = opt || {};
                opt.overWrite = false;
                fs.writeFile(path, content, opt).then(function (res) {
                    resolve(res);
                }).catch(function (err) {
                    if (err.code === "EEXIST") {
                        var data = {header: "Confirm Dialog",
                                    question: "File " + path + " already exists. Overwrite file?",
                                   buttons: ["Cancel", "OK"]};
                        displayQuestion.create(data)
                            .on("ok", function (e, view) {
                                opt.overWrite = true;
                                fs.writeFile(path, content, opt).then(function (res) {
                                    resolve(res);
                                }).catch(function (err) { console.log(err); reject(err); });
                                view.remove();
                            }).on("cancel", function (e, view) {
                                reject({
                                    code: "CANCELED_BY_USER",
                                    message: "Operation cancelled by the user",
                                    path: path
                                });
                                view.remove();
                            });
                    }
                });
            }
        });
    };

    /**
     * @function <hr><a name="readFile">readFile</a>
     * @description Reads the content of a file from disk.
     * @param path {!String} The path of the file that shall be read.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     * @returns {Promise(Descriptor)} A Promise that resolves to a file descriptor.
     * @memberof module:FileSystem
     * @instance
     *
     * @example <caption>Using readFile to read the content of file main.pvs stored in directory test.</caption>
     * // fs is the FileSystem instance
     * fs.readFile("test/main.pvs").then(function (res) {
     *     // res.content is the file content
     *     var content = res.content;
     *     //...
     * }).catch(function (err) {
     *     //file could not be read...
     *     console.log(err);
     * });
     *
     */
    FileSystem.prototype.readFile = function (path, opt) {
        return new Promise(function (resolve, reject) {
            if (!path) {
                reject({
                    code: "INVALID_ARG",
                    message: "Invalid path " + path,
                    path: path
                });
            } else {
                opt = opt || {};
                opt.encoding = opt.encoding || "utf8";
                var token = { path: path, encoding: opt.encoding };
                WSManager.getWebSocket().readFile(token, function (err, res) {
                    if (!err) {
                        var descriptor = new Descriptor(res.path, res.content, { encoding: opt.encoding });
                        resolve(descriptor);
                    } else {
                        console.log(err);
                        reject(err);
                    }
                });
            }
        });
    };

    /**
     * @function <hr><a name="readFileDialog">readFileDialog</a>
     * @description Reads the content of a file from disk. This function is a variant of
     *              <a href="readFile">readFile</a> designed to show a file browser
     *              for selecting the file.
     * @returns {Promise(Descriptor)} A Promise that resolves to a file descriptor.
     * @memberof module:FileSystem
     * @instance
     *
     * @example <caption>Using readFileDialog to open the file browser.</caption>
     * // fs is the FileSystem instance
     * fs.readFileDialog().then(function (res) {
     *     // res.content is the file content
     *     var content = res.content;
     *     //...
     * }).catch(function (err) {
     *     //file could not be read...
     *     console.log(err);
     * });
     *
     */
    FileSystem.prototype.readFileDialog = function (opt) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            opt = opt || {};
            opt.path = opt.path || PreferenceStorage.get(PreferenceKeys.LAST_DIRECTORY_VISITED);
            opt.filter = opt.filter ||
                ((opt.encoding === "base64") ? MIME.getInstance().imageFilter : MIME.getInstance().modelFilter);
            new RemoteFileBrowser(opt.filter)
                .open(opt.path, { title: opt.title || "Select files (use shift key to select multiple files)" })
                .then(function (files) {
                    if (files) {
                        var paths = files.map(function (f) {
                            return f.path;
                        });
                        var promises = [];
                        paths.forEach(function (path) {
                            promises.push(fs.readFile(path, opt));
                        });
                        return Promise.all(promises).then(function (res) {
                            resolve(res);
                        });
                    }
                    Promise.resolve();
                }).catch(function (err) {
                    reject(err);
                });
        });
    };

    /**
     * @function <hr><a name="readLocalFile">readLocalFile</a>
     * @description Reads the content of a local file stored on the client.
     * @param fileList {!FileList} The list of local files that shall be read.
     * @returns {Promise(Array(Descriptor))} A Promise that resolves to an Array of file descriptors.
     * @memberof module:FileSystem
     * @instance
     *
     * @example <caption>Using readFile to read the content of file main.pvs stored in directory test.</caption>
     * // fs is the FileSystem instance
     * fs.readFile("test/main.pvs").then(function (res) {
     *     // res.content is the file content
     *     var content = res.content;
     *     //...
     * }).catch(function (err) {
     *     //file could not be read...
     *     console.log(err);
     * });
     *
     */
    FileSystem.prototype.readLocalFile = function (fileList) {
        return new Promise(function (resolve, reject) {
            if (!fileList) {
                reject({
                    code: "INVALID_ARG",
                    message: "Invalid argument " + fileList,
                    fileList: fileList
                });
            } else {
                var promises = [], i = 0;
                for (i = 0; i < fileList.length; i++) {
                    promises.push(FileHandler.readLocalFile(fileList[i]));
                }
                return Promise.all(promises).then(function (res) {
                    var descriptors = [];
                    res.forEach(function (file) {
                        descriptors.push(new Descriptor(file.name, file.content, { encoding: file.encoding }));
                    });
                    resolve(descriptors);
                }).catch(function (err) { reject(err); });
            }
        });
    };
/**
     * @function openLocalFilesDialog
     * @description Opens a dialog that allows users to select local files that shall be imported in the project.
     * @returns {Promise(FileList)} a Promise that resolves a FileList object with the files selected by the user.
     *           Each file in the FileList has the following properties:
     *           <li>name (String): the name of the file</li>
     *           <li>type (String): the MIME type of the file. For images, the MIME type starts with 'image/'</li>
     * @memberof module:FileSystem
     * @instance
     */
    FileSystem.prototype.readLocalFileDialog = function (opt) {
        var fs = this;
        opt = opt || {};
        opt.extensions = opt.extensions ||
            ((opt.encoding === "base64") ? MIME.getInstance().getImageExts() : MIME.getInstance().getModelExts());
        return new Promise(function (resolve, reject) {
            openFilesForm.create(opt).on("cancel", function (e, view) {
                view.remove();
                reject({
                    code: "CANCELED_BY_USER",
                    message: "Operation cancelled by the user",
                    fileList: null
                });
            }).on("ok", function (e, view) {
                view.remove();
                fs.readLocalFile(e.data.projectFiles)
                    .then(function (res) { resolve(res); })
                    .catch(function (err) { reject(err); });
            });
        });
    };


    /**
     * @function <hr><a name="deleteFile">deleteFile</a>
     * @description Deletes a file from a project folder.
     * @param path {!String} The path of the file that shall be deleted.
     *              The provided path must start with the name of the project folder (e.g., "AlarisGP/alaris.pvs").
     * @returns {Promise(String)} A Promise that resolves to the name of the deleted file.
     * @memberof module:FileSystem
     * @instance
     *
     * @example <caption>Using deleteFile to delete file test/main.pvs</caption>
     * // fs is the FileSystem instance
     * fs.deleteFile("test/main.pvs").then(function (res) {
     *     // res is the name of the deleted file
     *     var path = res;
     *     //...
     * }).catch(function (err) {
     *     //file could not be deleted...
     *     console.log(err);
     * });
     *
     */
    FileSystem.prototype.deleteFile = function (name) {
        return new Promise(function (resolve, reject) {
            if (!name) {
                reject({ type: "INVALID_ARG", msg: "Incorrect file name " + name});
            } else {
                var token = {
                    name: name.split("/").slice(-1).join(""),
                    path: name
                };
                WSManager.getWebSocket().deleteFile(token, function (err) {
                    if (!err) {
                        // note: file system watcher will take care of populating file tree view
                        var notification = "File " + name + " successfully deleted.";
                        NotificationManager.show(notification);
                        // f.clearListeners(); --TODO: check if this is actually needed
                        resolve(name);
                    } else {
                        // reject with error
                        reject(err);
                        NotificationManager.error(err);
                    }
                });
            }
        });
    };

    /**
     * @function <hr><a name="deleteFileDialog">deleteFileDialog</a>
     * @description Deletes a file from the projects folder. This function is a variant of <a href="#deleteFile">deleteFile</a>
     *              designed to show a confirmation dialog before deleting files.
     * @param path {!String} The path of the file that shall be deleted.
     *              The provided path must start with the name of the project folder (e.g., "AlarisGP/alaris.pvs").
     * @returns {Promise(String)} A Promise that resolves to the name of the deleted file.
     * @memberof module:FileSystem
     * @instance
     *
     */
    FileSystem.prototype.deleteFileDialog = function (path) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            displayQuestion.create({question: "Delete file " + path + "?"})
                .on("ok", function (e, view) {
                    fs.deleteFile(path).then(function (res) {
                        resolve(res);
                    }).catch(function (err) { reject(err); });
                    view.remove();
                }).on("cancel", function (e, view) {
                    reject({
                        code: "CANCELED_BY_USER",
                        message: "Operation cancelled by the user",
                        path: path
                    });
                    view.remove();
                });
        });
    };

    /**
     * @function <hr><a name="rmDir">rmDir</a>
     * @description Deletes a directory.
     * @param path {!String} Path of the folder that shall be deleted.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     * @return {Promise} A Promise that resolves to the name of the deleted folder.
     * @memberof module:FileSystem
     * @instance
     */
    FileSystem.prototype.rmDir = function (path) {
        return new Promise(function (resolve, reject) {
            if (path) {
                WSManager.getWebSocket().deleteDirectory(path, function (err) {
                    if (!err || err.code === "ENOENT") {
                        var notification = "Folder " + path + " removed from project.";
                        NotificationManager.show(notification);
                        resolve(path);
                    } else {
                        //show error
                        NotificationManager.error(err);
                        reject(err);
                    }
                });
            } else {
                reject({
                    code: "INVALID_PATH",
                    message: "Invalid path " + JSON.stringify(path),
                    path: path
                });
            }
        });
    };

    /**
     * @function <hr><a name="rmDirDialog">rmDirDialog</a>
     * @description Deletes a directory. This is a variant of <a href="#rmDir">rmDir</a> designed
     *              to show a dialog to ask confirmation before deleting directories.
     * @param path {!String} Path of the folder that shall be deleted.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     * @return {Promise} A Promise that resolves to the name of the deleted folder.
     * @memberof module:FileSystem
     * @instance
     */
    FileSystem.prototype.rmDirDialog = function (path) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            if (path) {
                displayQuestion.create({question: "Delete directory " + path + "?"})
                    .on("ok", function (e, view) {
                        fs.rmDir(path).then(function (res) {
                            resolve(res);
                        }).catch(function (err) { reject(err); });
                        view.remove();
                    }).on("cancel", function (e, view) {
                        reject({
                            code: "CANCELED_BY_USER",
                            message: "Operation cancelled by the user",
                            path: path
                        });
                        view.remove();
                    });
            } else {
                reject({
                    code: "INVALID_PATH",
                    message: "Invalid path " + JSON.stringify(path),
                    path: path
                });
            }
        });
    };

    /**
     * @function <hr><a name="mkDir">mkDir</a>
     * @description Creates a directory.
     * @param path {!String} Path of the folder that shall be created.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     * @param opt {?Object} Options
     *        <li>overWrite {bool} Enables overwriting of the directory. Default: false.</li>
     * @return {Promise(Descriptor)} A Promise that resolves to a folder descriptor.
     * @memberof module:FileSystem
     * @instance
     */
    FileSystem.prototype.mkDir = function (path, opt) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            function mkDir_aux(path, opt) {
                WSManager.getWebSocket().writeDirectory(path, function (err, res) {
                    if (err) {
                        var msg = "Folder " + err.path;
                        if (err.code === "EEXIST") {
                            msg += " already exists.";
                        } else {
                            msg += " could not be created" + JSON.stringify(err);
                        }
                        if (!opt || !opt.silentMode) {
                            NotificationManager.error(msg);
                        } else { Logger.log(msg); }
                        if (opt.overWrite && opt.overWrite === true) {
                            resolve(new Descriptor(path, null, { isDirectory: true }));
                        } else {
                            reject(err);
                        }
                    } else {
                        var notification = "Directory " + res.path + " successfully created";
                        if (!opt || !opt.silentMode) {
                            NotificationManager.show(notification);
                        } else { Logger.log(notification); }
                        resolve(new Descriptor(res.path, null, { isDirectory: true }));
                    }
                });
            }
            if (path) {
                opt = opt || {};
                if (opt.overWrite) {
                    fs.rmDir(path).then(function (res) {
                        mkDir_aux(path, opt);
                    }).catch(function (err) { reject(err); });
                } else {
                    mkDir_aux(path, opt);
                }
            } else {
                reject({
                    code: "INVALID_PATH",
                    message: "Invalid path " + JSON.stringify(path),
                    path: path
                });
            }
        });
    };

    /**
     * @function <hr><a name="mkDirDialog">mkDirDialog</a>
     * @description Creates a directory. This is a variant of <a href="#mkDir">mkDir</a> designed
     *              to show a dialog to ask confirmation before overwriting directories.
     * @param path {!String} Path of the folder that shall be created.
     *              The provided file path is used as a relative path from the project folder
     *              of the PVSio-web installation (i.e., pvsio-web/examples/projects/).
     * @param opt {?Object} Options
     *        <li>overWrite {bool} Enables overwrite. This option is handled by the function
     *            so that directories are not overwritten without explicit confirmation form the user.</li>
     * @return {Promise(Descriptor)} A Promise that resolves to a folder descriptor.
     * @memberof module:FileSystem
     * @instance
     */
    FileSystem.prototype.mkDirDialog = function (path, opt) {
        var fs = this;
        return new Promise(function (resolve, reject) {
            if (path) {
                opt = opt || {};
                opt.overWrite = false;
                return fs.mkDir(path, opt).catch(function (err) {
                    if (err.code === "EEXIST") {
                        var data = {question: "Directory " + path + " already exists. Overwrite directory?"};
                        displayQuestion.create(data)
                            .on("ok", function (e, view) {
                                opt.overWrite = true;
                                fs.mkDir(path, opt).then(function (res) {
                                    resolve(res);
                                }).catch(function (err) { console.log(err); reject(err); });
                                view.remove();
                            }).on("cancel", function (e, view) {
                                view.remove();
                                reject();
                            });
                    }
                });
            } else {
                reject({
                    code: "INVALID_PATH",
                    message: "Invalid path " + JSON.stringify(path),
                    path: path
                });
            }
        });
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new FileSystem();
            }
            return instance;
        }
    };
});
