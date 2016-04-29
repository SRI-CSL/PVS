/**
 * Utility functions used by the pvssocketserver for dealing with filesystem etc
 * @author Patrick Oladimeji, Paolo Masci
 * @date 6/26/14 11:29:07 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global require, module, __dirname*/

var fs = require("fs"),
    path = require("path"),
    Promise                 = require("es6-promise").Promise,
    logger                  = require("tracer").console(),
    imageExts = [".jpg", ".jpeg", ".png"],
    baseProjectDir          = path.join(__dirname, "../../examples/projects/"),
    filesFilter = [".pvs", ".tex", ".txt", ".i", ".json"].concat(imageExts);

var noop = function () { "use strict"; };
 /**
 * Recursively creates a directory structure while ensuring that any non-existent parent folders
 * are created as necessary. E.g., to create /temp/foo/foo/test in the directory /temp without a foo
 * directory, the function ensures that the appropriate parent directories foo/foo are created
 * @param {string} dirPath the path to the directory to create
 * @param {function (err)} cb the callback function to invoke when the directory creation is complete
 */
function mkdirRecursive(dirPath, cb) {
    "use strict";
	cb = cb || noop;
    try {
        fs.mkdirSync(dirPath);
        cb();
    } catch (mkdirErr) {
        if (mkdirErr && mkdirErr.code === "ENOENT") {
            // the callback will be invoked only by the first instance of mkdirRecursive
            var parentDirectory = dirPath.substr(0, dirPath.lastIndexOf("/"));
            mkdirRecursive(parentDirectory, function (error) {
                // note: multiple instances of this function might be running in parallel
                // because the caller could have invoked the function using Promise.all(promises)
                // we therefore need to handle the case EEXIST (two or more instances could
                // be competing for the creation of the same parent directories)
                if (!error || error.code === "EEXIST") {
                    try {
                        fs.mkdirSync(dirPath);
                        cb();
                    } catch (mkdirErr2) {
                        cb(mkdirErr2);
                    }
                } else {
                    cb(error);
                }
            });
        } else {
            // if the path has been created successfully, just invoke the callback function (if any)
            cb();
        }
    }
}
/**
 * Get the stat for the file in the specified path
 * @returns {Promise} a promise that resolves with the stat object of the file
  see http://nodejs.org/api/fs.html#fs_class_fs_stats for details
 */
function stat(fullPath) {
    "use strict";
    return new Promise(function (resolve, reject) {
        fs.stat(fullPath, function (err, res) {
            if (err) {
                reject(err);
            } else {
                resolve(res);
            }
        });
    });
}
/**
    Reads the content of the file at the specified fullPath
    @param {String} fullPath the full path to the file to read
    @param {String} encoding the encoding of the file being read (default is utf8)
    @returns {Promise} a promise that resolves with the content of the file
*/
function readFile(fullPath, encoding) {
    "use strict";
    encoding = encoding || "utf8";
    return new Promise(function (resolve, reject) {
        fs.readFile(fullPath, {encoding: encoding}, function (err, content) {
            if (err) {
                reject(err);
            } else {
                if (encoding === "base64") {
                    var ext = path.extname(fullPath).toLowerCase();
                    resolve("data:image/" + ext.substr(1).toLowerCase() + ";base64," + content);
                } else {
                    resolve(content);
                }
            }
        });
    });
}

/**
 * Checks whether or not a path points to an image
 * @param   {String} fullPath The full path to the file
 * @returns {boolean} true if the path points to a recognised image
 */
function isImage(fullPath) {
    var ext = path.extname(fullPath).toLowerCase();
    return imageExts.indexOf(ext) > -1;
}

/**
 Writes a file with the specified content to the specified path. If the parent folders of the specified path
 do not exist, they are created
 @param {string} fullPath the full path to the file
 @param {string} content the content of the file
 @param {string?} fileEncoding the encoding to use for writing the file (defaults to utf8)
 @returns {Promise} a promise that resolves when file has been written or rejects when an error occurs
*/
///TODO clean up these parameters - propose to make into one json object with the parameters as properties of the object
function writeFile(fullPath, content, fileEncoding, opt) {
    "use strict";
    fileEncoding = fileEncoding || "utf8";
    //remove prefixes from file content before saving images
    if (content && isImage(fullPath)) {
        content = content.replace(/^data:image\/(\w+);base64,/, "");
    }
    return new Promise(function (resolve, reject) {
        function fileExists(f) {
            try {
                var ans = fs.statSync(fullPath);
                if (ans && ans.isFile()) {
                    return true;
                }
                return false;
            } catch (fileExistsError) {
                return false;
            }
        }
        
        if (typeof fullPath !== "string") {
            reject({ type: "writeFile_error", path: fullPath, err: "Incorrect path (" + fullPath + ")"});
        } else if (typeof content !== "string") {
            reject({ type: "writeFile_error", path: fullPath, err: "Incorrect file content (" + content + ")"});
        } else {
            var folder = fullPath.substring(0, fullPath.lastIndexOf(path.sep));
            mkdirRecursive(folder, function (err) {
                if (!err || (err && err.code === "EEXIST")) {
                    if ((opt && opt.overWrite) || !fileExists(fullPath)) {
                        try {
                            fs.writeFileSync(fullPath, content, fileEncoding);
                            resolve({path: fullPath, content: content, encoding: fileEncoding});
                        } catch (writeFileErr) {
                            reject(writeFileErr);
                        }
                    } else {
                        reject({
                            code: "EEXIST",
                            message: "File already exists",
                            path: fullPath
                        });
                    }
                } else {
                    reject(err);
                }
            });
        }
    });
}

/**
    Recursively reads the files in a directory using promises
    @param {string} fullPath the path to the directory to read
    @param {array} filter a list of extensions for files whose contents we wish to get. If filter is null, false or undefined then no content will be returned for any files
    @returns {Promise} a promise that resolves with an array of objects  for the files in the given directory.
    The object may contain just path property or may include content if the getContent parameter was passed
*/
function getFolderTree(fullPath, filter) {
    "use strict";
    return stat(fullPath).then(function (f) {
        if (f.isDirectory()) {
            return new Promise(function (resolve, reject) {
                fs.readdir(fullPath, function (err, files) {
                    if (err) {
                        reject(err);
                    } else {
                        var promises = files.map(function (name) {
                            var file = path.join(fullPath, name);
                            return getFolderTree(file, filter);
                        });

                        Promise.all(promises)
                            .then(function (res) {
                                var flattened = res.reduce(function (a, b) {
                                    if (Array.isArray(b)) {
                                        return a.concat(b);
                                    } else {
                                        a.push(b);
                                        return a;
                                    }
                                }, []);
                                // we need to include the directory as well,
                                // otherwise the client will not see the directory it if it's empty
                                flattened.push({path: fullPath, isDirectory: true});
                                resolve(flattened);
                            }, reject);
                    }
                });
            });
        } else {
            //resolve with filename and content only for model files and images (they are listed in filter)
            return new Promise(function (resolve, reject) {
                var ext = path.extname(fullPath).toLowerCase();
                var opt = {encoding: isImage(fullPath) ? "base64" : "utf8"};
                if (filter && filter.indexOf(ext) > -1) {
                    fs.readFile(fullPath, opt, function (err, data) {
                        if (err) {
                            reject(err);
                        } else {
                            resolve({
                                path: fullPath,
                                content: isImage(fullPath) ? ("data:image/" + ext.substr(1).toLowerCase() + ";base64," + data) : data,
                                encoding: opt.encoding
                            });
                        }
                    });
                } else {
                    resolve({
                        path: fullPath,
                        encoding: opt.encoding
                    });
                }
            });
        }
    }, function (err) {
        return Promise.reject(err);
    });
}


/**
* open a project with the specified projectName
* @param {string} projectName the name of the project to open
* @returns {Promise} a promise that resolves with data for the opened project
*/
function openProject(projectName) {
    "use strict";
    logger.debug("Opening project " + projectName + " ...");
    var projectPath = path.join(baseProjectDir, projectName),
        res = { name: projectName };

    return new Promise(function (resolve, reject) {
        //get filepaths and their contents
        getFolderTree(projectPath, filesFilter)
            .then(function (files) {
                res.descriptors = files.filter(function (f) { return !f.isDirectory; }).map(function (f) {
                    f.name = f.path.split("/").slice(-1).join("");
                    f.path = f.path.replace(projectPath, projectName);
                    return f;
                });
                resolve(res);
            }).catch(reject);
    });
}

/**
 * Lists all the projects on the server by listing folder names in the projects directory
 * @return {Promise} a promise that resolves with a list of project names
 */
function listProjects() {
    "use strict";
    return new Promise(function (resolve, reject) {
        fs.readdir(baseProjectDir, function (err, files) {
            if (err) {
                reject(err);
            } else {
                Promise.all(files.map(function (file) {
                    return stat(path.join(baseProjectDir, file))
                        .then(function (f) {
                            if (f.isDirectory()) {
                                return new Promise(function (resolve, reject) {
                                    //get the image in the directory if any
                                    fs.readdir(path.join(baseProjectDir, file), function (err, files) {
                                        if (err) {
//											reject(err); // we send all directories, also those that are empty.
                                            resolve({name: file, image: null});
                                        } else {
                                            var image = files.filter(function (f) {
                                                return isImage(f);
                                            })[0];
                                            var result = files.length ? {name: file, image: image} : {name: file, image: null};
                                            resolve(result);
                                        }
                                    });
                                });
                            } else { return Promise.resolve(null); }
                        }).catch(reject);
                })).then(function (files) {
                    resolve(files.filter(function (f) { return f; }));
                }).catch(reject);
            }
        });
    });
}

/**
    renames the oldpath into the newPath
    @returns {Promise} a promise that resolves with the new path name
*/
function renameFile(oldPath, newPath) {
    "use strict";
    oldPath = path.join(baseProjectDir, oldPath);
    newPath = path.join(baseProjectDir, newPath);

    return new Promise(function (resolve, reject) {
        stat(newPath).then(function (res) {
            reject("ENOTEMPTY");
        }).catch(function (err) {
            if (err && err.code === "ENOENT") {
                //file does not exist so ok to rename
                fs.rename(oldPath, newPath, function (err) {
                    if (err) {
                        reject(err);
                    } else {
                        resolve(newPath);
                    }
                });
            } else {
                reject(err);
            }
        });
    });
}

module.exports = {
    renameFile: renameFile,
    mkdirRecursive: mkdirRecursive,
    stat: stat,
    readFile: readFile,
    writeFile: writeFile,
    getFolderTree: getFolderTree,
    openProject: openProject,
    listProjects: listProjects,
    isImage: isImage
};
