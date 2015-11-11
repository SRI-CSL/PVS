/**
 *  Contains utility functions for handling files
 * @author Patrick Oladimeji, Enrico D'Urso, Paolo Masci
 * @date 11/14/13 8:15:57 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Promise, FileReader*/
define(function (require, exports, module) {
    "use strict";
    var openJSON = require("util/forms/openTextFile");


    /**
     *
     */
    function createFileLoadFunction(file, onFileLoaded) {
        return function (cb) {
            var fr = new FileReader();
            fr.onload = function (event) {
                var content = event.target.result;
                if (onFileLoaded && typeof onFileLoaded === "function") {
                    onFileLoaded(file, content);
                }
                if (cb && typeof cb === "function") { cb(); }
            };
            fr.readAsText(file);
        };
    }

    function readLocalFileAsImage(file) {
        return new Promise(function (resolve, reject) {
            var fr = new FileReader();
            fr.onload = function (event) {
                var content = event.target.result;
                resolve({name: file.name, content: content, encoding: "base64"});
            };
            fr.onerror = function (event) {
                reject(event);
            };
            fr.readAsDataURL(file);
        });
    }


    function readLocalFileAsText(file) {
        return new Promise(function (resolve, reject) {
            var fr = new FileReader();
            fr.onload = function (event) {
                var content = event.target.result;
                resolve({name: file.name, content: content, encoding: "utf8"});
            };
            fr.onerror = function (event) {
                reject(event);
            };
            fr.readAsText(file);
        });
    }

    function readLocalFile(file) {
        if (file) {
            if (file.type && file.type.indexOf("image/") === 0) {
                return readLocalFileAsImage(file);
            }
            return readLocalFileAsText(file);
        }
        return Promise.reject();
    }


    /**
     * Opens text files and returns JSON objects
     * @param callback is the callback function invoked when the files are open
     * @param opt is a descriptor containing { query, fileExt },
     * where query is the title of the created open window, and FileExt specifies the file extensions (e.g., ".txt,.json")
     * @returns { name, content }, where filename is a string, and content is a JSON object
     * @memberof fileHandler
     */
    function openLocalFileAsJSON(callback, opt) {
        if (!callback || typeof callback !== "function") { return; }
        openJSON.create({
            header: opt.header || "Open JSON file...",
            extensions: opt.extensions || ".json"
        }).on("cancel", function (event, view) {
            view.remove();
            return { event: event };
        }).on("ok", function (event, view) {
            view.remove();
            if (event.data && event.data.file && event.data.file.length > 0) {
                var file = event.data.file[0];
                var fr = new FileReader();
                fr.onload = function (event) {
                    callback(null, {
                        name: file.name,
                        content: JSON.parse(event.target.result)
                    });
                };
                fr.onerror = function (event) {
                    callback(event.target.err);
                };
                fr.readAsText(file);
            }
        });
    }


    /**
     * Opens text files
     * @param callback is the callback function invoked when the files are open
     * @param opt is a descriptor containing { query, fileExt },
     * where query is the title of the created open window, and FileExt specifies the file extensions (e.g., ".txt,.json")
     * @returns { name, content }, where name and content are strings
     * @memberof fileHandler
     */
    function openLocalFileAsText(callback, opt) {
        if (!callback || typeof callback !== "function") { return; }
        openJSON.create({
            header: opt.header || "Open text file...",
            extensions: opt.extensions || ".txt"
        }).on("cancel", function (event, view) {
            view.remove();
            return { event: event };
        }).on("ok", function (event, view) {
            view.remove();
            if (event.data && event.data.file && event.data.file.length > 0) {
                var file = event.data.file[0];
                var fr = new FileReader();
                fr.onload = function (event) {
                    callback(null, {
                        name: file.name,
                        content: event.target.result
                    });
                };
                fr.onerror = function (event) {
                    callback(event.target.err);
                };
                fr.readAsText(file);
            }
        });
    }


    /************* Exported Function ************************************************/
    // openXXX return pairs { name, content }
    // readXXX return a Promise
    module.exports = {
        openLocalFileAsJSON: openLocalFileAsJSON, // content is a JSON object
        openLocalFileAsText: openLocalFileAsText, // content is a string
        createFileLoadFunction: createFileLoadFunction,
        readLocalFile: readLocalFile
    };
});
