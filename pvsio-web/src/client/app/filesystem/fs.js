/**
 * @module fs
 * @desc File system utils
 * @author Paolo Masci
 * @date Nov 08, 2018
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define, Promise */
define(function (require, exports, module) {
    "use strict";

    class FileSystem {
        constructor(ws) {
            this.ws = ws;
        }
        writeFile (path, content, opt) {
            opt = opt || {};
            opt.encoding = opt.encoding || "utf8";
            if (content && typeof content === "object") {
                content = JSON.stringify(content, null, " ");
            }
            let _this = this;
            if (path.indexOf("./") === 0) {
                path = path.split("/").slice(1).join("/");
            }
            return new Promise(function (resolve, reject) {
                if (!path) {
                    reject({
                        code: "INVALID_ARG",
                        message: "Invalid path " + path,
                        path: path
                    });
                } else {
                    if (_this.ws) {
                        const token = {
                            path: path,
                            name: path.split("/").slice(-1).join(""),
                            content: content || "",
                            encoding: opt.encoding,
                            opt: opt
                        };
                        _this.ws.writeFile(token, function (err, res) {
                            if (err || res.path !== token.path) {
                                return reject(err);
                            }
                            return resolve({
                                message: "File " + res.path + " correctly written to disk" ,
                                path: token.path,
                                content: token.content,
                                encoding: token.encoding
                            });
                        });
                    } else {
                        console.error("Cannot write file, WebSocket is closed :/");
                    }
                }
            });
        }
        readFile (path, opt) {
            opt = opt || {};
            opt.encoding = opt.encoding || "utf8";
            let _this = this;
            if (path.indexOf("./") === 0) {
                path = path.split("/").slice(1).join("/");
            }
            return new Promise(function (resolve, reject) {
                if (!path) {
                    reject({
                        code: "INVALID_ARG",
                        message: "Invalid path " + path,
                        path: path
                    });
                } else {
                    if (_this.ws) {
                        const token = {
                            path: path,
                            name: path.split("/").slice(-1).join(""),
                            encoding: opt.encoding,
                            opt: opt
                        };
                        _this.ws.readFile(token, function (err, res) {
                            if (err) {
                                return reject(err);
                            }
                            return resolve({
                                content: res.content,
                                encoding: res.encoding,
                                name: res.name
                            });
                        });
                    } else {
                        console.error("Cannot read file, WebSocket is closed :/");
                    }
                }
            });
        }
    }

    module.exports = FileSystem;
});
