/**
 * @module RemoteFileBrowser
 * @version 1.1
 * @description
 * Allows readonly access to the file system.

    var RemoteFileBrowser = require("plugins/prototypebuilder/RemoteFileBrowser");
    //create a new RemoteFileBrowser instance and open the default projects directory
    //you can pass in a directory relative to the projects directory to open a different directory
    new RemoteFileBrowser(null).open("")
        .then(function (files) {
            var paths = files.map(function (f) {
                return f.path;
            }).join(",");
            console.log(paths);
        });
 * @author Patrick Oladimeji, Paolo Masci
 * @date 3/17/15 22:30:00 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Promise, Handlebars, $ */
define(function (require, exports, module) {
    "use strict";
    var TreeList        = require("./TreeList"),
        WSManager       = require("websockets/pvs/WSManager"),
        template        = require("text!pvsioweb/forms/templates/fileBrowser.handlebars"),
        BaseDialog      = require("pvsioweb/forms/BaseDialog"),
        MIME            = require("util/MIME").getInstance(),
        PreferenceStorage = require("preferences/PreferenceStorage").getInstance(),
        PreferenceKeys = require("preferences/PreferenceKeys");
    var timer, rfb;

    var OpenFilesView = BaseDialog.extend({
        render: function (data) {
            var t = Handlebars.compile(template);
            this.$el.html(t(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel",
            "input #baseDirectory": "onTextChanged",
            "input #currentPath": "onEdit",
            "click #btnHome": "selectHome",
            "click #btnEdit": "enableEdit",
            "click #btnUp": "goUpADirectory"
        },
        onTextChanged: function (event) {
            clearTimeout(timer);
            timer = setTimeout(function () {
                rfb.rebaseDirectory($("#baseDirectory").val());
            }, 100);
        },
        onEdit: function (event) {
            //TODO
        },
        selectHome: function (event) {
            rfb.rebaseDirectory("~");
            rfb._treeList.selectItem("~");
        },
        enableEdit: function (event) {
            if (document.getElementById("currentPath").readOnly) {
                document.getElementById("btnEdit").style.backgroundColor = "white";
                document.getElementById("currentPath").readOnly = false;
                document.getElementById("currentPath").focus();
            } else {
                document.getElementById("btnEdit").style.backgroundColor = "";
                document.getElementById("currentPath").readOnly = true;
            }
        },
        goUpADirectory: function (event) {
            var dir = d3.select("#currentPath");
            var path = rfb._baseDirectory;
            var levelUp = path.substr(0, path.lastIndexOf("/"));
            rfb.rebaseDirectory(levelUp);
            //update the directory shown on the top of the window
            dir.property("value", levelUp);
        }
    });

    /**
        Constructs a new instance of the RemoteFileBrowser
        @param {function} filterFunc a function to filter which file should be shown in the browser if null, then all files are shown
    */
    function RemoteFileBrowser(filterFunc) {
        rfb = this;
        timer = null;
        this.filterFunc = filterFunc || function (d) { return true; };
    }

    RemoteFileBrowser.prototype._treeList = null;

    RemoteFileBrowser.prototype._baseDirectory = null;
    /**
     * Utility function to sort the list of files returned by the remote file browser
     * @param   {String}   a first argument
     * @param   {String}   b second argument of the sort
     * @returns {number} -1 if a < b, 0 if a === b and 1 if a > b
     */
    function fileSort(a, b) {
        return a.path.toLowerCase() < b.path.toLowerCase()
            ? -1 : a.path.toLowerCase() === b.path.toLowerCase() ? 0 : 1;
    }

    function getRemoteDirectory(path) {
        var ws = WSManager.getWebSocket();
        return new Promise(function (resolve, reject) {
            ws.send({type: "readDirectory", path: path}, function (err, res) {
                if (err) {
                    reject(err);
                } else {
                    resolve(res.files.filter(rfb.filterFunc).sort(fileSort));
                }
            });
        });
    }

    /**
     * Gets the current base directory whose content is being rendered in the remote browser.
     * @returns {String} The base directory
     */
    RemoteFileBrowser.prototype.getBaseDirectory = function () {
        return this._baseDirectory;
    };

    RemoteFileBrowser.prototype.rebaseDirectory = function (path) {
        var self = this;
        getRemoteDirectory(path)
            .then(function (files) {
                var data = {name: path, path: path, children: files, isDirectory: true};
                self._treeList.data = data;
                self._treeList.render(data);
                self._baseDirectory = path;
            }).catch(function (err) {
                self._treeList.data = [];
            });
    };

    /**
     Opens a dialog to browse a remote directory.
     @param {string} path the path relative to the project directory whose content should be shown in the browser
     @returns {Promise} a Promise that settled (with an array containing selected files/folders [{path: <string>}]) when the user presses the ok or cancel button on the dialog
    */
    RemoteFileBrowser.prototype.open = function (path, opt) {
        var KB = 1000, MB = KB * KB, GB = KB * MB;
        var result;
        function toUserFriendlySize(sizeInBytes) {
            if (sizeInBytes < KB) {
                return {value: sizeInBytes, unit: "B"};
            } else if (sizeInBytes < MB) {
                result = {value: sizeInBytes / KB, unit: "KB"};
            } else if (sizeInBytes < GB ) {
                result = {value: sizeInBytes / MB, unit: "MB"};
            } else {
                result = {value: sizeInBytes / GB, unit: "GB"};
            }
            result.value = result.value.toFixed(0);
            return result;
        }

        path = path || "~";
        opt = opt || {};
        var view = new OpenFilesView({baseDirectory: path, title: (opt.title || "Open file")});
        var self = this;
        getRemoteDirectory(path)
            .then(function (files) {
                var data = {name: path, path: path, children: files || [], isDirectory: true};
                self._treeList = new TreeList(data, "#file-browser", true);
                self._treeList.addListener("SelectedItemChanged", function (event) {
                    var data = event.data,
                        size = data.stats ? toUserFriendlySize(data.stats.size) : "";
                    document.getElementById("currentPath").value = data.path;
                    d3.select("#file-name").html(data.path);
                    d3.select("#file-size").html(size.value);
                    d3.select("#file-size-unit").html(size.unit);
                    d3.select("#file-last-modified").html(new Date(data.stats.modified).toString());
                    //if the file is an image retrieve a preview
                    if (MIME.isImage(data.path)) {
                        WSManager.getWebSocket().readFile({path: data.path, encoding: "base64"}, function (err, res) {
                            if (!err) {
                                d3.select("#image-preview").attr("src", res.content);
                            }
                        });
                    }
                    if (data.isDirectory && !data.children && !data._children) {
                        getRemoteDirectory(data.path)
                            .then(function (files) {
                                data.children = files || [];
                                if (data.children.length === 0) {
                                    data.empty = true;
                                }
                                self._treeList.render(data);
                            }).catch(function (err) {
                                console.log(err);
                            });
                    }
                });
            });
        return new Promise(function (resolve, reject) {
            view.on("cancel", function (event, view) {
                clearTimeout(timer);
                view.remove();
                reject();
            }).on("ok", function (event, view) {
                clearTimeout(timer);
                var selectedFiles = self._treeList.getSelectedItems();
                var firstFile = selectedFiles[0],
                    fileDirectory;
                if (firstFile.isDirectory) {
                    fileDirectory = firstFile.path;
                } else {
                    fileDirectory = firstFile.parent.path;
                }
                if (PreferenceStorage.get(PreferenceKeys.REMEMBER_LAST_DIRECTORY)) {
                    PreferenceStorage.set(PreferenceKeys.LAST_DIRECTORY_VISITED, fileDirectory);
                }
                resolve(selectedFiles);
                view.remove();
            });
        });
    };


    module.exports = RemoteFileBrowser;
});
