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
        EmuchartsManager = require("plugins/emulink/EmuchartsManager"),
        dateFormat  = require("lib/dateformat"),
        PreferenceKeys = require("preferences/PreferenceKeys");

    var timer, rfb, grabPosition = [0,0], source;

    Handlebars.registerHelper("inc", function(value, options) {
        return parseInt(value) + 1;
    });

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
            "click .bookmark": "bookmarks",
            "dragstart .bookmark": "handleDragStart",
            "dragenter .bookmark": "handleDragEnter",
            "dragover .bookmark": "handleDragOver",
            "dragleave .bookmark": "handleDragLeave",
            "dragend .bookmark": "handleDragEnd",
            "drop .bookmark": "handleDrop",
            "dragstart .line": "handleDragStartLine",
            "dragleave .line": "handleDragLeaveLine",
            "dragenter .line": "handleDragEnterLine",
            "dragover .line": "handleDragOverLine",
            "dragend .line": "handleDragEndLine",
//            "mouseover #image-svg-preview": "handleMouseEnter",
//            "mouseleave #file-preview": "handleMouseLeave",
//            "mouseleave #preview-zoom": "handleMouseLeave",
            "dblclick .line": "handleDoubleClickLine"
        },
        onTextChanged: function (event) {
            clearTimeout(timer);
            timer = setTimeout(function () {
                rfb.rebaseDirectory($("#baseDirectory").val());
            }, 100);
        },
        onEdit: function (event) {
            clearTimeout(timer);
            timer = setTimeout(function () {
                rfb.rebaseDirectory($("#currentPath").val());
            }, 100);
        },
        selectHome: function (event) {
            document.getElementById("currentPath").value = rfb.examplesFolder;
            rfb.rebaseDirectory(rfb.examplesFolder);
            rfb._treeList.selectItem(rfb.examplesFolder);
        },
        enableEdit: function (event) {
            if (document.getElementById("currentPath").readOnly) {
                document.getElementById("btnEdit").style.backgroundColor = "white";
                document.getElementById("currentPath").readOnly = false;
                document.getElementById("currentPath").focus();
                document.getElementById("currentPath").addEventListener("keydown", keyHandler, false);
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
        },
        bookmarks: function(event) {
            var key = event.currentTarget.innerText.trim();
            var paths = PreferenceStorage.get(PreferenceKeys.BOOKMARKS);
            rfb.rebaseDirectory(paths[key]);
            document.getElementById("currentPath").value = paths[key];
        },
        handleDragStartLine: function(event) {
            if (event.currentTarget.getAttribute("draggable") === "true") {
                document.getElementById("file-bookmarks").classList.add("over");
                source = event.currentTarget;
                event.originalEvent.dataTransfer.effectAllowed = "move";

                var posX = event.originalEvent.offsetX;
                var posY = event.originalEvent.offsetY;

                grabPosition[0] = posX;
                grabPosition[1] = posY;
            } else {
                return false;
            }

        },
        handleDragStart: function(event) {

            document.getElementById(event.currentTarget.id).classList.add("opacity");

            source = event.currentTarget;
            event.originalEvent.dataTransfer.effectAllowed = "move";
            event.originalEvent.dataTransfer.setData('text', event.currentTarget.innerHTML);

            var posX = event.originalEvent.offsetX;
            var posY = event.originalEvent.offsetY;

            grabPosition[0] = posX;
            grabPosition[1] = posY;
        },
        handleDragEnter: function(event) {
            if (event.currentTarget.id.indexOf("bookmark-list") > -1) {
                if (source !== event.currentTarget) {
                    document.getElementById(event.currentTarget.id).classList.add("over");
                }
            }
            return false;
        },
        handleDragEnterLine: function(event) {
            if (event.preventDefault) {
                event.preventDefault();
            }

            return false;
        },
        handleDragOver: function(event) {
            if (event.preventDefault) {
                event.preventDefault();
            }

            event.originalEvent.dataTransfer.effectAllowed = "move";

            return false;
        },
        handleDragOverLine: function(event) {
            if (event.preventDefault) {
                event.preventDefault();
            }

            event.originalEvent.dataTransfer.effectAllowed = "move";

            return false;
        },
        handleDragLeave: function(event) {
            document.getElementById(event.currentTarget.id).classList.remove("over");
            console.log("leave");

            return false;
        },
        handleDragLeaveLine: function(event) {

            if (event.preventDefault()) {
                event.preventDefault();
            }

            return false;
        },
        handleDrop: function(event) {
            if (event.stopPropagation()){
                event.stopPropagation();
            }

            if (event.preventDefault()) {
                event.preventDefault();
            }

            if (source.id.indexOf("line") > -1) {
                var path = source.__data__.path;
                var key = source.innerText.trim();
                var paths = PreferenceStorage.get(PreferenceKeys.BOOKMARKS);

                if (!(paths[key])) {
                    var id = Object.keys(paths).length;

                    var div = document.createElement("div");
                    div.setAttribute("id","bookmark-list-" + (id+1));
                    div.setAttribute("class","bookmark");
                    div.setAttribute("draggable",true);

                    var li = document.createElement("li");

                    var span_icon = document.createElement("span");
                    span_icon.setAttribute("class","glyphicon glyphicon-folder-close");
                    var span_text = document.createElement("span");
                    span_text.setAttribute("class","bookmark-label");
                    span_text.textContent = " " + key;
                    li.appendChild(span_icon);
                    li.appendChild(span_text);

                    div.appendChild(li);

                    document.getElementById("bookmark-ul").appendChild(div);
                }

                paths[key] = path;
                PreferenceStorage.set(PreferenceKeys.BOOKMARKS,paths);

            }

            if (source.id.indexOf("bookmark-list") > -1) {
                if (source !== event.currentTarget) {
                    source.innerHTML = event.currentTarget.innerHTML;
                    event.currentTarget.innerHTML = event.originalEvent.dataTransfer.getData('text');
                }
            }

            document.getElementById(event.currentTarget.id).classList.remove("over");
            return false;
        },
        handleDragEnd: function(event) {
            if (event.preventDefault) {
                event.preventDefault();
            }

            if (isOutside(event,"open-file-browser")) {
                var elem = document.getElementById(event.currentTarget.id);
                elem.remove();
                var key = source.innerText.trim();
                var paths = PreferenceStorage.get(PreferenceKeys.BOOKMARKS);
                delete paths[key];
                PreferenceStorage.set(PreferenceKeys.BOOKMARKS, paths);
            } else {

                document.getElementById(event.currentTarget.id).classList.remove("opacity");
            }

            return false;
        },
        handleDragEndLine: function(event) {
            if (event.preventDefault) {
                event.preventDefault();
            }

            if (source.id.indexOf("line") > -1 && !isOutside(event,"file-bookmarks")) {
                var path = source.__data__.path;
                var key = source.innerText.trim();
                var paths = PreferenceStorage.get(PreferenceKeys.BOOKMARKS);

                if (!(paths[key])) {
                    var id = Object.keys(paths).length;

                    var div = document.createElement("div");
                    div.setAttribute("id","bookmark-list-" + (id+1));
                    div.setAttribute("class","bookmark");
                    div.setAttribute("draggable",true);

                    var li = document.createElement("li");

                    var span_icon = document.createElement("span");
                    span_icon.setAttribute("class","glyphicon glyphicon-folder-close");
                    var span_text = document.createElement("span");
                    span_text.setAttribute("class","bookmark-label");
                    span_text.textContent = " " + key;
                    li.appendChild(span_icon);
                    li.appendChild(span_text);

                    div.appendChild(li);

                    document.getElementById("bookmark-ul").appendChild(div);
                }

                paths[key] = path;
                PreferenceStorage.set(PreferenceKeys.BOOKMARKS,paths);

            }

            document.getElementById("file-bookmarks").classList.remove("over");
            document.getElementById(event.currentTarget.id).classList.remove("opacity");

            return false;
        },
        handleMouseEnter: function(event) {

            event.preventDefault();
            var path = document.getElementById("currentPath").value;

            d3.select("#preview-zoom").style("display","block");//.style("right", "110%").style("top", "10%");

            if (MIME.isEmucharts(path)) {
                WSManager.getWebSocket().readFile({path: path}, function (err, res) {
                    if (!err) {
                        rfb.emuchartsManager.preview(JSON.parse(res.content), {
                            container: "#svg-preview-zoom",
                            scale_zoom: 0.6,
                            type: "zoom"
                        });
                    }
                });
            }

        },
        handleMouseLeave: function(event) {
            event.preventDefault();
            d3.select("#preview-zoom").style("display","none").style("right", "0px").style("top", "0px");
        },

        handleDoubleClickLine: function(event) {
            event.preventDefault();
            var selectedFiles = rfb._treeList.getSelectedItems();
            var firstFile = selectedFiles[0];
            if (firstFile && !firstFile.isDirectory) {
                d3.select("#open-file-browser").select("#btnOk").node().click();
            }
        }

    });

    function isOutside(evt, id) {
        var element = document.getElementById(id).getBoundingClientRect();

        var posX = evt.originalEvent.clientX + grabPosition[0];
        var posY = evt.originalEvent.clientY + grabPosition[1];

        if (posX < element.left || posX > (element.left + element.width)) {
            return true;
        } else {
            if (posY < element.top || posY > (element.top + element.height)) {
                return true;
            } else {
               return false;
            }
        }
    }

    function keyHandler(e) {
        var code = e.keyCode;
        var path;
        if (code === 9) {
            e.preventDefault();
            path = document.getElementById("currentPath").value;
            var tmp = path.substr(0, path.lastIndexOf("/"));
            var name = path.substr(path.lastIndexOf("/")+1, path.length-1);
            getRemoteDirectory(tmp).then(function (files) {

                if (files.length === 1) {

                    document.getElementById("currentPath").value = files[0].path;
                } else {
                    var found = false;
                    for (var j=0; j< files.length;j++) {
                        if (files[j].name.indexOf(name) > -1 && name !== "" && files[j].name.startsWith(name) && !found) {
                            document.getElementById("currentPath").value = files[j].path;
                            found = true;
                        }
                    }
                }

            }).catch(function (err) {
                console.log(err);
            });
        }

        if (code === 39) {
            path = document.getElementById("currentPath").value;
            if (path.substr(path.length-1) !== "/") {
                document.getElementById("currentPath").value = path.concat("/");
                rfb.rebaseDirectory(path.concat("/"));
            }
        }
        if (code === 13) {
            e.preventDefault();
            window.location.hash = '#file-browser';
        }
    }

    /**
        Constructs a new instance of the RemoteFileBrowser
        @param {function} filterFunc a function to filter which file should be shown in the browser if null, then all files are shown
    */
    function RemoteFileBrowser(filterFunc) {
        rfb = this;
        timer = null;
        this.emuchartsManager = EmuchartsManager.getInstance();
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

    function readExamplesFolder() {
        var ws = WSManager.getWebSocket();
        return new Promise(function (resolve, reject) {
            ws.send({ type: "readExamplesFolder" }, function (err, res) {
                if (err) {
                    reject(err);
                } else {
                    resolve({
                        files: res.files.filter(rfb.filterFunc).sort(fileSort),
                        examplesFolder: res.path
                    });
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
        getRemoteDirectory(path)
            .then(function (files) {
                var data = {name: path, path: path, children: files, isDirectory: true};
                rfb._treeList.data = data;
                rfb._treeList.render(data);
                rfb._baseDirectory = path;
            }).catch(function (err) {
                rfb._treeList.data = [];
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

        function toUserFriendlyDate(date) {
            return dateFormat(date, "dddd, mmmm dd, yyyy 'at' HH:MM:ss");
        }

        path = path || "";
        opt = opt || {};
        var view = new OpenFilesView({baseDirectory: path, bookmarks: PreferenceStorage.get(PreferenceKeys.BOOKMARKS), title: (opt.title || "Open file")});
//        getRemoteDirectory(path)
        readExamplesFolder()
            .then(function (ans) {
                var files = ans.files;
                var path = ans.examplesFolder;
                rfb.examplesFolder =  ans.examplesFolder;
                rfb._baseDirectory = ans.examplesFolder;
                document.getElementById("currentPath").value = ans.examplesFolder;
                document.getElementById("baseDirectory").value = ans.examplesFolder;
                var data = { name: path, path: path, children: files || [], isDirectory: true };
                rfb._treeList = new TreeList(data, "#file-browser", true);
                rfb._treeList.addListener("SelectedItemChanged", function (event) {
                    var data = event.data,
                        size = data.stats ? toUserFriendlySize(data.stats.size) : "",
                        modified = data.stats ? toUserFriendlyDate(data.stats.modified) : "";
                    document.getElementById("currentPath").value = data.path;
                    d3.select("#file-name").html(data.path);
                    d3.select("#file-size").html(size.value);
                    d3.select("#file-size-unit").html(size.unit);
                    d3.select("#file-last-modified").html(modified);

                    //if the file is an image retrieve a preview
                    if (MIME.isImage(data.path)) {
                        WSManager.getWebSocket().readFile({path: data.path, encoding: "base64"}, function (err, res) {
                            if (!err) {
                                d3.select("#image-preview").attr("src", res.content).style("display", "block");
                            }
                        });
                    }
                    if (MIME.isEmucharts(data.path)) {
                        WSManager.getWebSocket().readFile({path: data.path}, function (err, res) {
                            if (!err) {
                                var tmp = JSON.parse(res.content);
                                if (tmp && tmp.chart) {
                                    rfb.emuchartsManager.preview(tmp.chart, {
                                        container: "#svg-preview",
                                        scale_zoom: 0.3,
                                        type: "zoom"
                                    });
                                    d3.select("#file-preview").style("display", "block");
                                }
                            }
                        });
                    }

                    if (data.isDirectory) {
                        d3.select("#file-browser").style("width", "82.5%");
                        d3.select("#file-preview").style("display", "none");
                    } else {
                        d3.select("#file-browser").style("width", "57%").style("float", "left").style("margin-left","0");
                        d3.select("#file-preview").style("width", "25.5%").style("display", "block");
                    }
                }).addListener("ItemExpanded", function (event) {
                    var data = event.data;

                    if (data.isDirectory && !data.children && !data._children && !data.empty && !data._empty) {
                        getRemoteDirectory(data.path)
                            .then(function (files) {
                                data.children = files || [];
                                if (data.children.length === 0) {
                                    data.empty = true;
                                }
                                rfb._treeList.render(data);
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
                resolve();
            }).on("ok", function (event, view) {
                clearTimeout(timer);
                var selectedFiles = rfb._treeList.getSelectedItems();
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
