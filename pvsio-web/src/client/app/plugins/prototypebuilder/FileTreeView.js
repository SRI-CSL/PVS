/**
 * Renders a folder and the list of files and any folders within it.
 * @author Patrick Oladimeji
 * @date 1/14/14 11:53:17 AM
 */
/*jshint unused: false*/
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, unparam: true*/
/*global define, d3, Promise*/
define(function (require, exports, module) {
    "use strict";
    var eventDispatcher = require("util/eventDispatcher"),
        FileSystem      = require("filesystem/FileSystem"),
        QuestionForm    = require("pvsioweb/forms/displayQuestion"),
        NotificationManager = require("project/NotificationManager"),
        TreeList        = require("./TreeList");

    var elementId, project, projectManager, folderCounter = 0,
        unSavedFileName = "untitled_file", unSavedFolderName = "untitled_folder", treeList;

    var fs;
    ///FIXME Sort out the use of alert dialogs to notify errors here --
    ///also are there client side checks we can do to prevent some errors
    function FileTreeView(_elId, folderData, _projectManager) {
        eventDispatcher(this);
        var ftv = this;
        fs = new FileSystem();
        elementId = _elId;
        project = _projectManager.project();
        projectManager = _projectManager;

        //add listeners to project changes
        if (project) {
            project.addListener("DirtyFlagChanged", function (event) {
                //set file as dirty
                treeList.markDirty(event.file.path, event.file.dirty());
//            }).addListener("ProjectMainSpecFileChanged", function () {
//               //change the main file class
//
            }).addListener("FileAdded", function (event) {
                //add the new file to the tree list data
                treeList.addItem({
                    name: event.file.name,
                    path: event.file.path,
                    isDirectory: false
                });
            }).addListener("FileRemoved", function (event) {
                //delete the removed file from the tree list data
                ftv.deleteItem(event.file);
            });
        }

        treeList = new TreeList(folderData, elementId);
        treeList.addListener("SelectedItemChanged", function (event) {
            var e = {type: event.type, selectedItem: event.data};
            ftv.fire(e);
        }).addListener("Rename", function (event) {
            treeList.createNodeEditor(event.data, function (node, oldPath) {
                var f = project.getDescriptor(oldPath);
                if (event.data.isDirectory) {
                    if (oldPath !== node.path) {//we only want to rename folder if the name has actually been changed
                        project.renameFolder(oldPath, node.path, function (err, res) {
                            if (err) {
                                // alert user
                                if (err.message === "ENOTEMPTY") {
                                    alert("Error: the folder could not be renamed into " +
                                          err.newPath + " (another folder with the same name already exists)." +
                                          " Please choose a different name");
                                } else { alert(err.message); }
                                // revert to previous name
                                var prevData = event.data;
                                prevData.path = oldPath;
                                prevData.name = oldPath.substring(oldPath.lastIndexOf("/") + 1);
                                treeList.createNodeEditor(prevData);
                                // and trigger blur event to remove the overlay node used for renaming
                                treeList.blur();
                            } else {
                                // the path of all affected nodes is automatically updated in project.renameFolder
                                treeList.render(project.getDescriptors());
                            }
                        });
                    }

                } else {//renaming a file
                    project.renameFile(f, node.name, function (err, res) {
                        if (err) {
                            alert(JSON.stringify(err));
                            var prevData = event.data;
                            prevData.path = oldPath;
                            prevData.name = oldPath.substring(oldPath.lastIndexOf("/") + 1);
                            treeList.createNodeEditor(prevData);
                        }
                    });
                }
            });
        }).addListener("New File", function (event) {
            // files or folders might be filtered out in the PVSio-web file browser
            // so to choose a name for the new file or folder we can't rely on the
            // list of files and folders shown in the file browser -- we directly try
            // to create the file or folder, and change its name until we succeed
            var nameObj = { uid : 0, name: unSavedFileName, ext: ".pvs" };
            function getName() {
                if (nameObj.uid === 0) { return nameObj.name + nameObj.ext; }
                return nameObj.name + "_" + nameObj.uid + nameObj.ext;
            }
            function getPath() {
                // the returned path is relative to the folder selected in the PVSio-web file browser
                return event.data.path + "/" + getName();
            }
            function createFile(path, fuel) {
                return new Promise(function (resolve, reject) {
                    fs.writeFile(path, "", { silentMode: true })
                        .then(function (res) { resolve(res); })
                        .catch(function (err) {
                            if (err.code === "EEXIST") {
                                if (fuel > 0) {
                                    nameObj.uid++;
                                    return createFile(getPath(), fuel - 1);
                                } else {
                                    err.message = "Too many " + unSavedFileName +
                                        " files in the current folder -- PVSio-web could not generate " +
                                        " a fresh file name. Please delete one of these " + unSavedFileName +
                                        " files before creating a new file.";
                                    reject(err);
                                }
                            } else {
                                reject(err);
                            }
                        });
                });
            }
            createFile(getPath(), 64).catch(function (err) { NotificationManager.error(err.message); });
        }).addListener("New Folder", function (event) {
            var nameObj = { uid : 0, name: unSavedFolderName };
            function getName() {
                if (nameObj.uid === 0) { return nameObj.name; }
                return nameObj.name + "_" + nameObj.uid;
            }
            function getPath() {
                // the returned path is relative to the folder selected in the PVSio-web file browser
                return event.data.path + "/" + getName();
            }
            function createFolder(path, fuel) {
                return new Promise(function (resolve, reject) {
                    fs.mkDir(path, { silentMode: true })
                        .then(function (res) { resolve(res); })
                        .catch(function (err) {
                            if (err.code === "EEXIST") {
                                if (fuel > 0) {
                                    nameObj.uid++;
                                    return createFolder(getPath(), fuel - 1);
                                } else {
                                    err.message = "Too many " + unSavedFolderName +
                                        " in the current directory -- PVSio-web could not generate " +
                                        " a fresh folder name. Please delete one of these " + unSavedFolderName +
                                        " folders before creating a new file.";
                                    reject(err);
                                }
                            } else {
                                reject(err);
                            }
                        });
                });
            }
            createFolder(getPath(), 64).catch(function (err) { NotificationManager.error(err.message); });
        }).addListener("Delete", function (event) {
            var path = event.data.path, isDirectory = event.data.isDirectory;
            if (path === project.name()) {
                alert("Cannot delete project root directory.");
                return;
            }
            var isMainFile = (project.mainPVSFile()) ? (path === project.mainPVSFile().path) : false;
            QuestionForm.create({
                header: "Confirm Delete",
                question: (isMainFile) ? (path + " is currently set as Main File for the project. Are you sure you want to delete it?")
                                : ("Are you sure you want to delete " + path + "?"),
                buttons: ["Cancel", "Delete"]
            }).on("delete", function (e, view) {
                if (isDirectory) {
                    fs.rmDir(path)
                        .catch(function (err) { console.log(err); });
                } else {
                    fs.deleteFile(path)
                        .catch(function (err) { console.log(err); });
                }
                if (isMainFile) {
                    project.mainPVSFile(null);
                    d3.select("#btnSetMainFile").attr("disabled", false);
                }
                view.remove();
            }).on("cancel", function (e, view) { view.remove(); });
        });
    }

    FileTreeView.prototype.deleteItem = function (file) {
        var path = typeof file === "string" ? file : file.path;
        treeList.removeItem(path);
    };

    /**
        Gets the undlerying treeList object. This would be useful if a direct access to manipulating
        the data in the treeList node is needed. The following calls are available
        selectItem
        addItem
        removeItem
        renameItem
        getSelectedItem
    */
    FileTreeView.prototype.getTreeList = function () {
        return treeList;
    };
    /**
        selects the file passed
    */
    FileTreeView.prototype.selectItem = function (file) {
        if (file || file.path) {
            var path = typeof file === "string" ? file : file.path;
            return treeList.selectItem(path);
        }
        return false;
    };

    /**
     * Renames the selected file to the name specified
     * @param {string} newName The newName to give the file.
     */
    FileTreeView.prototype.renameSelected = function (newName) {
        treeList.renameItem(treeList.getSelectedItem(), newName);
    };

    /**
        Gets the selected file in the treeview
        @returns {String} The full path to the selected file
     */
    FileTreeView.prototype.getSelectedItem = function () {
        var res = treeList.getSelectedItem();
        return (res) ? res.path : undefined;
    };

    /**
        Gets the selected data in the treeview
        @returns {Object({name, path, isDirectory})}
     */
    FileTreeView.prototype.getSelectedData = function () {
        var res = treeList.getSelectedItem();
        return (res) ? { name: res.name, path: res.path, isDirectory: res.isDirectory }
            : { name: project.name(), path: project.name(), isDirectory: true };
    };


    /**
     * Renames the project
     * @param {string} newName The new project name.
     */
    FileTreeView.prototype.renameProject = function (newProjectName) {
        treeList.renameRoot(newProjectName);
    };


    module.exports = FileTreeView;
});
