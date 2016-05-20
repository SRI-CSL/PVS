/**
 * @module ProjectManager
 * @version 1.0
 * @description
 * Project Manager is responsible for operations on projects (e.g., create, remove, open).
 * Currently, it embeds the PVSio-web file browser, so the module includes APIs for files/directories,
 * and selection of elements within the PVSio-web file browser.<br/>
 * This module can be used for the following purposes:
 *   <ul><li>Listen to events such as "ProjectChanged", "SelectedFileChanged"</li>
 *   <li>Open/Create/Delete/Rename projects</li>
 *   <li>Get an instance of the current project</li>
 *   <li>Perform operations across projects (e.g., copy files across projects)</li></ul>
 * @author Patrick Oladimeji, Paolo Masci
 * @date 11/15/13 9:49:03 AM
 *
 * @example <caption>Typical use of ProjectManager APIs within a PVSio-web plugin module.</caption>
 * // Example module that uses ProjectManager.
 * define(function (require, exports, module) {
 *     "use strict";
 *
 *     // get a ProjectManager instance
 *     var pm = require("project/ProjectManager").getInstance();
 *
 *     function main() {
 *         // add event listeners
 *         pm.addListener("SelectedFileChanged", onSelectedFileChanged);
 *         pm.addListener("ProjectChanged", onProjectChanged);
 *
 *         // Perform operations on the current project
 *         pm.project() // this is the current project
 *           .addFile("main.pvs", "%-- file content...", { overWrite: true }).then(function (res) {
 *              //file added successfully, res is the file descriptor
 *              //...
 *           }).catch(function (err) {
 *              //operation failed, err shows the error details
 *              console.log(JSON.stringify(err));
 *              //...
 *           });
 *     }
 * });
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, window, Promise, document*/
define(function (require, exports, module) {
    "use strict";
    var property              = require("util/property"),
        eventDispatcher       = require("util/eventDispatcher"),
        Project               = require("project/Project"),
        FileTreeView          = require("pvsioweb/FileTreeView"),
        Descriptor            = require("project/Descriptor"),
        WSManager             = require("websockets/pvs/WSManager"),
        displayQuestion       = require("pvsioweb/forms/displayQuestion"),
        CreateProjectView     = require("project/forms/CreateProjectView"),
        SaveProjectView       = require("project/forms/SaveProjectView"),
        openProjectForm       = require("pvsioweb/forms/openProject"),
        NotificationManager   = require("project/NotificationManager"),
        Constants             = require("util/Constants"),
        MIME                  = require("util/MIME"),
        PVSioWebClient        = require("PVSioWebClient").getInstance(),
        FileSystem            = require("filesystem/FileSystem");

    var pvsFilesListView; // This is the PVSio-web file browser instance ** TODO: create a separate module **
    var _projectManager; // Project Manager instance ("this" pointer)
    var fs;

    //----------------------------------------------------------------------------------------
    //     These are the APIs of the pvsio-web file browser embedded within ProjectManager
    //----------------------------------------------------------------------------------------
    /**
     * @function onFSUpdate
     * @description Event listener for file system updates
     * @memberof module:ProjectManager
     * @instance
     * @private
     */
    function onFSUpdate(event) {
        function getFolderChildren(files, parentPath) {
            var tree = {};
            files.forEach(function (f) {
                var args = f.path.split("/");
                var ptr = tree;
                args.forEach(function (a) {
                    ptr[a] = ptr[a] || {name: a, children: {}};
                    ptr = ptr[a].children;

                });
            });
            function getChildren(children, parentPath) {
                if (children && Object.keys(children).length) {
                    var res = Object.keys(children).map(function (key) {
                        var child = children[key];
                        child.path = parentPath + "/" + child.name;
                        child.children = getChildren(child.children, child.path);
                        child.isDirectory = child.children !== undefined;
                        return child;
                    });
                    return res;
                }
                return undefined;
            }
            return getChildren(tree, parentPath);
        }
        var f, project = _projectManager.project(), children = [],
            parentFolderName = project.name(), parent = project.name();
        if (event.event === "delete") {
            f = project.getDescriptor(event.path);
            if (f) { // it's a file
                project.removeDescriptor(f);
            } else { // it's a directory
                var affectedFiles = project.getDescriptors().filter(function (f) {
                    return f.path.indexOf(event.path) === 0;
                });
                // remove file descriptors from the project
                affectedFiles.forEach(function (f) {
                    project.removeDescriptor(f);
                });
                // remove the folder from the PVSio-web file browser
                pvsFilesListView.deleteItem(event.path);
            }
        } else if (event.event === "rename" || event.event === "change") { //file or folder added
            if (event.isDirectory && event.name !== "pvsbin") {
                event.subFiles = event.subFiles || [];
                children = getFolderChildren(event.subFiles, event.path);
                if (!pvsFilesListView.getTreeList().nodeExists(event.path)) {
                    parentFolderName = event.path.split("/").slice(0, -1).join("/");
                    parent = pvsFilesListView.getTreeList().findNode(function (d) {
                        return d.path === parentFolderName;
                    });
                    var newFolder = {name: event.name, path: event.path, children: children, isDirectory: true};
                    pvsFilesListView.getTreeList().addItem(newFolder, parent);
                    //add the projectfiles
                    event.subFiles.forEach(function (f) {
                        var pf = new Descriptor(event.path + "/" + f.path);
                        project.addDescriptor(pf, true);
                    });
                }
                // select the folder
                pvsFilesListView.selectItem(event.path);
            } else if (!event.isDirectory) {
                if (!_projectManager.fileDescriptorExists(event.path)) {
                    f = new Descriptor(event.path);
                    if (_projectManager.isImage(event.path)) {
                        f.encoding = "base64";
                    }
                    project.addDescriptor(f);
                    // select the file
                    pvsFilesListView.selectItem(event.path);
                } else {
                    // this file system update is due to a change in the file content
                    // we need to refresh the file
                    f = new Descriptor(event.path);
                    if (_projectManager.isImage(event.path)) {
                        f.encoding = "base64";
                    }
                    // refresh the content cached in the descriptors
                    project.refreshDescriptor(f).then(function (res) {
                        if (res.content) {
                            pvsFilesListView.getTreeList().refreshSelectedItem();
//                            NotificationManager.show("Another application changed " + event.path +
//                                                     ". PVSio-web has reloaded the file content from disk.");
                            console.log("Another application changed " + event.path +
                                                     ". PVSio-web has reloaded the file content from disk.");
                        }
                    }).catch(function (err) { console.log(err); });
                }
                if (f.path === (_projectManager.project().name() + "/" + Constants.widgetDefinitionsFile)) {
                    //fire widgetFileChanged event -- this will trigger refresh of UI widgets
                    var evt = {type: "WidgetsFileChanged", data: f.loadContent()};
                    _projectManager.fire(evt);
                }
            }
        } else if (event.event === "refresh") { //file or folder added
            if (event.isDirectory && event.name !== "pvsbin") {
                children = getFolderChildren(event.subFiles, event.path);
                parentFolderName = event.path.replace(event.name, "");
                parent = pvsFilesListView.getTreeList().findNode(function (d) {
                    return d.path === parentFolderName;
                });
                // refresh the content with the new information
                var folder = {name: event.name, path: event.path, children: children, isDirectory: true};
                pvsFilesListView.getTreeList().addItem(folder, parent);
                //add files
                event.subFiles.forEach(function (f) {
                    var pf = new Descriptor(f.path);
                    project.addDescriptor(pf);
                    pvsFilesListView.getTreeList().addItem({
                        path: pf.path,
                        name: pf.name,
                        isDirectory: false
                    });
                });
                //add subfolders
                event.subFolders.forEach(function (f) {
                    var name = f.path.split("/").slice(-1).join("");
                    if (name !== "pvsbin") {
                        pvsFilesListView.getTreeList().addItem({
                            path: f.path,
                            name: name,
                            isDirectory: true
                        });
                    }
                });
            } else {
                console.log("Warning: unexpected file refresh event sent from PVSio-web server: " + JSON.stringify(event));
            }
        } else {
            console.log("Warning: unexpected event type: " + event.event);
        }
    }
    /**
     * @function renderSourceFileList
     * @description Renders the list of files in the PVSio-web file browser.
     * @memberof module:ProjectManager
     * @instance
     * @private
     */
    function renderSourceFileList() {
        var pm = _projectManager;
        var project = _projectManager.project();
        var folderStructure = project.getFolderStructure();

        pvsFilesListView = new FileTreeView("#pvsFiles", folderStructure, pm);
        pvsFilesListView.addListener("SelectedItemChanged", function (event) {
            //bubble the event
            event.type = "SelectedFileChanged";
            pm.fire(event);
        });
        return pvsFilesListView;
    }

    /**
     * @function <a name="ProjectManager">ProjectManager</a>
     * @description Constructor. This function is private to the module.
     *              To get a ProjectManager instance, please use <a href="#getInstance">getInstance</a>.
     * @param project {Project} The PVSio-web project that shall be managed.
     *        The current implementation of ProjectManager is able to manage one project at a time.
     * @memberof module:ProjectManager
     * @instance
     * @private
     */
    function ProjectManager(project) {
        function registerFSUpdateEvents() {
            WSManager.getWebSocket().removeListener("FileSystemUpdate", onFSUpdate);
            WSManager.getWebSocket().addListener("FileSystemUpdate", onFSUpdate);
        }
        _projectManager = this;
        project = project || new Project(Constants.defaultProjectName);
        /**
         * The current project can be accessed as using _projectManager.project()
         */
        this.project = property.call(this, project).addListener("PropertyChanged", function (e) {
            registerFSUpdateEvents(e.fresh);
        });
        window.onbeforeunload =  function () {
            var p = _projectManager.project();
            if (p && p._dirty()) { return "Are you sure you want to exit? All unsaved changed will be lost."; }
            WSManager.getWebSocket().closePVSProcess();
        };
        renderSourceFileList();
        registerFSUpdateEvents(project);
        eventDispatcher(this);
        document.onkeydown = function (event) {
            // key 83 is 's'
            if (event.ctrlKey && (event.keyCode === 83 || event.which === 83)) {
                event.preventDefault();
                event.stopPropagation();
                _projectManager.saveProject();
                console.log("Project saved!");
            }
        };
        fs = new FileSystem();
        return this;
    }

    /**
     * @function <a name="reconnectToServer">reconnectToServer</a>
     * @description Reestablishes connection to the server without reloading the client.
     * This ensures that unsaved changes are not lost e.g., after server crashes
     * @returns {Promise} a Promise that is settled when the connection to the server has been
     * reestablished or an error occurs
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.reconnectToServer = function () {
        var project = this.project();
        function startPVSProcess(ws) {
            var mainFile = project.mainPVSFile().path.replace(project.name() + "/", "");
            return new Promise(function (resolve, reject) {
                ws.startPVSProcess({name: mainFile, projectName: project.name()}, function (err) {
                    if (err) {
                        reject(err);
                    } else {
                        _projectManager.fire({type: "PVSProcessReady"});
                        resolve(true);
                    }
                });
            });
        }
        return PVSioWebClient.connectToServer()
            .then(startPVSProcess);
    };

    /**
     * @function <a name="renderFileTreeView">renderFileTreeView</a>
     * @description refreshes the fileTreeView
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.renderFileTreeView = function () {
        renderSourceFileList();
    };

    /**
     * @function <hr><a name="getSelectedData">getSelectedData</a>
     * @description Gets the descriptor of the file or directory currently selected within the PVSio-web file browser.
     * @returns {Descriptor} Returns the descriptor of the file that is currently selected within the PVSio-web file browser.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.getSelectedData = function () {
        var data = pvsFilesListView.getSelectedData();
        if (data.isDirectory) {
            return new Descriptor(data.path, null, { isDirectory: true });
        }
        return _projectManager.getSelectedFile();
    };

    /**
     * @function <hr><a name="getSelectedFile">getSelectedFile</a>
     * @description Get the descriptor of the file currently selected within the PVSio-web file browser.
     * @returns {Descriptor} Returns the descriptor of the file that is currently selected within the PVSio-web file browser.
     *          The function returns null if the selected item is a folder.
     *          This function is a variant of <a href="#getSelectedData">getSelectedData</a>
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.getSelectedFile = function () {
        return _projectManager.project().getDescriptor(pvsFilesListView.getSelectedItem());
    };

    /**
     * @function selectFile
     * @description Selects the file specified as argument
     * @param file {String|Descriptor} The file that shall be selected.
     *        <li>If the file is specified as a String, then file is a path relative
     *        to the current project (and the name of the current project folder is included in the path).</li>
     *        <li>If the file is specified as a file descriptor (Descriptor), then the file descriptor
     *        shall be one of those stored in the current project. These descriptors can be retrieved
     *        using ProjectManager.getFileDescriptors().</li>
     * @returns {bool} true if the file was successfully selected, false otherwise
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.selectFile = function (file) {
        // select item in filetree
        if (file) {
            return pvsFilesListView.selectItem(file);
        }
        console.log("Warning: " + file + " parameter passed to PrototypeManager.selectFile(...)");
        return false;
    };


    //----------------------------------------------------------------------------------------
    //     These are the main APIs of ProjectManager
    //----------------------------------------------------------------------------------------
    /**
     * @function <a name="fireProjectChanged">fireProjectChanged</a>
     * @description Fires "ProjectChanged" event, and contextually refreshes the PVSio-web browser user interface.
     * @private
     */
    function fireProjectChanged(event) {
        var project = event.current;
        document.title = "PVSio-Web -- " + event.current;
        d3.select("#header #txtProjectName").html(event.current);        
        renderSourceFileList();
        _projectManager.selectFile(project.mainPVSFile() || project.pvsFilesList()[0] || project.name());
        _projectManager.fire(event);
    }

    /**
     * @function <hr><a name="openProject">openProject</a>
     * @description Load a project from disk.
     * @param projectName The name of the project that shall be opened.
     * @returns {Promise(Project)} A Promise that resolves to the descriptor of the project loaded from disk.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.openProject = function (projectName) {
        return new Promise(function (resolve, reject) {
            if (projectName && typeof projectName === "string" && projectName !== "") {
                WSManager.getWebSocket().send({type: "openProject", name: projectName}, function (err, res) {
                    if (!err) {
                        var descriptors = []; // file descriptors
                        if (res.project.descriptors) {
                            res.project.descriptors.forEach(function (desc) {
                                descriptors.push(new Descriptor(desc.path, desc.content, { encoding: desc.encoding }));
                            });
                        }
                        var p = new Project(res.project.name);
                        p.initFromJSON(descriptors);
                        p._dirty(false);
                        var evt = {type: "ProjectChanged", current: p, previous: _projectManager.project()};
                        _projectManager.project(p);
                        // fire ProjectChanged event
                        fireProjectChanged(evt);
                        resolve(p);
                    } else {
                        reject(err);
                    }
                });
            } else {
                reject({
                    code: "INVALID_ARG",
                    message: "Invalid project name " + projectName
                });
            }
        });
    };

    /**
     * @function <hr><a name="openProjectDialog">openProjectDialog</a>
     * @description Load a project from disk. This is a variant of <a href="#openProject">openProject</a>
     *              designed to show a dialog that shows the list of available projects
     *              and allows the user to select which project shall be opened.
     * @returns {Promise(Project)} A Promise that resolves to the descriptor of the project loaded from disk.
     * @memberof module:ProjectManager
     * @instance
    */
    ProjectManager.prototype.openProjectDialog = function () {
        return new Promise(function (resolve, reject) {
            WSManager.getWebSocket().send({type: "listProjects"}, function (err, res) {
                if (!err) {
                    res.projects = res.projects.filter(function (project) {
                        return project.name !== Constants.autosavePath;
                    });
                    openProjectForm.create(res.projects, function (d) { return d; })
                        .on("ok", function (e, view) {
                            view.remove();
                            var name = e.data.projectName;
                            _projectManager.openProject(name).then(function (res) {
                                resolve(name);
                            }).catch(function (err) { reject(err); });
                        })
                        .on("cancel", function (e, view) {
                            view.remove();
                            reject({
                                code: "CANCELED_BY_USER",
                                message: "Operation cancelled by the user"
                            });
                        });
                } else {
                    reject(err);
                }
            });
        });
    };


    /**
     * @function createProject
     * @description Creates a new Project, that is a new directory in folder "examples/projects"
     * @param data {Object} Object with the following properties
     *    <li> projectName is a String defining the project name. This property is mandatory.</li>
     *    <li> prototypeImage is a FileList object specifying the prototype image. This property is optional and can be left unspecified.</li>
     *    <li> pvsSpec is a FileList object specifying the path of a txt file containing a model.  This property is optional and can be left unspecified.</li>
     *    <li> overWrite is a boolean that indicates whether the directory shall be overwritten with the new content in the case the directory already exists. If overWrite is false and the directory exists, then createProject will reject the request. This property is optional and can be left unspecified. Default is false.</li>
     *    <li> silentMode is a boolean than can be used to hide notifications.  This property is optional and can be left unspecified. Default is false. </li>
     * FIXME: the type of prototypeImage and pvsSpec should be changed into Descriptors. This way both local files and remote files can be passed as function arguments.
     * @returns {Promise(Project)} A Promise that resolves to the descriptor of the created project
     *    The descriptor has the following format:
     *    <li> projectName is a String defining the project name. </li>
     *    <li> prototypeImage is an Array of Strings; each string is the path of an image filename ?? </li>
     *    <li> pvsSpec is an Array of Strings; each string is the path of a txt file containing a model.</li>
     * @memberof ProjectManager
     * @instance
     */
    ProjectManager.prototype.createProject = function (data) {
        return new Promise(function (resolve, reject) {
            var success = true;
            var descriptors = [];
            var pvsiowebJSON = {};
            pvsiowebJSON.version = "2.0";
            function finalise(p) {
                descriptors.push(project.addDescriptor("pvsioweb.json", JSON.stringify(pvsiowebJSON, null, " ")));
                var previous = _projectManager.project();
                p.project.initFromJSON(p.descriptors);
                _projectManager.project(p.project);
                // fire ProjectChanged event
                var evt = { type: "ProjectChanged", current: p.project, previous: previous };
                fireProjectChanged(evt);
                success = success && data.success;
                resolve(p.project);
            }
            // sanity check
            if (!data || !data.projectName) {
                return reject({
                    err: "(ProjectManager.createProject) Illegal argument " + JSON.stringify(data),
                    data: data
                });
            }
            // params are ok
            var opt = {
                overWrite : data.overWrite,
                silentMode: data.silentMode
            };
            var project = new Project(data.projectName);
            return fs.mkDir(data.projectName, opt).then(function (res) {
                if (PVSioWebClient.serverOnLocalhost()) {
                    project.importRemoteFiles(data.pvsSpec).then(function (res) {
                        if (res && res.length > 0) {
                            descriptors = descriptors.concat(res);
                            pvsiowebJSON.mainPVSFile = res[0].path.split("/").slice(1).join("/");
                        }
                        project.importRemoteFiles(data.prototypeImage).then(function (res) {
                            if (res && res.length > 0) {
                                descriptors = descriptors.concat(res);
                                pvsiowebJSON.prototypeImage = res[0].path.split("/").slice(1).join("/");
                            }
                            finalise({ project: project, descriptors: descriptors });
                        });
                    }).catch(function (err) {
                        console.log(err);
                        finalise({ project: project, descriptors: descriptors, success: false });
                    });
                } else {
                    project.importLocalFiles(data.localPVSSpec).then(function (res) {
                        if (res) { descriptors = res; }
                        return project.importLocalFiles(data.localPrototypeImage).then(function (res) {
                            if (res && res.length > 0) {
                                descriptors = descriptors.concat(res);
                                descriptors.push(
                                    project.addDescriptor(
                                        new Descriptor(
                                            "pvsioweb.json",
                                            JSON.stringify({ prototypeImage: res[0].name })
                                        )
                                    )
                                );
                            }
                            finalise(project, descriptors);
                            resolve(project);
                        }).catch(function (err) { finalise(project, descriptors); console.log(err); reject(err); });
                    }).catch(function (err) { finalise(project, descriptors); console.log(err); reject(err); });
                }
            }).catch(function (err) { reject(err); });
        });
    };

    /**
     * @function createProjectDialog
     * @description Shows a dialog that allows users to enter the details of a new project.
     * @returns {Promise(Project)} A Promise that resolves to the descriptor of the created project.
     *    The descriptor has the following format:
     *    <li> projectName is a String defining the project name. </li>
     *    <li> prototypeImage is an Array of Strings; each string is the path of an image filename ?? </li>
     *    <li> pvsSpec is an Array of Strings; each string is the path of a txt file containing a model.</li>
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.createProjectDialog = function () {
        return new Promise(function (resolve, reject) {
            WSManager.getWebSocket().send({type: "listProjects"}, function (err, res) {
                if (err) { console.log(err); }
                var projects = [];
                res.projects.forEach(function (project) {
                    projects.push(project.name);
                });
                var defaultName = "prototype-" + new Date().toISOString().split("T")[0];
                CreateProjectView.create({ projectName: defaultName }).on("cancel", function (e, formView) {
                    formView.remove();
                    reject({
                        code: "CANCELED_BY_USER",
                        message: "Operation cancelled by the user"
                    });
                }).on("ok", function (e, formView) {
                    if (projects.indexOf(e.data.projectName) >= 0) {
                        displayQuestion.create({question: "Project " + e.data.projectName +
                                                " already exists. Overwrite project?"})
                            .on("ok", function (ans, view) {
                                e.data.overWrite = true;
                                _projectManager.createProject(e.data).then(function (res) {
                                    resolve(res);
                                }).catch(function (err) { reject(err); });
                                view.remove();
                                formView.remove();
                            }).on("cancel", function (ans, view) {
                                reject({
                                    code: "CANCELED_BY_USER",
                                    message: "Operation cancelled by the user"
                                });
                                view.remove();
                            });
                    } else {
                        formView.remove();
                        return _projectManager.createProject(e.data).then(function (res) {
                            resolve(res);
                        }).catch(function (err) { reject(err); });
                    }
                });
            });
        });
    };

    /**
     * @function <a name="saveProject">saveProject</a>
     * @description Saves the current project on disk.
     * @returns {Promise(Project)} A Promise that resolves to the saved project.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.saveProject = function () {
        return new Promise(function (resolve, reject) {
            _projectManager.project().saveProject().then(function (res) {
                _projectManager.fire({type: "ProjectSaved", project: _projectManager.project()});
                var notification = "Project " + _projectManager.project().name() + " saved successfully!";
                NotificationManager.show(notification);
                resolve(res);
            }).catch(function (err) {
                var msg = "There was an error saving the project: " + JSON.stringify(err);
                NotificationManager.error(msg);
                reject(err);
            });
        });
    };

    /**
     * @function <a name="saveProjectDialog">saveProjectDialog</a>
     * @description This function is a variant of <a href="#saveProject">saveProject</a>
     *              that shows a dialog that allows users to enter the details of the project that shall be saved.
     * @param projectName {?String} Default name for the project. If this parameter is non-null,
     *                             the name is shown in the dialog presented to the user.
     * @returns {Promise(Project)} A Promise that resolves to the descriptor of the saved project.
     *    The descriptor has the following format:
     *    <li> projectName is a String defining the project name. </li>
     *    <li> prototypeImage is an Array of Strings; each string is the path of an image filename ?? </li>
     *    <li> pvsSpec is an Array of Strings; each string is the path of a txt file containing a model.</li>
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.saveProjectDialog = function (projectName) {
        return new Promise(function (resolve, reject) {
            function saveProject(newName) {
                _projectManager.saveProject().then(function (res) {
                    var oldName = _projectManager.project().name();
                    res.renameProject(newName, function (err, res) {
                        if (err) {
                            // alert user
                            if (err.message === "ENOTEMPTY") {
                                alert("Error: the folder could not be renamed into " +
                                      err.newPath + " (another folder with the same name already exists)." +
                                      " Please choose a different name");
                            } else { alert(err.message); }
                            reject(err);
                        } else {
                            _projectManager.project(res);
                            //fire project changed event -- this will trigger updates in the UI
                            var evt = {
                                type: "ProjectChanged",
                                current: _projectManager.project(),
                                previous: oldName
                            };
                            fireProjectChanged(evt);
                            resolve(_projectManager.project());
                        }
                    });
                }).catch(function (err) { reject(err); });
            }
            WSManager.getWebSocket().send({type: "listProjects"}, function (err, res) {
                var projects = [];
                if (!err) {
                    res.projects.forEach(function (project) {
                        projects.push(project.name);
                    });
                } else {
                    console.log(err);
                }
                SaveProjectView.create(projectName).on("cancel", function (e, formView) {
                    formView.remove();
                    reject({
                        code: "CANCELED_BY_USER",
                        message: "Operation cancelled by the user"
                    });
                }).on("ok", function (e, formView) {
                    if (projects.indexOf(e.data.projectName) >= 0) {
                        displayQuestion.create({question: "Project " + e.data.projectName +
                                                " already exists. Overwrite project?"})
                            .on("ok", function (e, view) {
                                e.data.overWrite = true;
                                // remove existing folder, save project and then rename folder
                                fs.rmDir(e.data.projectName).then(function (res) {
                                    saveProject(e.data.projectName);
                                }).catch(function (err) {
                                    console.log("Warning: Error while trying to remove project folder. " + err);
                                    saveProject(e.data.projectName);
                                });
                                view.remove();
                            }).on("cancel", function (e, view) {
                                reject({
                                    code: "EEXISTS",
                                    message: "Operation cancelled by the user"
                                });
                            });
                    } else {
                        saveProject(e.data.projectName);
                    }
                    formView.remove();
                });
            });
        });
    };

    /**
     * @function <a name="backupProject">backupProject</a>
     * @description Creates a backup copy of the project files. The file content used for the backup
     *              always reflects the current content shown in the PVSio-web user interface, that is
     *              even unsaved changed are saved.
     * @param cloneName {String} The name of the backup folder where the backup copy of the project files
     *                           shall be copied in.
     * @returns {Promise(Project)} A Promise that resolves to the backup folder name.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.backupProject = function (backupName, opt) {
        backupName = backupName || (_projectManager.project().name() + "_backup");
        return new Promise(function (resolve, reject) {
            var promises = [];
            var descriptors = _projectManager.project().getDescriptors();
            if (descriptors) {
                descriptors.forEach(function (descriptor) {
                    var path = descriptor.path.replace(_projectManager.project().name(), backupName);
                    promises.push(
                        new Promise(function (resolve, reject) {
                            descriptor.getContent().then(function (res) {
                                var options = opt || {};
                                options.encoding = descriptor.encoding;
                                fs.writeFile(path, descriptor.content, options).then(function (res) {
                                    resolve(res);
                                }).catch(function (err) { reject(err); });
                            }).catch(function (err) { reject(err); });
                        })
                    );
                });
            }
            Promise.all(promises).then(function (res) {
                resolve(backupName);
            }).catch(function (err) { console.log(err); reject(err); });
        });
    };

    /**
     * @function <hr><a name="deleteProject">deleteProject</a>
     * @description Deletes a project: its directory and all its files.
     * @param projectName {!String} Name of the project that shall be deleted.
     *              The provided name is relative to the project folder (pvsio-web/examples/projects/).
     * @return {Promise} A Promise that resolves to the name of the deleted project.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.deleteProject = function (projectName) {
        return fs.rmDir(projectName);
    };

    /**
     * @function <hr><a name="deleteProjectDialog">deleteProjectDialog</a>
     * @description Deletes a project: its directory and all its files. This is a variant of <a href="#rmDir">rmDir</a> designed
     *              to show a dialog to ask confirmation before deleting the project.
     * @param projectName {!String} Name of the project that shall be deleted.
     *              The provided name is relative to the project folder pvsio-web/examples/projects/.
     * @return {Promise} A Promise that resolves to the name of the deleted project.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.deleteProjectDialog = function (projectName) {
        return fs.rmDirDialog(projectName);
    };

    /**
     * @function getFileDescriptors
     * @description Returns the file descriptors stored in the currect project.
     * @returns {Array(Descriptor)}
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.getFileDescriptors = function () {
        return _projectManager.project().getDescriptors();
    };


    /**
     * @function fileExists
     * @description Checks whether a file exists on disk in the current project folder.
     * @param file {Descriptor|String} The file path that needs to be checked.
     *        The file path can be specified as a String, or as a file descriptor (Descriptor).
     *        The path is relative to the current project.
     * returns {Promise} A Promise that resolves to true if pf exists, otherwise returns false
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.fileExists = function (file) {
        return _projectManager.project().fileExists(file);
    };

    /**
     * @function fileDescriptorExists
     * @description Check whether the file descriptor passes as argument is stored in the current project.
     *              File descriptors are uniquely identified using the String specified in property path.
     * @param descriptor {Descriptor|String} The file descriptor whose existence shall be checked within the current project.
     * @returs {bool} true if the descriptor exists, false otherwise
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.fileDescriptorExists = function (descriptor) {
        return _projectManager.project().fileDescriptorExists(descriptor);
    };

    /**
     * @function restartPVSioWeb
     * @description Restarts the PVSio-wev process with the current project. The callback is invoked when the process is ready.
     * @param cb {ProjectManager~pvsProcessReady} The callback function that shall be invoked wht the process is ready.
     * @memberof module:ProjectManager
     * @instance
    */
    ProjectManager.prototype.restartPVSioWeb = function (cb) {
        function noop() {}
        cb = cb || noop;
        var project = _projectManager.project(),
            ws = WSManager.getWebSocket();
        if (project && project.mainPVSFile()) {
            ws.lastState("init(0)");
            ws.startPVSProcess({
                name: project.mainPVSFile().name,
                projectName: project.name()
            }, cb);
        }
        return _projectManager;
    };

    /**
     * @function createDefaultProject
     * @description Creates a new default Project and its folder on disk.
     *              This is a variant of <a href="#createProject">createProject</a>: the project name is pre-defined, and the content of the project folder is restored (i.e., all files are loaded in to the project) if the project folder already exists.
     * @returns {Promise(Project)} Promise that resolves to the created project.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.createDefaultProject = function () {
        return new Promise(function (resolve, reject) {
            function finalise(project) {
                //save the new project in project manager
                _projectManager.project(project);
                //fire project changed event -- this will trigger updates in the UI
                var evt = {type: "ProjectChanged", current: project, previous: (_projectManager.project() || project)};
                fireProjectChanged(evt);
                //resolve the Promise
                resolve(project);
            }
            _projectManager.createProject({
                projectName: Constants.defaultProjectName,
                overWrite: false, // we don't delete the content of the folder, but re-use it.
                silentMode: true
            }).then(function (res) {
                resolve(res);
            }).catch(function (err) {
                if (err.code === "EEXIST") {
                    finalise(new Project(Constants.defaultProjectName));
                } else {
                    reject(err);
                }
            });
        });
    };

    /**
     * @function <hr><a name="newFileDescriptor">newFileDescriptor</a>
     * @description Creates a new file descriptor.
     * @param data {Object({name: String, content: String,
     *              encoding: String, opt: {Object}})}.
     *    <li> name (String) represent the name of the file.
     *         The file name can be prefixed with a path.
     *         This property is mandatory.</li>
     *    <li> content (String) represents the content of the file.
     *         This property is optional.</li>
     *    <li> encoding (String) specified the file encoding (utf8 or base64).
     *         This property is optional. If unspecified, encoding utf8 is used.</li>
     * @returns {Descriptor} The created file descriptor. The file descriptor has the following properties:
     *    <li>path {String} The path of the file</li>
     *    <li>content {String} The content of the file</li>
     *    <li>encoding: {String} The encoding of the file</li>
     * Note: the returned descriptor is null when an error occurs in the creation of the file descriptor.
     * @memberof module:ProjectManager
     * @instance
     */
    ProjectManager.prototype.newFileDescriptor = function (data) {
        return _projectManager.project().newFileDescriptor(data);
    };


    ProjectManager.prototype.getSupportedImageExtensions = function () {
        return MIME.getInstance().getImageExts;
    };
    ProjectManager.prototype.isImage = function (name) {
        return MIME.getInstance().isImage(name);
    };


    /**
     * @function <hr><a name="getInstance">getInstance</a>
     * @description Get a ProjectManager instance. The current implementation uses one instance all the time.
     * @param project {Project} The PVSio-web project that shall be managed.
     *        The current implementation of Project Manager manages one project at a time.
     * @returns {ProjectManager} Get the active ProjectManager instance. The instance can fire the following events:
     *     <ul><li>"ProjectChanged"</li>
     *     <li>"ProjectSaved"</li>
     *     <li>"SelectedFileChanged"</li>
     *     <li>"WidgetsFileChanged"</li></ul>
     * @memberof module:ProjectManager
     * @instance
     */
    module.exports = {
        getInstance: function () {
            _projectManager = _projectManager || new ProjectManager();
            return _projectManager;
        }
    };
/**
 * @callback ProjectManager~onProjectSaved
 * @param {object} err This value is set if any error occurs during the save operation.
 * @param {Project} project The project saved.
 */
/**
 * @callback ProjectManager~onFileDeleted
 * @param {object} err
 * @param {string} success
 */
/**
 * @callback ProjectManager~pvsProcessReady
 * @param {object} err This value is set if there is an error on the server (e.g. process failed to start)
 * @param {object} data This contains the console output when a pvsprocess starts
 */
});
