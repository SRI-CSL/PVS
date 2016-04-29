/**
 *
 * @author Patrick Oladimeji
 * @date 4/27/14 15:54:08 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, d3 */
define(function (require, exports, module) {
    "use strict";
    var listHeight = 28,
        childIndent = 10,
        duration = 200,
        contextMenuItems = ["New File", "New Folder", "Rename", "Delete"];
    var globalId = 0;
    var eventDispatcher = require("util/eventDispatcher");


    /**
        Find the node with the give id
    */
    function find(f, root) {
        if (root) {
            if (f(root)) {
                return root;
            } else if (root.children && root.children.length) {
                var i, child, res;
                for (i = 0; i < root.children.length; i++) {
                    child = root.children[i];
                    res = find(f, root.children[i]);
                    //break out of loop if we have found it
                    if (res) {
                        return res;
                    }
                }
            }
        }
        return null;
    }

    function TreeList(d, _el, _readOnly, _allowMultiSelection) {
        eventDispatcher(this);
        this.el = _el || "body";
        this.data = d;
        this.parentHeight = 380;
        this.treeHeight = this.parentHeight;
        this._selectedData = d;
        this._readOnly = _readOnly;
        this._allowMultiSelection = _allowMultiSelection;
        this.render(this.data);

        if (!this._readOnly) {
            this._installContextMenuHandlers();
        }
    }

     // install handlers for context menu if this is not a readonly list
    //create custom context menu for the list item
    TreeList.prototype._installContextMenuHandlers = function () {
        var self = this,
            el = this.el;
        function createMenu(menuItems, sourceEvent) {
            var div = d3.select("body").append("div").attr("type", "context").attr("class", "contextmenu")
                .style("position", "absolute")
                .style("top", sourceEvent.pageY + "px")
                .style("left", sourceEvent.pageX + "px")
                .style("opacity", "0.98");
            var ul = div.append("ul").style("list-style", "none");

            var menus = ul.selectAll("li.menuitem").data(menuItems).enter()
                .append("li").attr("class", "menuitem").attr("id", function (d) {
                    return d.toLowerCase().replace(/\s/g, "");
                }).html(String);

            menus.on("click", function (d) {
                var selectedData = self.getSelectedItem();
                //we want to rename or delete the actually selected data but we need to add items to the selected data
                //only if the selected item is a directory, if not a directory we want to add to the parent
                var data = ["Rename", "Delete"].indexOf(d) > -1 ? selectedData :
                            selectedData.isDirectory ? selectedData : selectedData.parent;
                var event = {type: d, data: data};
                self.fire(event);
                div.remove();
            });
        }

        if (el && d3.select(el) && d3.select(el).node()) {
            d3.select(el).node().oncontextmenu = function (event) {
                event.preventDefault();
                event.cancelBubble = true;
                if (event.stopPropagation) {
                    event.stopPropagation();
                }
                d3.select("div.contextmenu").remove();
                createMenu(contextMenuItems, event);
                return false;
            };

            document.onclick = function (e) {
                var ctxmenu = d3.select("div.contextmenu");
                if (e.target !== ctxmenu.node() && e.button !== 2) {
                    ctxmenu.remove();
                }
            };
        }
    };
    TreeList.prototype.render = function (parent, noAnimation) {
        var self = this,
            el = this.el;
        function toggleChildren(d) {
            if (d.children) {
                d._children = d.children;
                d.children = null;
            } else {
                d.children = d._children;
                d._children = null;
            }
            if (d.empty) {
                d._empty = d.empty;
                d.empty = null;
            } else if (d._empty) {
                d.empty = d._empty;
                d._empty = null;
            }
        }

        var tree = d3.layout.treelist().childIndent(childIndent).nodeHeight(listHeight);
        var nodes = tree.nodes(self.data);
        var ul = d3.select(el).select("ul");
        if (ul.empty()) {
            ul = d3.select(el).append("ul");
        }
        this.treeHeight = nodes.length * tree.nodeHeight();

        ul.attr("class", "treelist").classed("noselect", true)
            .attr("id", "file_browser")
            .style("height",  this.treeHeight + "px")
            .style("position", "relative")
            .style("list-style", "none");

        var nodeEls = ul.selectAll("li.node").data(nodes, function (d) {
            d.id = d.id || ++globalId;
            return d.path || d.id;
        });

        var enteredNodes = nodeEls.enter()
            .append("li")
            .style("position", "absolute")
            .style("top", parent.y + "px")
            .style("opacity", 0)
            .style("height", tree.nodeHeight() + "px")
            .on("mousedown", function (d) {
                if (d3.event.shiftKey) {
                    d3.select(this).classed("selected", "true");
                } else if (self._selectedData !== d) {
                    var pd = self._selectedData;
                    self._selectedData = d;
                    ul.selectAll("li.node").classed("selected", function (d) {
                        return self._selectedData === d;
                    });
                    d.previousData = pd;
                    var event = {type: "SelectedItemChanged", data: d};
                    // clear all editable flags
                    ul.selectAll("li.node").select(".label").attr("contentEditable", false);
                    self.fire(event);
                }
            })
            .on("dblclick", function (d) {
                toggleChildren(d);
                self.render(d);
            });

        var listWrap = enteredNodes.append("div").classed("line", true);
        var updatedNodes = nodeEls, exitedNodes = nodeEls.exit();
        var chevron = listWrap.append("span").attr("class", function (d) {
            var icon = d.children || d.empty ? " glyphicon-chevron-down"
                : d._children ? "glyphicon-chevron-right"
                : d.isDirectory ? " glyphicon-chevron-right"
                : "";
            return "chevron glyphicon " + icon;
        }).style("height", function (d) {
            return d.isDirectory ? "90%" : "0%";
        }).style("padding", function (d) {
            return d.isDirectory ? "6px 0px 6px 0px" : "0px";
        });
        chevron.on("click", function (d) {
            toggleChildren(d);
            self.render(d);
            if (self._selectedData !== d) {
                var pd = self._selectedData;
                self._selectedData = d;
                ul.selectAll("li.node").classed("selected", function (d) {
                    return self._selectedData === d;
                });
                d.previousData = pd;
                var event = {type: "SelectedItemChanged", data: d};
                // clear all editable flags
                ul.selectAll("li.node").select(".label").attr("contentEditable", false);
                console.log(event);
                self.fire(event);
            }
        });


        //add icons for folder for file
        listWrap.append("span").attr("class", function (d) {
            var icon = d.isDirectory ? "glyphicon-folder-close"
                : "glyphicon-file";
            return "glyphicon " + icon;
        });
        //add text
        listWrap.append("span").attr("class", "label")
            .html(function (d) { return d.name; });

        //update chevron direction and style
        nodeEls.select("span.chevron").attr("class", function (d) {
            var icon = d.children || d.empty ? " glyphicon-chevron-down"
                : d._children ? "glyphicon-chevron-right"
                : d.isDirectory ? " glyphicon-chevron-right"
                : "";
            return "chevron glyphicon " + icon;
        }).style("height", function (d) {
            return d.isDirectory ? "90%" : "0%";
        }).style("padding", function (d) {
            return d.isDirectory ? "6px 0px 6px 0px" : "0px";
        });
        //update list class
        nodeEls.attr("class", function (d, i) {
            var c = i % 2 === 0 ? "node even" : "node odd";
            if (self._selectedData && d.path === self._selectedData.path) {
                c = c.concat(" selected");
            }
            return c;
        }).style("display", "block");

        if (!noAnimation) {
            updatedNodes = updatedNodes.transition().duration(duration);
        }
        updatedNodes.style("top", function (d) {
            return (d.y - tree.nodeHeight()) + "px";
        }).style("opacity", 1);
        updatedNodes.selectAll(".line").style("padding-left", function (d) {
            return d.x + "px";
        });
        //update any label names
        updatedNodes.select("span.label").text(function (d) { return d.name; });
        //remove hidden nodes
        exitedNodes.remove();

        //update width  of ul element so that selected list item spans full width of the ul even when
        //they overflow on the x-axis
        var max = 0;
        nodeEls.each(function (d) {
            var width = d3.select(this).select(".line").node().getBoundingClientRect().width
                + d.x;
            if (width > max) { max = width; }
        });
        ul.style("width", max + "px");
    };

    TreeList.prototype.refreshSelectedItem = function () {
        if (this._selectedData) {
            //fire selected item changed event
            this.fire({type: "SelectedItemChanged", data: this._selectedData});
        }
    };

    TreeList.prototype.selectItem = function (path) {
        var selectedData = this._selectedData, el = this.el;
        if (!selectedData || selectedData.path !== path) {
            var nodes = d3.select(el).selectAll(".node");
            var pd = selectedData;
            nodes.classed("selected", function (d) {
                if (d.path === path) {
                    selectedData = d;
                    return true;
                } else {
                    return false;
                }
            });
            if (selectedData) {
                //fire selected item changed event
                selectedData.previousData = pd;
                //update _selectedData
                this._selectedData = selectedData;
                //fire selected item changed event
                this.fire({type: "SelectedItemChanged", data: selectedData});

//                setTimeout(function () {
//                    if (d3.select(el).node()) {
//                        d3.select(el).node().scrollTop = selectedData.y;
//                    }
//                }, duration);
                return true;
            }
        }
        return false;
    };

    /**
     * given a selected node, this function selects the next in the hierarchy
     * FIXME: implement this function! now we just select the parent node
     */
    TreeList.prototype.selectNext = function (nodePath, index) {
        var selectedData = this._selectedData, el = this.el;
        var nodes = d3.select(el).selectAll(".node");
        var parent = nodePath.substring(0, nodePath.lastIndexOf("/"));
        var path = parent;
        var pd = selectedData;
        nodes.classed("selected", function (d) {
            if (d.path === path) {
                selectedData = d;
                return true;
            } else {
                return false;
            }
        });
        //fire selected item changed event
        selectedData.previousData = pd;
        this.fire({type: "SelectedItemChanged", data: selectedData});

        setTimeout(function () {
            d3.select(el).node().scrollTop = selectedData.y;
        }, duration);
    };

    TreeList.prototype.markDirty = function (path, sign) {
        d3.select(this.el).selectAll(".node")
            .filter(function (d) {
                return d.path === path;
            }).classed("dirty", sign ? true : false);
    };

    /**
        Adds a new file item to the tree.
        @param {object} item the item to add. It should contain the following properties
            !name: <string>
            !path: <string>
            ?isDirectory: <boolean>
            ?children: <array>
        @param {object} parent the parent node for the new item. If this is not specified, the parent is deduced from
        the path of the item to be added.
        @returns the new node item that was added
    */
    TreeList.prototype.addItem = function (item, parent) {
        var self = this;
        if (item.path === item.name) { // it's the root folder, just render it to refresh the view
            this.render(item);
        } else {
            var parentFolderName = item.path.replace("/" + item.name, "");
            parent = parent || find(function (d) {
                return d.path === parentFolderName;
            }, self.data);
            if (!parent) {
                var ancestorPath = parentFolderName.substring(0, parentFolderName.lastIndexOf("/"));
                var ancestorName = parentFolderName.replace(ancestorPath + "/", "");
                parent = this.addItem({name: ancestorName, path: parentFolderName, children: [], isDirectory: true});
            }
            if (!parent.isDirectory) {
                parent = parent.parent;
            }
            parent.children = parent.children || parent._children || [];
            // this is a sanity check --- path must be unique, otherwise we have duplicated elements.
            var duplicate = parent.children.filter(function (elem) { return elem.path === item.path; });
            if (duplicate.length === 0) {
                parent.children.push(item);
                item.parent = parent;
                this.render(parent);
            } else {
                console.log("Warning: attempt to add an existing element in TreeList (" + item.path + ")");
            }
        }
        return item;
    };
    /**
        Removes the item at the specified path from the tree
        @param {string} path
    */
    TreeList.prototype.removeItem = function (path) {
        var selectedData = this._selectedData;
        var self = this;
        var toRemove = find(function (node) {
            return node.path === path;
        }, self.data);
        if (toRemove && toRemove.parent) {
            var index = toRemove.parent.children ? toRemove.parent.children.indexOf(toRemove) : -1;
            if (index > -1) {
                toRemove.parent.children.splice(index, 1);
                this.render(toRemove.parent);
            }
            if (selectedData === toRemove && toRemove.parent) {
                this.selectItem(toRemove.parent.path);
            }
        } else {
            console.log("(TreeList.RemoveItem) Warning: cannot find " + path + " in TreeView.");
        }
    };

    /**
        Removes the item with the specified id from the tree
        @param {string} id
    */
    TreeList.prototype.removeItemByID = function (id) {
        var selectedData = this._selectedData;
        var self = this;
        var toRemove = find(function (node) {
            return node.id === id;
        }, self.data);
        if (toRemove) {
            var index = toRemove.parent.children ? toRemove.parent.children.indexOf(toRemove) : -1;
            if (index > -1) {
                toRemove.parent.children.splice(index, 1);
                this.render(toRemove.parent);
            }

            if (selectedData === toRemove && toRemove.parent) {
                this.selectItem(toRemove.parent.path);
            }
        }
    };

    TreeList.prototype.nodeExists = function (nodePath) {
        var nodes = d3.select(this.el).selectAll(".node .label").filter(function (d) {
            return d.path === nodePath;
        });
        if (!nodes.node()) {
            return false;
        }
        return true;
    };

    TreeList.prototype.createNodeEditor = function (node, onEnter, onCancel) {
        function nodeFinder(d) {
            return function (t) { return t.path === d.path; };
        }
        var self = this;
        var selectedData = this.getSelectedItem();
        selectedData = selectedData ? find(nodeFinder(selectedData), this.data) : selectedData;
        var n = find(nodeFinder(node), this.data) || selectedData;

        var nodes = d3.select(this.el).selectAll(".node").filter(function (d) {
            return d === n;
        });

        // an input text element is temporarily appended to let the user type the label
        var inputText = nodes.select(".label").html("")
            .append("input").attr("class", "input_text")
            .attr("type", "text")
            .attr("placeholder", node.name)
            .attr("value", node.name);

        var oldPath = node.path;

        function doCreate(elem, newLabel) {
            if (newLabel === "") { newLabel = node.name; }
            d3.select(elem.parentNode).html(newLabel);
            self.renameItem(n, newLabel);
        }
        var inputEl = inputText.node();
        inputEl.focus();
        inputEl.onblur = function () {
            // NB: the first thing is to remove the onblur event handler, otherwhise the event will be triggered twice
            inputEl.onblur = null;
            doCreate(inputEl, inputEl.value);
            if (onEnter && typeof onEnter === "function") {
                onEnter(n, oldPath);
            }
        };
        inputEl.onkeydown = function (event) {
            if (event.which === 13) { // enter key pressed
                event.preventDefault();
                inputEl.onkeydown = null;
                inputEl.onblur();
            } else if (event.which === 27) { // escape key pressed
                inputEl.onblur = null;
                event.preventDefault();
                doCreate(inputEl, node.name);
                if (onCancel && typeof onCancel === "function") {
                    onCancel(n, oldPath);
                }
                inputEl.onkeydown = null;
            }
        };
    };

    TreeList.prototype.renameItem = function (item, newName) {
        function updatePath(item, newName) {
            item.path = item.parent ? (item.parent.path + "/" + newName) : newName;
            item.name = newName;
            if (item.children) {
                item.children.forEach(function (c) {
                    updatePath(c, c.name);
                });
            }
        }
        updatePath(item, newName);
        this.render(item);
    };

    TreeList.prototype.getSelectedItem = function () {
        return this.getSelectedItems()[0];
    };

    TreeList.prototype.getSelectedItems = function () {
        var els = d3.select(this.el).selectAll(".selected");
        return els.data();
    };

    TreeList.prototype.renameRoot = function (newName) {
        // we assume here that the first label in the tree is always the project name
        this.renameItem(d3.select(this.el).select(".label").data()[0], newName);
        d3.select(this.el).select(".label").text(newName);
    };

    TreeList.prototype.blur = function () {
        var n = this.getSelectedItem();
        var this_node = d3.select(this.el).selectAll(".node").filter(function (d) {
            return d === n;
        });
        if (this_node && this_node.length > 0) {
            this_node.select(".label").html(n.name);
        }
        return this;
    };

    TreeList.prototype.findNode = function (f) {
        return find(f, this.data);
    };

    module.exports = TreeList;

});
