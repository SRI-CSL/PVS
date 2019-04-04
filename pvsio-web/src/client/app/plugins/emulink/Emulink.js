/**
 *
 * @author Paolo Masci
 * @date 25/05/14 6:39:02 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true*/
/*global define, Promise, d3*/
define(function (require, exports, module) {
    "use strict";
    const normalize = require("util/Normalize").getInstance();
    var ProjectManager		= require("project/ProjectManager"),
        WSManager           = require("websockets/pvs/WSManager"),
        PluginManager       = require("plugins/PluginManager"),
        ModelEditor         = require("plugins/modelEditor/ModelEditor"),
        PVSioWebClient      = require("PVSioWebClient"),
        EmuchartsEditorModes= require("plugins/emulink/EmuchartsEditorModes"),
        // Emucharts           = require("plugins/emulink/Emucharts"),

        displayDelete          = require("plugins/emulink/forms/displayDelete"),
//        displayAddExpression   = require("plugins/emulink/forms/displayAddExpression"),
        QuestionForm            = require("pvsioweb/forms/displayQuestion"),
        SaveAsView              = require("project/forms/SaveAsView"),

        emuchartsSelector       = require("plugins/emulink/tools/EmuchartsSelector").getInstance(),
        emuchartsTables         = require("plugins/emulink/tools/EmuchartsTables").getInstance(),
        EmuchartsCodeGenerators = require("plugins/emulink/models/EmuchartsCodeGenerators"),
        ConsistencyTemplateView = require("plugins/emulink/tools/propertytemplates/ConsistencyTemplateView"),
        FeedbackTemplateView    = require("plugins/emulink/tools/propertytemplates/FeedbackTemplateView"),
        ReversibilityTemplateView = require("plugins/emulink/tools/propertytemplates/ReversibilityTemplateView"),
//        EmuchartsTextEditor    = require("plugins/emulink/EmuchartsTextEditor"),

        contextMenus           = require("plugins/emulink/menus/ContextMenus").getInstance(),

        EmuchartsParser        = require("plugins/emulink/EmuchartsParser"),
        pvs_theory             = require("text!./tools/propertytemplates/pvs_theory.handlebars"),
        pvs_transition_system  = require("text!./tools/propertytemplates/pvs_transition_system.handlebars"),
        pvs_guard              = require("text!./tools/propertytemplates/pvs_guard.handlebars"),

        FileHandler            = require("filesystem/FileHandler"),
        fs                     = require("filesystem/FileSystem").getInstance(),
        PimTestGenerator       = require("plugins/emulink/models/pim/PIMTestGenerator"),
        PMTextGenerator        = require("plugins/emulink/models/pim/PMTextGenerator"),
        PIMImporter            = require("plugins/emulink/models/pim/PIMImporter"),

        emuchartsManager       = require("plugins/emulink/EmuchartsManager").getInstance(),
        ExportDiagram          = require("plugins/emulink/tools/ExportDiagram");

    var instance;
    var projectManager;
    var editor;
    var ws;
    var pvsioWebClient;
    var canvas;

    var MODE;
    var emuchartsCodeGenerators;

    var pimImporter;
    var pimTestGenerator;

    var exportDiagram;

    var options = { autoinit: true };

    function initToolbars() {
        // make sure the svg is visible
        d3.select("#EmuchartLogo").classed("hidden", true);
        d3.select("#graphicalEditor").classed("hidden", false);
        // reset toolbar color
        if (document.getElementById("btn_toolbarBrowse")) {
            document.getElementById("btn_toolbarBrowse").style.background = "black";
        }
        if (document.getElementById("btn_toolbarAddState")) {
            document.getElementById("btn_toolbarAddState").style.background = "black";
        }
        if (document.getElementById("btn_toolbarAddTransition")){
            document.getElementById("btn_toolbarAddTransition").style.background = "black";
        }
        if (document.getElementById("btn_toolbarRename")) {
            document.getElementById("btn_toolbarRename").style.background = "black";
        }
        if (document.getElementById("btn_toolbarDelete")) {
            document.getElementById("btn_toolbarDelete").style.background = "black";
        }
    }

    function modeChange_callback(event) {
/*        var EmuchartsEditorMode = document.getElementById("EmuchartsEditorMode");
        if (EmuchartsEditorMode) {
            if (event.mode === MODE.BROWSE()) {
                EmuchartsEditorMode.style.background = "green";
            } else {
                EmuchartsEditorMode.style.background = "steelblue";
            }
            EmuchartsEditorMode.textContent = "Editor mode: " + MODE.mode2string(event.mode);
        }
        var infoBox = document.getElementById("infoBox");
        if (infoBox) {
            infoBox.value = MODE.modeTooltip(event.mode);
        }*/
    }

    function addState_handler(evt) {
        var stateID = emuchartsManager.getFreshStateName();
        var position = { x: evt.mouse[0], y: evt.mouse[1] };
        emuchartsManager.add_state(stateID, position);
        if (options.autoinit && emuchartsManager.getStates().length === 1) {
            var newTransitionName = emuchartsManager.getFreshInitialTransitionName();
            emuchartsManager.add_initial_transition(newTransitionName, stateID);
        }
    }

    function deleteTransition_handler(event) {
        var transitionID = event.edge.id;
        emuchartsManager.delete_transition(transitionID);
    }

    function deleteInitialTransition_handler(event) {
        var transitionID = event.edge.id;
        emuchartsManager.delete_initial_transition(transitionID);
    }

    function deleteState_handler(event) {
        var stateID = event.node.id;
        emuchartsManager.delete_state(stateID);
    }

    function renameState_handler(event) {
        var theState = emuchartsManager.getState(event.node.id);
        emuchartsTables.edit({ machinestate: theState });
    }
    function changeStateColor_handler(event) {
        var theState = emuchartsManager.getState(event.node.id);
        emuchartsTables.set({ machinestate_color: theState });
    }

    function renameTransition_handler(event) {
        emuchartsTables.edit({ transition: event.edge });
    }

    function highlightTransition_handler(event) {
        emuchartsTables.highlight({ transition: event.edge.id });
    }
    function selectTransition_handler(event) {
        emuchartsTables.highlight({ transition: event.edge.id });
    }
    function deselectTransition_handler(event) {
        emuchartsTables.deselect({ transition: event.edge.id });
    }

    function renameInitialTransition_handler(event) {
        emuchartsTables.edit({ initialtransition: event.edge });
    }

    function addTransition_handler(event) {
        var newTransitionName = emuchartsManager.getFreshTransitionName();
        emuchartsManager.add_transition(newTransitionName,
                                        event.source.id,
                                        event.target.id);
    }

    function addInitialTransition_handler(event) {
        var newTransitionName = emuchartsManager.getFreshInitialTransitionName();
        emuchartsManager.add_initial_transition(newTransitionName, event.target.id);
    }

    function stateAdded_handler(event) {
        emuchartsTables.set({ machinestates: emuchartsManager.getStates() });
    }
    function stateRemoved_handler(event) {
        emuchartsTables.set({ machinestates: emuchartsManager.getStates() });
    }
    function stateRenamed_handler(event) { }//print_theory(); print_node(); }
    function stateColorChanged_handler(event) { }//print_theory(); print_node(); }
    function transitionAdded_handler(event) {
        emuchartsTables.set({ transitions: emuchartsManager.getTransitions() });
    }
    function transitionRemoved_handler(event) {
        emuchartsTables.set({ transitions: emuchartsManager.getTransitions() });
    }
    function transitionRenamed_handler(event) { }//print_theory(); print_node(); }
    function initialTransitionAdded_handler(event) { }//console.log("initial transition added"); }//print_theory(); print_node(); }
    function initialTransitionRemoved_handler(event) { }//console.log("initial transition removed"); }//print_theory(); print_node(); }
    function initialTransitionRenamed_handler(event) { }//console.log("initial transition renamed"); }//print_theory(); print_node(); }
    function constantAdded_handler(event) {
        emuchartsTables.set({ constants: emuchartsManager.getConstants() });
    }
    function constantRemoved_handler(event) {
        emuchartsTables.set({ constants: emuchartsManager.getConstants() });
    }
    function datatypeAdded_handler(event) {
        emuchartsTables.set({ datatypes: emuchartsManager.getDatatypes() });
    }
    function datatypeRemoved_handler(event) {
        emuchartsTables.set({ datatypes: emuchartsManager.getDatatypes() });
    }
    function variableAdded_handler(event) {
        emuchartsTables.set({ statevariables: emuchartsManager.getVariables() });
    }
    function variableRemoved_handler(event) {
        emuchartsTables.set({ statevariables: emuchartsManager.getVariables() });
    }

    function updateContextTables() {
        emuchartsTables.set({
            statevariables: emuchartsManager.getVariables(),
            machinestates: emuchartsManager.getStates(),
            transitions: emuchartsManager.getTransitions(),
            constants: emuchartsManager.getConstants(),
            datatypes: emuchartsManager.getDatatypes()
        });
    }

    /**
     * Constructor
     * @memberof Emulink
     */
    function Emulink() {
        pvsioWebClient = PVSioWebClient.getInstance();
        emuchartsCodeGenerators = EmuchartsCodeGenerators.getInstance();
        MODE = new EmuchartsEditorModes();
        emuchartsManager.addListener("emuCharts_editorModeChanged", modeChange_callback);
        emuchartsManager.addListener("emuCharts_addState", addState_handler);
//        emuchartsManager.addListener("emuCharts_d3ZoomTranslate", d3ZoomTranslate_handler);
        emuchartsManager.addListener("emuCharts_deleteTransition", deleteTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteInitialTransition", deleteInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_deleteState", deleteState_handler);
        emuchartsManager.addListener("emuCharts_renameState", renameState_handler);
        emuchartsManager.addListener("emuCharts_changeStateColor", changeStateColor_handler);
        emuchartsManager.addListener("emuCharts_renameTransition", renameTransition_handler);
        emuchartsManager.addListener("emuCharts_highlightTransition", highlightTransition_handler);
        emuchartsManager.addListener("emuCharts_selectTransition", selectTransition_handler);
        emuchartsManager.addListener("emuCharts_deselectTransition", deselectTransition_handler);
        emuchartsManager.addListener("emuCharts_renameInitialTransition", renameInitialTransition_handler);
        emuchartsManager.addListener("emuCharts_addTransition", addTransition_handler);
        emuchartsManager.addListener("emuCharts_addInitialTransition", addInitialTransition_handler);

        emuchartsManager.addListener("emuCharts_stateAdded", stateAdded_handler);
        emuchartsManager.addListener("emuCharts_stateRemoved", stateRemoved_handler);
        emuchartsManager.addListener("emuCharts_constantAdded", constantAdded_handler);
        emuchartsManager.addListener("emuCharts_constantRemoved", constantRemoved_handler);
        emuchartsManager.addListener("emuCharts_datatypeAdded", datatypeAdded_handler);
        emuchartsManager.addListener("emuCharts_datatypeRemoved", datatypeRemoved_handler);
        emuchartsManager.addListener("emuCharts_variableAdded", variableAdded_handler);
        emuchartsManager.addListener("emuCharts_variableRemoved", variableRemoved_handler);
        emuchartsManager.addListener("emuCharts_transitionAdded", transitionAdded_handler);
        emuchartsManager.addListener("emuCharts_transitionRenamed", transitionRenamed_handler);
        emuchartsManager.addListener("emuCharts_transitionRemoved", transitionRemoved_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionAdded", initialTransitionAdded_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionRenamed", initialTransitionRenamed_handler);
        emuchartsManager.addListener("emuCharts_initialTransitionRemoved", initialTransitionRemoved_handler);
        emuchartsManager.addListener("emuCharts_stateRenamed", stateRenamed_handler);
        emuchartsManager.addListener("emuCharts_stateColorChanged", stateColorChanged_handler);
        emuchartsManager.addListener("emuCharts_newEmuchartsLoaded", function (event) {
            updateContextTables();
        });

        emuchartsSelector.addListener("EmuchartsSelector_select", function (event) {
            if (event && event.emuchart) {
                emuchartsManager.loadEmucharts(event.emuchart.id);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                // update tables
                updateContextTables();
            }
        });

        // PIM objects.
        pimImporter = new PIMImporter();

        pimTestGenerator = new PimTestGenerator("pim_Test_Gen");

        exportDiagram = new ExportDiagram();
        return this;
    }

    let name = "Emucharts Editor";

    Emulink.prototype.getName = function () {
        return name;
    };

    Emulink.prototype.getId = function () {
        return normalize.removeSpaceDash(name);
    };


    function delete_transition() {
        document.getElementById("menuTransitions").children[1].style.display = "none";
        var transitions = emuchartsManager.getTransitions();
        var initialTransitions = emuchartsManager.getInitialTransitions();
        initialTransitions.forEach(function (it) {
            transitions.push(it);
        });
        var labels = [];
        transitions.forEach(function (transition) {
            if (transition.source) {
                labels.push(transition.name + "  ("
                            + transition.source.name + "->"
                            + transition.target.name + ")");
            } else {
                labels.push(transition.name + "  ("
                            + "INIT" + "->"
                            + transition.target.name + ")");
            }
        });
        displayDelete.create({
            header: "Please select trigger to be deleted...",
            textLabel: "Transition to be deleted",
            currentLabels: labels,
            buttons: ["Cancel", "Delete"]
        }).on("delete", function (e, view) {
            var t = e.data.options.get("currentLabel");
            var transitionID = transitions[t].id;
            emuchartsManager.delete_transition(transitionID);
            emuchartsManager.delete_initial_transition(transitionID);
            view.remove();
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    }
    function delete_mode() {
        document.getElementById("menuStates").children[1].style.display = "none";
        var states = emuchartsManager.getStates();
        var labels = [];
        states.forEach(function (state) {
            labels.push(state.name + "  (id: " + state.id + ")");
        });
        displayDelete.create({
            header: "Please select state to be deleted...",
            textLabel: "State to be deleted",
            currentLabels: labels,
            buttons: ["Cancel", "Delete"]
        }).on("delete", function (e, view) {
            var s = e.data.options.get("currentLabel");
            var stateID = states[s].id;
            emuchartsManager.delete_state(stateID);
            view.remove();
        }).on("cancel", function (e, view) {
            // just remove rename window
            view.remove();
        });
    }

    Emulink.prototype.browseMode = function () {
        return this.changeMode(MODE.BROWSE());
    };

    Emulink.prototype.changeMode = function (mode) {
        initToolbars();
        emuchartsManager.set_editor_mode(mode);
        switch (mode) {
            case MODE.BROWSE(): {
                if (d3.select("#btn_toolbarBrowse").node()) {
                    d3.select("#btn_toolbarBrowse").node().style.background = "green";
                }
                break;
            }
            case MODE.DELETE(): {
                if (d3.select("#btn_toolbarDelete").node()) {
                    d3.select("#btn_toolbarDelete").node().style.background = "steelblue";
                }
                break;
            }
            case MODE.RENAME(): {
                if (d3.select("#btn_toolbarRename").node()) {
                    d3.select("#btn_toolbarRename").node().style.background = "steelblue";
                }
                break;
            }
            case MODE.ADD_TRANSITION(): {
                if (d3.select("#btn_toolbarAddTransition").node()) {
                    d3.select("#btn_toolbarAddTransition").node().style.background = "steelblue";
                }
                break;
            }
            case MODE.ADD_STATE(): {
                if (d3.select("#btn_toolbarAddState").node()) {
                    d3.select("#btn_toolbarAddState").node().style.background = "steelblue";
                }
                break;
            }
        }
        return this;
    };

    Emulink.prototype.saveAllCharts = function () {
        if (emuchartsManager) {
            return emuchartsManager.saveAllCharts();
        }
        return Promise.resolve();
    };

    Emulink.prototype.saveChart = function () {
        if (emuchartsManager) {
            return emuchartsManager.saveChart();
        }
        return Promise.resolve();
    };

    Emulink.prototype.createHtmlElements = function () {
        var _this = this;
        var content = require("text!plugins/emulink/forms/maincontent.handlebars");
        canvas = pvsioWebClient.createCollapsiblePanel({
            headerText: _this.getName(),
            showContent: true,
            owner: _this.getId()
        });
        canvas = canvas.html(content);

        emuchartsTables.createHtmlElements();

        function restartEditor() {
            // set initial editor mode
            _this.changeMode(MODE.BROWSE());
            // render emuchart
            emuchartsManager.render({ keep_transformations: true });
            // update tables
            updateContextTables();
        }

        d3.select("#btnLoadEmuchart").on("click", function () {
            emuchartsManager.openChart().then(function (res) {
                emuchartsManager.saveChart();
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                restartEditor();
            }).catch(function (err) {
                console.log(err);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                restartEditor();
            });
        });

        // toolbar
        d3.select("#btn_toolbarAddState").on("click", function () {
            _this.changeMode(MODE.ADD_STATE());
        });
        d3.select("#btn_toolbarAddTransition").on("click", function () {
            _this.changeMode(MODE.ADD_TRANSITION());
        });
        d3.select("#btn_toolbarRename").on("click", function () {
            _this.changeMode(MODE.RENAME());
        });
        d3.select("#btn_toolbarDelete").on("click", function () {
            _this.changeMode(MODE.DELETE());
        });
        d3.select("#btn_toolbarBrowse").on("click", function () {
            _this.changeMode(MODE.BROWSE());
        });
        d3.select("#btn_toolbarZoomIn").on("click", function () {
            emuchartsManager.zoom_in();
        });
        d3.select("#btn_toolbarZoomOut").on("click", function () {
            emuchartsManager.zoom_out();
        });
        d3.select("#btn_toolbarZoomReset").on("click", function () {
            emuchartsManager.zoom_reset();
        });
        // bootstrap tooltip
        $('[data-toggle="tooltip"]').tooltip();


        //-- Emuchart menu -----------------------------------------------------------
        contextMenus.createHtmlElements();
        contextMenus.addListener("ContextMenus.editVariable", function (evt) {
            if (evt && evt.variable) {
                var theVariable = emuchartsManager.getVariable(evt.variable.id);
                emuchartsTables.edit({ statevariable: theVariable });
            }
        });
        contextMenus.addListener("ContextMenus.editConstant", function (evt) {
            if (evt && evt.constant) {
                var theConstant = emuchartsManager.getConstant(evt.constant.id);
                emuchartsTables.edit({ constant: theConstant });
            }
        });
        contextMenus.addListener("ContextMenus.editDatatype", function (evt) {
            if (evt && evt.datatype) {
                var theDatatype = emuchartsManager.getDatatype(evt.datatype.id);
                emuchartsTables.edit({ datatype: theDatatype });
            }
        });

        d3.select("#btn_menuNewChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            var name = emuchartsManager.uniqueEmuchartsID();
            SaveAsView.create({
                heading: "New Emucharts...",
                placeholder: "Please enter Emucharts name...",
                label: "Emucharts name",
                name: name
            }).on("cancel", function (e, formView) {
                formView.remove();
            }).on("ok", function (e, formView) {
                emuchartsManager.newChart(e.data.name);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                restartEditor();
                formView.remove();
            });

        });
        d3.select("#menuEmuchart").on("mouseover", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "block";
        });
        d3.select("#btn_menuCloseChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            if (!emuchartsManager.empty_chart()) {
                // we need to delete the current chart because we handle one chart at the moment
                QuestionForm.create({
                    header: "Warning: the current chart has unsaved changes.",
                    question: "The current chart has unsaved changes that will be lost. Confirm Close?",
                    buttons: ["Cancel", "Confirm close"]
                }).on("ok", function (e, view) {
                    emuchartsManager.delete_chart();
                    d3.select("#btn_toolbarBrowse").node().click();
                    view.remove();
                }).on("cancel", function (e, view) {
                    view.remove();
                });
            }
        });
        d3.select("#btn_menuOpenChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            emuchartsManager.openChart().then(function (res) {
                emuchartsManager.saveChart();
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                restartEditor();
            }).catch(function (err) {
                console.log(err);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                restartEditor();
            });
        });

        emuchartsManager.addListener("EmuchartsManager.saveChart", function (evt) {
            emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
            restartEditor();
        });

        d3.select("#btn_menuSaveChart").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            emuchartsManager.saveChart();
        });
        d3.select("#btn_menuSaveAllCharts").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            emuchartsManager.saveAllCharts();
        });
        d3.select("#btn_menuSaveChartAs").on("click", function () {
            document.getElementById("menuEmuchart").children[1].style.display = "none";
            var emuDesc = emuchartsManager.getEmuchartsDescriptors().filter(function (desc) {
                return desc.is_selected === true;
            });
            var name = (emuDesc.length === 1) ? emuDesc[0].emuchart_name : "emucharts_" + projectManager.project().name();
            SaveAsView.create({
                heading: "Save As...",
                placeholder: "Please enter Emucharts name...",
                label: "Emucharts name",
                name: name
            }).on("cancel", function (e, formView) {
                formView.remove();
            }).on("ok", function (e, formView) {
                emuchartsManager.saveChartAs(e.data.name);
                formView.remove();
            });
        });
        d3.select("#btn_menuExportAsImage").on("click", function () {
            exportDiagram.toVectorialImage(emuchartsManager);
        });
        d3.select("#btn_menuCloseCurrentChart").on("click", function () {
            emuchartsManager.closeChart();
            emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
            restartEditor();
        });
        d3.select("#btn_menuDeleteCurrentChart").on("click", function () {
            emuchartsManager.deleteChartDialog().then(function (res) {
                if (res) {
                    // the file has been deleted, we need to update the Emucharts Editor front-end
                    emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                    restartEditor();
                }
            });
        });

        //-- States menu -----------------------------------------------------------
        d3.select("#menuStates").on("mouseover", function () {
            document.getElementById("menuStates").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewState").on("click", function () {
            emuchartsTables.add({ machinestate: true });
        });
        d3.select("#btn_menuRenameState").on("click", function () {
            emuchartsTables.edit({ machinestate: true, aux: true });
        });
        d3.select("#btn_menuDeleteState").on("click", function () {
            delete_mode();
        });
        d3.select("#btn_menuLayOutStates").on("click", function () {
            var editor = emuchartsManager.getSelectedEditor();
            if (editor) {
                var trans = editor.getTransformation();
                // editor.layOutChart_nath();
                editor.layOutChart();
                emuchartsManager.render({ trans: trans });
            }
        });

        //-- Transitions menu -----------------------------------------------------------
        d3.select("#menuTransitions").on("mouseover", function () {
            document.getElementById("menuTransitions").children[1].style.display = "block";
        });
        d3.select("#btn_menuNewTransition").on("click", function () {
            emuchartsTables.add({ transition: true, aux: true });
        });
        d3.select("#btn_menuRenameTransition").on("click", function () {
            emuchartsTables.edit({ transition: true, aux: true });
        });
        d3.select("#btn_menuDeleteTransition").on("click", function () {
            delete_transition();
        });



        //-- Code generators menu -----------------------------------------------------------
        d3.select("#menuCodeGenenerators").on("mouseover", function () {
            document.getElementById("menuCodeGenenerators").children[1].style.display = "block";
        });

        function printer_template(printer_name, file_extension) {
            var desc = emuchartsManager.getSelectedEmuchartsDescriptor();
            var filename = (desc && desc.emuchart_name) ? desc.emuchart_name
                                : ("emucharts_" + projectManager.project().name());
            filename = filename.replace(/-/g, "_");
            var emucharts = {
                name: filename,
                author: {
                    name: "xxxx",
                    affiliation: "xxxx",
                    contact: "xxx"
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                datatypes: emuchartsManager.getDatatypes(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = printer_name.print(emucharts);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + file_extension;
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, " + file_extension.replace(".","") + " model is undefined.");
            }
        }

        d3.select("#btn_menuPVSPrinter").on("click", function () {
            var desc = emuchartsManager.getSelectedEmuchartsDescriptor();
            var filename = (desc && desc.emuchart_name) ? desc.emuchart_name
                                : ("emucharts_" + projectManager.project().name());
            filename = filename.replace(/-/g, "_");
            var emucharts = {
                name: filename,
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                datatypes: emuchartsManager.getDatatypes(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsPVSPrinter.print(emucharts, { interactive: true });
        });
        d3.select("#btn_menuAlloyPrinter").on("click", function () {
            printer_template(emuchartsCodeGenerators.emuchartsAlloyPrinter, ".alloy");
        });
        d3.select("#btn_menuNuXMVPrinter").on("click", function () {
            printer_template(emuchartsCodeGenerators.emuchartsNuXMVPrinter, ".smv");
            // var emucharts = {
            //     name: ("emucharts_" + projectManager.project().name().replace(/-/g, "_") + "_SMV"),
            //     author: {
            //         name: "..author name..",
            //         affiliation: "..affiliation..",
            //         contact: "..contact.."
            //     },
            //     importings: [],
            //     constants: emuchartsManager.getConstants(),
            //     variables: emuchartsManager.getVariables(),
            //     states: emuchartsManager.getStates(),
            //     transitions: emuchartsManager.getTransitions(),
            //     initial_transitions: emuchartsManager.getInitialTransitions()
            // };
            // var model = emuchartsCodeGenerators.emuchartsNuXMVPrinter.print(emucharts);
            // if (model.err) {
            //     console.log(model.err);
            //     return;
            // }
            // if (model.res) {
            //     var name = emucharts.name + ".smv";
            //     var content = model.res;
            //     return projectManager.project().addFile(name, content, { overWrite: true });
            // } else {
            //     console.log("Warning, NuXMV model is undefined.");
            // }
        });
        d3.select("#btn_menuPIMPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_PIM"),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsPIMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".tex";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, PIM model is undefined.");
            }
        });
        d3.select("#btn_menuCppPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name()),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsPIMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".cpp";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, C++ code is undefined.");
            }
        });
        d3.select("#btn_menuMALPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_MAL"),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsMALPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".i";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, MAL model is undefined.");
            }
        });
        d3.select("#btn_menuVDMPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_VDM"),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsVDMPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.res) {
                var name = emucharts.name + ".vdmsl";
                var content = model.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, VDM model is undefined.");
            }
        });

        d3.select("#btn_menuJavaScriptPrinter").on("click", function () {
            var desc = emuchartsManager.getSelectedEmuchartsDescriptor();
            var filename = (desc && desc.emuchart_name) ? desc.emuchart_name
                                : ("emucharts_" + projectManager.project().name());
            filename = filename.replace(/-/g, "_");
            var emucharts = {
                name: filename,
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                datatypes: emuchartsManager.getDatatypes(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsJSPrinter.print(emucharts, { interactive: false });
        });

        d3.select("#btn_menuAdaPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_ADA"),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsAdaPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.spec && model.body) {
                var overWrite = {overWrite: true};
                projectManager.project().addFile(emucharts.name + ".adb", model.body, overWrite);
                projectManager.project().addFile(emucharts.name + ".ads", model.spec, overWrite);
            } else {
                console.log("Warning, Ada code is undefined.");
            }
        });
        d3.select("#btn_menuBlessPrinter").on("click", function () {
            var emucharts = {
                name: ("emucharts_" + projectManager.project().name() + "_Bless"),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            var model = emuchartsCodeGenerators.emuchartsBlessPrinter.print(emucharts);
            console.log(model);
            if (model.err) {
                console.log(model.err);
                return;
            }
            if (model.thread) {
                var overWrite = {overWrite: true};
                projectManager.project().addFile(emucharts.name + ".aadl", model.thread, overWrite);
            } else {
                console.log("Warning, Bless model is undefined.");
            }
        });
        d3.select("#btn_menuMisraCPrinter").on("click", function () {
            var desc = emuchartsManager.getSelectedEmuchartsDescriptor();
            var filename = (desc && desc.emuchart_name) ? desc.emuchart_name
                                : ("emucharts_" + projectManager.project().name());
            filename = filename.replace(/-/g, "_");
            var emucharts = {
                name: filename,
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                datatypes: emuchartsManager.getDatatypes(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsMisraCPrinter.print(emucharts, { interactive: true });
        });
        d3.select("#btn_menuFMIPVSPrinter").on("click", function () {
            var emucharts = {
                name: projectManager.project().name(),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsFMIPVSPrinter.print(emucharts);
        });
        d3.select("#btn_menuAndroidPrinter").on("click", function () {
            var emucharts = {
                name: projectManager.project().name(),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsAndroidPrinter.print(emucharts);
        });
        d3.select("#btn_menuICOPrinter").on("click", function () {
            var emucharts = {
                name: projectManager.project().name(),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                importings: [],
                constants: emuchartsManager.getConstants(),
                variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: emuchartsManager.getInitialTransitions()
            };
            emuchartsCodeGenerators.emuchartsICOPrinter.print(emucharts);
        });

        //-- Verification menu ---------------------------------------------------
        d3.select("#btn_menuConsistencyOfActions").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            }).concat([ "current_state", "previous_state" ]);
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = EmuchartsParser.getInstance();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            ConsistencyTemplateView.create({
                header: "Consistency of user actions",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_");
                var pvs_property = e.data.get("pvs_property");
                var pvs_theorem = e.data.get("pvs_theorem");
                var prooflite_strategy = e.data.get("prooflite_strategy");
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var theTheory = Handlebars.compile(pvs_theory, { noEscape: true })({
                        theory_name: "consistency",
                        importing: emucharts_theory_name,
                        transition_system: Handlebars.compile(pvs_transition_system, { noEscape: true })({
                            functionName: "action",
                            transitions: transitions
                        }),
                        pvs_property: pvs_property,
                        pvs_guard: Handlebars.compile(pvs_guard, { noEscape: true })({
                            guard_name: "guard",
                            state: "State"
                        }),
                        pvs_theorem: pvs_theorem,
                        prooflite_strategy: prooflite_strategy
                    });
                    projectManager.project().addFile("consistency.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("consistency.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuReversibilityOfActions").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            }).concat([ "current_state", "previous_state" ]);
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = EmuchartsParser.getInstance();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            ReversibilityTemplateView.create({
                header: "Reversibility of user actions",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_");
                var pvs_property = e.data.get("pvs_property");
                var pvs_theorem = e.data.get("pvs_theorem");
                var prooflite_strategy = e.data.get("prooflite_strategy");
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var theTheory = Handlebars.compile(pvs_theory, { noEscape: true })({
                        theory_name: "reversibility",
                        importing: emucharts_theory_name,
                        transition_system: Handlebars.compile(pvs_transition_system, { noEscape: true })({
                            functionName: "action",
                            transitions: transitions
                        }),
                        pvs_property: pvs_property,
                        pvs_guard: Handlebars.compile(pvs_guard, { noEscape: true })({
                            guard_name: "guard",
                            state: "State"
                        }),
                        pvs_theorem: pvs_theorem,
                        prooflite_strategy: prooflite_strategy
                    });
                    projectManager.project().addFile("reversibility.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("reversibility.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
        });

        d3.select("#btn_menuVisibilityOfModes").on("click", function () {
            // document.getElementById("menuVerification").children[1].style.display = "none";
            var stateVariables = emuchartsManager.getVariables().map(function (variable) {
                return variable.name;
            }).concat([ "current_state", "previous_state" ]);
            var transitionLabels = emuchartsManager.getTransitions();
            var transitions = d3.map();
            var parser = EmuchartsParser.getInstance();
            transitionLabels.forEach(function (label) {
                var ans = parser.parseTransition(label.name);
                if (ans.res) {
                    transitions.set(ans.res.val.identifier.val);
                }
            });
            transitions = transitions.keys();
            FeedbackTemplateView.create({
                header: "Visibility of modes",
                stateVariables: stateVariables,
                transitions: transitions,
                buttons: ["Dismiss", "Create PVS Theory"]
            }).on("create pvs theory", function (e, view) {
                // do something useful here.... like create a pvs file with the instantiated property
                var emucharts_theory_name = "emucharts_" + projectManager.project().name().replace(/-/g, "_");
                var pvs_property = e.data.get("pvs_property");
                var pvs_theorem = e.data.get("pvs_theorem");
                var prooflite_strategy = e.data.get("prooflite_strategy");
                var modelEditor = ModelEditor.getInstance();
                (PluginManager.getInstance().isLoaded(modelEditor)
                    ? Promise.resolve()
                    : PluginManager.getInstance().enablePlugin(modelEditor))
                .then(function () {
                    var theTheory = Handlebars.compile(pvs_theory, { noEscape: true })({
                        theory_name: "visibility",
                        importing: emucharts_theory_name,
                        transition_system: Handlebars.compile(pvs_transition_system, { noEscape: true })({
                            functionName: "action",
                            transitions: transitions
                        }),
                        pvs_property: pvs_property,
                        pvs_guard: Handlebars.compile(pvs_guard, { noEscape: true })({
                            guard_name: "guard",
                            state: "State"
                        }),
                        pvs_theorem: pvs_theorem,
                        prooflite_strategy: prooflite_strategy
                    });
                    projectManager.project().addFile("visibility.pvs", theTheory, { overWrite: true });
                    projectManager.selectFile("visibility.pvs");
                });
            }).on("dismiss", function (e, view) {
                // just remove window
                view.remove();
            });
        });
        //-- Zoom menu -----------------------------------------------------------
        d3.select("#menuZoom").on("mouseover", function () {
            document.getElementById("menuZoom").children[1].style.display = "block";
        });
        d3.select("#btn_menuZoomIn").on("click", function () {
            emuchartsManager.zoom_in();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });
        d3.select("#btn_menuZoomOut").on("click", function () {
            emuchartsManager.zoom_out();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });
        d3.select("#btn_menuZoomReset").on("click", function () {
            emuchartsManager.zoom_reset();
            document.getElementById("menuZoom").children[1].style.display = "none";
        });

        //--node filter handler
        d3.select("input#filter").on("keyup", function () {
            var editor = emuchartsManager.getSelectedEditor();
            if (editor) {
                editor._nodeFilter = d3.select("input#filter").property("value");
                emuchartsManager.render();
            }
        });

        //-- Emuchart Selector  -----------------------------------------------------------
        emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
        // d3.select("#btn_emucharts_1").on("click", function () {
        //     emuchartsManager.loadEmucharts("EMUCHART__0");
        // });

        //-- PIM -----------------------------------------------------------------
        d3.select("#btn_toPIM").on("click", function () {
            if (emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is already a PIM.");
                return;
            }
            if (emuchartsManager.toPIM(true)) {
                console.log("Success, converted emuchart to a PIM.");
            }
            else {
                console.log("Warning, unable to convert emuchart to a PIM.");
            }
        });
        d3.select("#btn_fromPIM").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            if (emuchartsManager.toPIM(false)) {
                console.log("Success, converted emuchart from a PIM.");
            }
            else {
                console.log("Warning, unable to convert emuchart from a PIM.");
            }
        });
        d3.select("#btn_menuTestGenerator").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            var initTrans = emuchartsManager.getInitialTransitions();
            var emuchart = {
                name: ("emucharts_" + projectManager.project().name()),
                author: {
                    name: "..author name..",
                    affiliation: "..affiliation..",
                    contact: "..contact.."
                },
                //constants: emuchartsManager.getConstants(),
                //variables: emuchartsManager.getVariables(),
                states: emuchartsManager.getStates(),
                transitions: emuchartsManager.getTransitions(),
                initial_transitions: initTrans,
                pm: {
                    name: projectManager.project().name(),
                    widgets: [],
                    components: emuchartsManager.getStates(),
                    pmr: []
                },
                start_state: initTrans ? initTrans[0].target.name : "",
                final_states: [],
                isPIM: emuchartsManager.getIsPIM()
            };

            var tests = pimTestGenerator.print(emuchart.name, { pims: [ emuchart ], pms: [] });
            if (tests.err) {
                console.log(tests.err);
                return;
            }
            if (tests.res) {
                var name = tests.file_name;
                var content = tests.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            } else {
                console.log("Warning, TestGenerator model is undefined.");
            }
        });
        d3.select("#btn_menuTestGeneratorFromFile").on("click", function () {
            var models;
            // Generate tests from importing a file
            FileHandler.openLocalFileAsText(function (err, res) {
                if (res) {
                    // Try parse as PIM
                    models = pimImporter.importPIM(res);
                    if (models.err) {
                        console.log(models.err);
                        return;
                    }
                    // Remove file extension
                    var name = res.name.substr(0, res.name.lastIndexOf('.'));
                    var tests = pimTestGenerator.print(name, models.models);
                    if (tests.err) {
                        console.log(tests.err);
                        return;
                    }

                    if (tests.res) {
                        var testsName = tests.file_name;
                        var content = tests.res;
                        return projectManager.project().addFile(testsName, content, { overWrite: true });

                    } else {
                        console.log("Warning, TestGenerator model is undefined.");
                    }

                } else {
                    console.log("Error while opening file (" + err + ")");
                }

            }, { header: "Open PIM file..." });
        });
        d3.select("#btn_menuPMTextGenerator").on("click", function () {
            if (!emuchartsManager.getIsPIM()) {
                console.log("Warning, current emuchart is not a PIM.");
                return;
            }
            var emuchart = {
                pm: {
                    name: projectManager.project().name(),
                    widgets: [],
                    components: emuchartsManager.getStates(),
                    pmr: []
                }
            };

            var text = PMTextGenerator.print(("emucharts_" + projectManager.project().name()), emuchart);
            if (text.err) {
                console.log(text.err);
                return;
            }
            if (text.res) {
                var name = text.file_name;
                var content = text.res;
                return projectManager.project().addFile(name, content, { overWrite: true });
            }
        });
	};



    Emulink.prototype.getEmuchartsManager = function () {
        return emuchartsManager;
    };

    Emulink.prototype.getDependencies = function () {
        return [];
    };

    function unloadExternalListeners() {
        projectManager.removeListener("ProjectChanged", onProjectChanged);
    }

    function onProjectChanged(event) {
        // try to open all emucharts files of the project
        function finalize(opt) {
            opt = opt || {};
            if (!opt.loadOnly) {
                // render emuchart
                emuchartsManager.render();
            }
            // set initial editor mode
            var _this = require("plugins/emulink/Emulink").getInstance();
            _this.browseMode();
            updateContextTables();
        }
        function readFile(file, opt) {
            fs.readFile(file.path).then(function (res) {
                res.content = JSON.parse(res.content);
                emuchartsManager.importEmucharts(res, opt);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                finalize(opt);
            }).catch(function (err) {
                // log any error
                console.log(err);
                // open an empty chart and give it the same name of the corrupted emuchart
                emuchartsManager.newChart(file.name);
                emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
            });
        }
        var path = event.current.name();
        emuchartsManager.closeAllCharts();
        return new Promise(function (resolve, reject) {
            WSManager.getWebSocket().send({type: "readDirectory", path: path}, function (err, res) {
                if (err) {
                    reject(err);
                } else {
                    var emuchartsFiles = res.files.filter(function (file) {
                        return file.name.endsWith(".emdl");
                    }).sort(function (a,b) {
                        return a.name < b.name;
                    });
                    if (emuchartsFiles && emuchartsFiles.length > 0) {
                        var promises = [];
                        var isFirst = true;
                        var i = 0;
                        emuchartsFiles.forEach(function (file) {
                            promises.push(new Promise(function (resolve, reject) {
                                // Defer loading of files to avoid unresponsive user interfaces
                                // This is useful when the project has multiple files, as it may take some time to load them
                                if (isFirst) {
                                    isFirst = false;
                                    readFile(file);
                                } else {
                                    setTimeout(function () {
                                        readFile(file, { loadOnly: true });
                                    }, 600 + i * 200);
                                    i++;
                                }
                            }));
                        });
                        Promise.all(promises).then(function (res) {
                            resolve(emuchartsFiles);
                        }).catch(function (err) { reject(err); });
                    } else {
                        // if the emuchart files are not in the project, then just create an empty emuchart
                        emuchartsManager.closeAllCharts();
                        emuchartsManager.newChart();
                        emuchartsSelector.render(emuchartsManager.getEmuchartsDescriptors());
                        finalize();
                    }
                    resolve(emuchartsFiles);
                }
            });
        });
    }

    Emulink.prototype.initialise = function () {
        // enables the plugin -- this includes also enabling any dependencies defined in getDependencies method
        // create local references to PVS editor, websocket client, and project manager
        editor = ModelEditor.getInstance().getEditor();
        ws = pvsioWebClient.getWebSocket();
        projectManager = ProjectManager.getInstance();
        // listen to ProjectChanged events so that we can update the editor when a new project is opened
        projectManager.addListener("ProjectChanged", onProjectChanged);
        // create user interface elements
        try {
            this.createHtmlElements();
        } catch (err_html) {
            console.error(err_html);
        } finally {
            return new Promise(function (resolve, reject) {
                onProjectChanged({ current: projectManager.project() }).then(function () {
                    resolve(true);
                }).catch(function (err) {
                    console.log(err);
                    reject(err);
                });
            });
        }
    };

    Emulink.prototype.unload = function () {
        PVSioWebClient.getInstance().removeCollapsiblePanel(canvas);
        canvas = null;
        unloadExternalListeners();
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new Emulink();
            }
            return instance;
        },

        hasInstance: function () {
            return !!instance;
        }
    };
});
