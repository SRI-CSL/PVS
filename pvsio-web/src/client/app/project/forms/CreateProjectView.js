/**
 * model for new project
 * @author Patrick Oladimeji, Paolo Masci
 * @date Jan 3, 2013 : 12:56:03 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, Handlebars, $*/
define(function (require, exports, module) {
    "use strict";
    var d3			= require("d3/d3"),
        createProjectTemplate = require("text!./templates/createProject.handlebars"),
        FileSystem      = require("filesystem/FileSystem"),
        BaseDialog  = require("pvsioweb/forms/BaseDialog"),
//        MIME        = require("util/MIME"),
        PVSioWebClient        = require("PVSioWebClient").getInstance();

//    function imageFilter(d) {
//        var mime = MIME.getInstance();
//        return !mime.isHiddenFile(d.path) && (d.isDirectory || mime.isImage(d.path));
//    }
//
//    function pvsFilter(d) {
//        var mime = MIME.getInstance();
//        return !mime.isHiddenFile(d.path) && (d.isDirectory || mime.isPVS(d.path));
//    }

    function choose(_title, _encoding) {
        function fileList2Array(fl) {
            var ans = [], i = 0;
            for (i = 0; fl && i < fl.length; i++) { ans[i] = fl[i].name; }
            return ans;
        }
        var el_remote = (_encoding === "base64") ? "#prototypeImage" : "#pvsSpec";
        var fs = new FileSystem();
        if (PVSioWebClient.serverOnLocalhost()) {
            fs.readFileDialog({
                encoding: _encoding,
                title: _title
            }).then(function (res) {
                // TODO: here we can use res.content to show a preview of the image
                var paths = res.map(function (f) {
                    return f.path;
                }).join(",");// NB: avoid any space here between file names, otherwhise the path is not re-constructed correctly. FIXME: We need to think of a more robust implementation!
                $(el_remote).val(paths);
            });
        } else {
            var el_local = (_encoding === "base64") ? "#localPrototypeImage" : "#localPVSSpec";
            d3.select(el_local).on("change", function () {
                var paths = fileList2Array(d3.select(el_local).node().files);
                $(el_remote).val("");
                if (paths) {
                    $(el_remote).val(paths.join(", "));
                }
            });
            d3.select(el_local).node().click();
        }
    }

    var CreateProjectView = BaseDialog.extend({
        render: function (data) {
            var template = Handlebars.compile(createProjectTemplate);
            this.$el.html(template(data));
            $("body").append(this.el);
            return this;
        },
        events: {
            "click #btnOk": "ok",
            "click #btnCancel": "cancel",
            "click #btnChooseImage": "chooseImage",
            "click #btnChooseSpec": "chooseSpec"
        },
        chooseImage: function () {
            choose("Please select a prototype image...", "base64");
        },
        chooseSpec: function () {
            choose("Please select PVS model files...");
        }
    });

    module.exports = {
        create: function (data) {
            return new CreateProjectView(data);
        }
    };
});
