/**
 * MIME type utils
 * @author Paolo Masci -- this module is the client-side equivalent of server/FileFilter
 * @date 3/12/14 3:09:12 PM
 */
/*jshint unused: false*/
/*global define*/
define(function (require, exports, module) {
    "use strict";

    var instance = null,
        imageExts = [".jpg", ".jpeg", ".png"],
        modelExts = [".pvs", ".muz", ".tex", ".i", ".emdl", ".vdmsl", ".aadl", ".adb", ".ads", ".c", ".h", ".smv", ".als"],
        otherExts = [".txt" ]; //, ".json"];

    function MIME() { return this; }

    MIME.prototype.isHiddenFile = function (filename) {
        return (filename) ? filename.split("/").slice(-1)[0].indexOf(".") === 0 : false;
    };

    function checkExt(filename, legalExts) {
        if (filename && typeof filename === "string") {
            var ext = filename.split(".").slice(-1);
            if (ext && ext.length > 0) {
                return legalExts.indexOf("." + ext[0].toLowerCase()) > -1;
            }
            return false;
        }
        return false;
    }

    MIME.prototype.isImage = function (filename) {
        return checkExt(filename, imageExts);
    };

    MIME.prototype.isModel = function (filename) {
        return checkExt(filename, modelExts);
    };

    MIME.prototype.isPVS = function (filename) {
        return checkExt(filename, [".pvs"]);
    };

    MIME.prototype.isEmucharts = function (filename) {
        return checkExt(filename, [".emdl"]);
    };

    MIME.prototype.isPIM = function (filename) {
        return checkExt(filename, [".muz"]);
    };

    MIME.prototype.imageFilter = function (d) {
        return (instance.isHiddenFile(d.path) === false) && (d.isDirectory || instance.isImage(d.path));
    };

    MIME.prototype.modelFilter = function (d) {
        return (instance.isHiddenFile(d.path) === false) && (d.isDirectory || instance.isModel(d.path));
    };

    MIME.prototype.pvsFilter = function (d) {
        return (instance.isHiddenFile(d.path) === false) && (d.isDirectory || instance.isPVS(d.path));
    };

    MIME.prototype.filter = function (exts) {
        return function (descriptor) {
            return (instance.isHiddenFile(descriptor.path) === false) && (descriptor.isDirectory || checkExt(descriptor.path, exts));
        };
    };

    MIME.prototype.getImageExts = function () {
        return imageExts;
    };

    MIME.prototype.getModelExts = function () {
        return modelExts;
    };

    MIME.prototype.getFileExts = function () {
        return modelExts.concat(imageExts).concat(otherExts);
    };

    module.exports = {
        getInstance: function () {
            if (!instance) {
                instance = new MIME();
            }
            return instance;
        }
    };
});
