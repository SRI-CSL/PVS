/**
 * Collection of utils for normalization of strings
 * @author Paolo Masci
 * @date June 30, 2018
 */
/*jshint unused: false, esnext: true*/
/*global define*/
define(function (require, exports, module) {
    "use strict";

    let instance = null;

    class Normalize {
        constructor() { return this; }
        removeSpaceDash(str) {
            if (str) {
                return str.replace(/[\s|-]/g, "_");
            }
            return str;
        }
    }

    module.exports = {
        getInstance: function () {
            instance = instance || new Normalize();
            return instance;
        }
    };
});
