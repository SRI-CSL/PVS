/**
 * Utility functions to check whether the Javascript engine of the browser can correctly support PVSio-web
 * @author Paolo Masci
 * @date 5/2/15 12:46 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define*/
define(function (require, exports, module) {
    "use strict";
    
    var chrome = /Chrome\//.test(navigator.userAgent);
    var firefox = /Firefox\//.test(navigator.userAgent);
    
    function getBrowserVersion(browserName) {
        var info = navigator.userAgent.split(" ").filter(function(name) {
            return name.indexOf(browserName) >= 0;
        }).toString().split("/");
        if (info && info.length === 2) {
            return parseFloat(info[1]);
        }
        return -1;
    }
    
    function isBrowserSupported() {
        return chrome && getBrowserVersion("Chrome") >= 42 ||
                firefox && getBrowserVersion("Firefox") >= 37;
    }
    
    function getVersion() {
        if (chrome || firefox) {
            return navigator.userAgent.split(" ").filter(function(name) {
                return name.indexOf("Chrome") >= 0 || name.indexOf("Firefox") >= 0;
            }).toString().split("/").join(" ");
        }
        return navigator.userAgent;
    }
    
    function requiredBrowser() {
        return "PVSio-web requires Chome 42 or greater, or Firefox 37 or greater";
    }
    
    function requiredBrowserWarning() {
        return "Warning: " + requiredBrowser() + " (your browser is " + getVersion() + ")";
    }

    module.exports = {
        isBrowserSupported: isBrowserSupported,
        getVersion: getVersion,
        requiredBrowser: requiredBrowser,
        requiredBrowserWarning: requiredBrowserWarning
    };
});
