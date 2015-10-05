/**
 * Useful Utilities for working with html forms
 * @author Patrick Oladimeji
 * @date 11/5/13 9:22:48 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, d3*/

define(function (require, exports, module) {
    "use strict";
    var defaultInputSelectors = "select, input, textarea";
    function serializeForm(el, inputSelectors) {
        inputSelectors = inputSelectors || defaultInputSelectors;
        var res = {};
        d3.select(el).selectAll(inputSelectors).each(function () {
            var el = d3.select(this), type = el.attr("type");
            if (type === "checkbox" || type === "radio") {
                //only add checkboxes when they aer checked
                if (this.checked) {
                    //store as array if it is a checkbox
                    if (!res[el.attr("name")]) { res[el.attr("name")] = []; }
                    res[el.attr("name")].push(el.property("value"));
                }
            } else if (el.attr("type") === "file") {
                res[el.attr("name")] =  el.property("files");
            } else {
                res[el.attr("name")] = el.property("value") || el.text();
            }
        });
        return res;
    }
    
    function validate(form, inputSelectors) {
        var res = true;
        inputSelectors = inputSelectors || defaultInputSelectors;
        d3.select(form).selectAll(inputSelectors).each(function (d) {
            res = res && this.checkValidity();
        });
        return res;
    }

    module.exports = {
        serializeForm:	serializeForm,
        validateForm: validate
    };
});
