/**
 * Useful Utilities for working with html forms
 * @author Paolo Masci
 * @date 25/5/14 2:03:48 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
define(function (require, exports, module) {
    "use strict";
    function serializeForm(el, inputSelectors) {
        var options = d3.map();
        var labels = d3.map();
        inputSelectors.forEach(function (s) {
            var elem = document.getElementById(s);
            if (elem.selectedIndex >= 0) {
                // listbox item
                options.set(s, elem.selectedIndex);
            } else if (elem.value) {
                // inputbox item
                labels.set(s, elem.value.trim());
            }
        });
        return { options: options, labels: labels };
    }

    /**
     * Clears the values from the supplied inputs.
     * @param inputSelectors The Ids of the inputs to clear values from.
     */
    function clearForm(inputSelectors) {
        inputSelectors.forEach(function (s) {
            var elem = document.getElementById(s);
            if (elem.selectedIndex >= 0)
                elem.selectedIndex = 0;
            else if (elem.value)
                elem.value = "";
        });
    }
	
	function validate(form, inputSelectors) {
        // FIXME: implement this function
		return true;
	}
   
	module.exports = {
		serializeForm:	serializeForm,
        clearForm: clearForm,
		validateForm: validate
	};
});
