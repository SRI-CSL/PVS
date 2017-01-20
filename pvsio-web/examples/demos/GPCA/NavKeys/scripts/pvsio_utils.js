/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, _ */
/**
	Now we load the websocket library to use for connection to the pvsio webserver
	for more info on this see http://requirejs.org
*/
var ws;

var console_demo = document.getElementById('console_demo');
var ControllerConsole = document.getElementById('GIP-Controller_console');
var uiConsole = document.getElementById('GIP-UI_console');

var ctrl_clock;
var ctrl_clock_interval = 2048; //ms
var tick_enabled;
var tick_enabled_field = new RegExp(" tick_enabled:=([a-zA-Z]+)");

var displayFOUR_topline = document.getElementById("GIP-displayFOUR-topline");
var displayFOUR_primary = document.getElementById("GIP-displayFOUR-primary");
var displayFOUR_secondary = document.getElementById("GIP-displayFOUR-secondary");
var displayFOUR_botline = document.getElementById("GIP-displayFOUR-botline");

var displayTHREE_topline = document.getElementById("GIP-displayTHREE-topline");
var displayTHREE_primary = document.getElementById("GIP-displayTHREE-primary");
var displayTHREE_secondary = document.getElementById("GIP-displayTHREE-secondary");
var displayTHREE_botline = document.getElementById("GIP-displayTHREE-botline");

var displayTWO_topline = document.getElementById("GIP-displayTWO-topline");
var displayTWO_primary = document.getElementById("GIP-displayTWO-primary");
var displayTWO_secondary = document.getElementById("GIP-displayTWO-secondary");
var displayTWO_botline = document.getElementById("GIP-displayTWO-botline");

var displayONE_topline = document.getElementById("GIP-displayONE-topline");
var displayONE_primary = document.getElementById("GIP-displayONE-primary");
var displayONE_secondary = document.getElementById("GIP-displayONE-secondary");
var displayONE_botline = document.getElementById("GIP-displayONE-botline");

var powerLed = document.getElementById("GIP-PowerLed");

var display_cmd = document.getElementById("GIP-cmd");

var timestamp = new Date();
var bullet = "&#8226;";

// GUI_ACTION is set to 1 by the functions defined in UI_mappings.js
var GUI_ACTION = 0;

// this variables contains the GPCAUI state that will be rendered in the log window
var pvsio_response;

// these variables are used to parse the fields of the GPCA-UI state
var primaryMsg;
var primaryMsg_field = new RegExp(" primaryMsg:=([A-Za-z0-9\/_]+)");
var secondaryMsg;
var secondaryMsg_field = new RegExp(" secondaryMsg:=([A-Za-z0-9\/_]+)");
var dePrimaryMsg;
var dePrimaryMsg_field = new RegExp(" dePrimaryMsg:=([A-Za-z0-9\/_]+)");
var deSecondaryMsg;
var deSecondaryMsg_field = new RegExp(" deSecondaryMsg:=([A-Za-z0-9\/_]+)");
var display;
var display_field = new RegExp(" display:=([0-9\/.-]+)");
var dispMode;
var dispMode_field = new RegExp(" dispMode:=([0-9a-zA-Z\/_]+)");
var cursor;
var cursor_field  = new RegExp(" cursor:=([0-9\/.-]+)");
var unit;
var unit_field   = new RegExp(" displayUnit:=([0-9A-Za-z\/_]+)");
var block_interaction;
var block_interaction_field = new RegExp(" block_interaction:=([A-Za-z]+)");
// field ctrl_cmd specified the command for the controller
var ctrl_cmd;
var ctrl_cmd_field = new RegExp(" ctrl_cmd:=([0-9A-Za-z\/_().]+)");
var ctrl_state;
var ctrl_state_field = new RegExp(" ctrl_state:=([0-9A-Za-z\/_]+)");
var vtbi_value;
var vtbi_value_field = new RegExp("VTBI_val:=([0-9\/.-]+)");
var vtbi_unit;
var vtbi_unit_field = new RegExp("VTBI_unit:=([0-9a-zA-Z\/.-_]+)");
var doseRATE_value;
var doseRATE_value_field = new RegExp("doseRATE_val:=([0-9\/.-]+)");
var doseRATE_unit;
var doseRATE_unit_field = new RegExp("doseRATE_unit:=([0-9a-zA-Z\/.-_]+)");

var doseRateLowerHardLimit;
var doseRateLowerHardLimit_field = new RegExp("doseRateLowerHardLimit:=([0-9\/.]+)");
var doseRateLowerSoftLimit;
var doseRateLowerSoftLimit_field = new RegExp("doseRateLowerSoftLimit:=([0-9\/.]+)");
var doseRateUpperHardLimit;
var doseRateUpperHardLimit_field = new RegExp("doseRateUpperHardLimit:=([0-9\/.]+)");
var doseRateUpperSoftLimit;
var doseRateUpperSoftLimit_field = new RegExp("doseRateUpperSoftLimit:=([0-9\/.]+)");
var doseRateTypical;
var doseRateTypical_field = new RegExp("doseRateTypical:=([0-9\/.]+)");
var doseRateUnit;
var doseRateUnit_field = new RegExp("doseRateUnit:=([a-zA-Z_]+)");

var vtbiLowerHardLimit;
var vtbiLowerHardLimit_field = new RegExp("vtbiLowerHardLimit:=([0-9\/.]+)");
var vtbiLowerSoftLimit;
var vtbiLowerSoftLimit_field = new RegExp("vtbiLowerSoftLimit:=([0-9\/.]+)");
var vtbiUpperHardLimit;
var vtbiUpperHardLimit_field = new RegExp("vtbiUpperHardLimit:=([0-9\/.]+)");
var vtbiUpperSoftLimit;
var vtbiUpperSoftLimit_field = new RegExp("vtbiUpperSoftLimit:=([0-9\/.]+)");
var vtbiTypical;
var vtbiTypical_field = new RegExp("vtbiTypical:=([0-9\/.]+)");
var vtbiUnit;
var vtbiUnit_field = new RegExp("vtbiUnit:=([a-zA-Z_]+)");

var drugID;
var drugID_field = new RegExp("drugID:=([a-zA-Z_]+)");


// utility functions, used to have a uniform output for the gpcaui state and the messages from the controller
var prettyprintPVSioOutput = function (obj) {
  // this first filter removes messages generated by PVS (e.g., finished garbage collection)
    "use strict";
    var ans = obj[0];

    GIPlog(ans);
    UIlog(ans);
    return ans;
};
var prettyprintReceivedData = function (obj) {
    "use strict";
    var ans = obj.toString().substring(obj.toString().indexOf("(#"), obj.toString().lastIndexOf("#)") + 2);
    return ans;
};

// utility function, sends the command specified in ctrl_cmd to the GPCA controller
function send_message_to_state_controller() {
    "use strict";
//	var cmd = ctrl_cmd.toString();
//	var token = cmd.substring(0,cmd.indexOf("("));
	// need to evaluate the argument of higher-order commands ConfirmVTBI and ConfirmDoseRate
//	if(token == "ConfirmVTBI" || token == "ConfirmDoseRate" ) {
//		var val = cmd.substring(cmd.indexOf("(") + 1,cmd.indexOf(")"));
//		cmd = token + "(" + eval(val) + ")";
//	}
	// render command in the command display debugger
//	if(cmd != "CMD_NULL") { display_cmd.innerHTML = cmd; }
	// send command to controller
//	GIPlog("Sending message " + cmd + " to GPCA state controller...");
//	gip.send(cmd);
}

// utility function, renders the cursor in data entry mode
var render_dataentry = function (display, val) {
    "use strict";
    display.innerHTML = "";
    display.style.font = "bold 50px Arial";
    display.style.textAlign = "right";
    display.style.color = "black";

    var disp = val.toString();
    // display from left to right
    var decimal_place = disp.indexOf(".");
    var cursor_rendered = false;
    var i = 0;
    for (i = 0; i < disp.length; i++) {
        var digit = disp.charAt(i);
        if (digit === ".") {
            digit = bullet;
        }
        var close_span = false;
        if (decimal_place > 0) {
            if (cursor < 0) {
                if (cursor === decimal_place - i) {
                    display.innerHTML = display.innerHTML
                              + "<span class = decimaldigit><span class = \"invertedcolor\">"
                                  + digit + "</span></span>";
                    cursor_rendered = true;
                } else {
                    if (i > decimal_place) {
                        display.innerHTML = display.innerHTML + "<span class = decimaldigit>" + digit + "</span>";
                    } else { display.innerHTML = display.innerHTML + digit; }
                }
            } else {
                if (cursor === decimal_place - i - 1) {
                    display.innerHTML = display.innerHTML + "<span class = \"invertedcolor\">" + digit + "</span>";
                    cursor_rendered = true;
                } else {
                    if (i > decimal_place) {
                        display.innerHTML = display.innerHTML + "<span class = decimaldigit>" + digit + "</span>";
                    } else { display.innerHTML = display.innerHTML + digit; }
                }
            }
        } else {
            if (cursor === disp.length - i - 1) {
                display.innerHTML = display.innerHTML + "<span class = \"invertedcolor\">" + digit + "</span>";
                cursor_rendered = true;
            } else { display.innerHTML = display.innerHTML + digit; }
        }
    }
    if (cursor_rendered === false) {
        if (cursor < 0) {
            if (decimal_place < 0) {
                // the cursor has not been rendered because the decimal part is 0
                display.innerHTML = display.innerHTML + bullet;
                var gaps = cursor * -1;
                for (i = 0; i < gaps; i++) {
                    if (i === gaps - 1) {
                        display.innerHTML = display.innerHTML
                                          + "<span class = decimaldigit><span class = \"invertedcolor\">"
                                          + "_" + "</span></span>";
                    } else { display.innerHTML = display.innerHTML + "_"; }
                }
            } else {
                // the cursor has not been rendered because the number
                // has just one decimal digit (this rendering is specific to the case with 2 decimals)
                display.innerHTML = display.innerHTML
                              + "<span class = decimaldigit><span class = \"invertedcolor\">"
                          + "_" + "</span></span>";
            }
        } else { // the cursor is has been not rendered because it's beyond the max non-zero integer digit
            if (decimal_place < 0) {
                for (i = 0; i < cursor - disp.length; i++) {
                    if (i > decimal_place) {
                        display.innerHTML = "<span class = decimaldigit>" + "_" + "</span>" + display.innerHTML;
                    } else { display.innerHTML = "_" + display.innerHTML; }
                }
            } else {
                for (i = 0; i < cursor - decimal_place; i++) {
                    display.innerHTML = "_" + display.innerHTML;
                }
            }
            display.innerHTML = "<span class = \"invertedcolor\">" + "_" + "</span>" + display.innerHTML;
        }
    }
};

function log(msg) {
    "use strict";
//	console_demo.log(msg);
	console_demo.innerHTML = "[ " + (new Date()).toTimeString() + " ] " + msg + "<br><br>" + console_demo.innerHTML;
}

//function console_demo_log(msg){
//	console_demo.log(msg);
//	var c = document.getElementById('console_demo_log');
//	c.innerHTML = "[ " + (new Date()).toTimeString() + " ] " + msg + "<br><br>" + c.innerHTML;
//}

function pvsio_commands_log(msg) {
    "use strict";
	var c = document.getElementById('pvsio_commands_log');
	c.innerHTML = "[ " + (new Date()).toTimeString() + " ]<br>" + msg + "<br><br>" + c.innerHTML;
}

function pvsio_response_log(msg) {
    "use strict";
	var c = document.getElementById('pvsio_response_log');
	c.innerHTML = "[ " + (new Date()).toTimeString() + " ]<br>" + msg + "<br><br>" + c.innerHTML;
}

function specification_log(msg) {
    "use strict";
	var c = document.getElementById('specification_log');
	c.innerHTML = msg + "<br>" + c.innerHTML;
}

function show_controller_state() {
    "use strict";
	var button = document.getElementById('show_controller_state_button');
	if (ControllerConsole.style.display === "none") {
		ControllerConsole.style.display = "block";
		button.value = "Hide controller state";
	} else {
		ControllerConsole.style.display = "none";
		button.value = "Show controller state";
	}
}

function show_ui_state() {
    "use strict";
	var button = document.getElementById('show_ui_state_button');
	if (uiConsole.style.display === "none") {
		uiConsole.style.display = "block";
		button.value = "Hide user interface state";
	} else {
		uiConsole.style.display = "none";
		button.value = "Show user interface state";
	}
}

 /**
	 * parses pvs output into a json object map
	 */
function parseState(string) {
    "use strict";
}

var render_value = function (display, val, unit) {
    "use strict";
    display.innerHTML = "";
    display.style.font = "bold 32px Arial";
    display.style.textAlign = "left";
    display.style.color = "white";
    display.style.paddingTop = "6px";

    display.innerHTML = "";
    var maxDigits = 8;
    var maxPrecision = 2;
    var fractionalDigits = 0;
    var disp = val.toString();
    if (disp.length > maxDigits) { disp = disp.substring(0, maxDigits); }
    // display from left to right
    var decimal_place = disp.indexOf(".");
    var cursor_rendered = false;
    var i = 0;
    var stop = false;
    for (i = 0; i < disp.length && !stop; i++) {
        var digit = disp.charAt(i);
        if (digit === ".") {
            digit = bullet;
        }
        var close_span = false;
        if (i > decimal_place && decimal_place > 0) {
            if (fractionalDigits < maxPrecision) {
                display.innerHTML = display.innerHTML + "<span class = decimaldigitSMALL>" + digit + "</span>";
                fractionalDigits++;
            } else { stop = true; }
        } else { display.innerHTML = display.innerHTML + digit; }
    }
    display.innerHTML = display.innerHTML + " " + unit;
};

var read_gpcaui_state = function (e) {
    "use strict";
	// parse the state and initialise corresponding variables
	pvsio_response = prettyprintPVSioOutput(e.data);
	pvsio_response_log("[ " + (new Date()).toTimeString() + " ] ");
	pvsio_response_log(pvsio_response);

	ctrl_cmd = ctrl_cmd_field.exec(pvsio_response)[1];
	ctrl_state = ctrl_state_field.exec(pvsio_response)[1];

	primaryMsg = primaryMsg_field.exec(pvsio_response)[1];
	secondaryMsg = secondaryMsg_field.exec(pvsio_response)[1];

	dePrimaryMsg = dePrimaryMsg_field.exec(pvsio_response)[1];
	deSecondaryMsg = deSecondaryMsg_field.exec(pvsio_response)[1];

	display = eval(display_field.exec(pvsio_response)[1]);
	dispMode = dispMode_field.exec(pvsio_response)[1];
	cursor = eval(cursor_field.exec(pvsio_response)[1]);
	unit = unit_field.exec(pvsio_response)[1];

	block_interaction = block_interaction_field.exec(pvsio_response)[1];

	vtbi_value = eval(vtbi_value_field.exec(pvsio_response)[1]);
	vtbi_unit = vtbi_unit_field.exec(pvsio_response)[1];

	doseRATE_value = eval(doseRATE_value_field.exec(pvsio_response)[1]);
	doseRATE_unit = doseRATE_unit_field.exec(pvsio_response)[1];

	doseRateLowerHardLimit = eval(doseRateLowerHardLimit_field.exec(pvsio_response)[1]);

	doseRateLowerSoftLimit = eval(doseRateLowerSoftLimit_field.exec(pvsio_response)[1]);

	doseRateUpperHardLimit = eval(doseRateUpperHardLimit_field.exec(pvsio_response)[1]);

	doseRateUpperSoftLimit = eval(doseRateUpperSoftLimit_field.exec(pvsio_response)[1]);

	doseRateTypical = eval(doseRateTypical_field.exec(pvsio_response)[1]);

	doseRateUnit = doseRateUnit_field.exec(pvsio_response)[1];

	vtbiLowerHardLimit = eval(vtbiLowerHardLimit_field.exec(pvsio_response)[1]);

	vtbiLowerSoftLimit = eval(vtbiLowerSoftLimit_field.exec(pvsio_response)[1]);

	vtbiUpperHardLimit = eval(vtbiUpperHardLimit_field.exec(pvsio_response)[1]);

	vtbiUpperSoftLimit = eval(vtbiUpperSoftLimit_field.exec(pvsio_response)[1]);

	vtbiTypical = eval(vtbiTypical_field.exec(pvsio_response)[1]);

	vtbiUnit = vtbiUnit_field.exec(pvsio_response)[1];

	drugID = drugID_field.exec(pvsio_response)[1];

    tick_enabled = tick_enabled_field.exec(pvsio_response)[1];
};

var render_limits = function (display, hard_min, hard_max, soft_min, soft_max, unit) {
    "use strict";
	display.style.font = "12px Arial";
	display.style.textAlign = "left";
	display.innerHTML = "<br/>Hard limits (" + hard_min + " - " + hard_max + ") " + unit
				       + "<br/>Soft limits (" + soft_min + " - " + soft_max + ") " + unit;
};

var powerLedOff = function() {
    "use strict";
	var context = powerLed.getContext("2d");
	var centerX = powerLed.width / 2;
	var centerY = powerLed.height / 2;
	context.beginPath();
	context.arc(centerX, centerY, 54, 0, 2*Math.PI, false);
    context.fillStyle = "black";
    context.fill();
	context.stroke();
};

var powerLedOn = function() {
	var context = powerLed.getContext("2d");
	var centerX = powerLed.width / 2;
	var centerY = powerLed.height / 2;
	context.beginPath();
	context.arc(centerX, centerY, 32, 0, 2*Math.PI, false);
    context.fillStyle = "#00FF66"; // light green
    context.fill();
	context.stroke();
}


var clear_display = function() {
	displayFOUR_topline.innerHTML = "";
	displayFOUR_topline.style.background = "transparent";
	displayFOUR_primary.innerHTML = "";
	displayFOUR_primary.style.background = "transparent";
	displayFOUR_primary.style.paddingTop = "0px";
	displayFOUR_secondary.innerHTML = "";
	displayFOUR_secondary.style.background = "transparent";
	displayFOUR_secondary.style.paddingTop = "0px";

	displayTHREE_topline.innerHTML = "";
	displayTHREE_topline.style.background = "transparent";
	displayTHREE_primary.innerHTML = "";
	displayTHREE_primary.style.background = "transparent";
	displayTHREE_primary.style.font = "bold 16px Arial";
	displayTHREE_primary.style.textAlign = "center";
	displayTHREE_primary.style.paddingTop = "0px";
	displayTHREE_secondary.innerHTML = "";
	displayTHREE_secondary.style.background = "transparent";
	displayTHREE_secondary.style.font = "16px Arial";
	displayTHREE_secondary.style.textAlign = "center";
	displayTHREE_secondary.style.paddingTop = "0px";
	displayTHREE_botline.innerHTML = "";
	displayTHREE_botline.style.background = "transparent";

	displayTWO_topline.innerHTML = "";
	displayTWO_topline.style.background = "transparent";
	displayTWO_topline.style.font = "16px Arial";
	displayTWO_secondary.innerHTML = "";
	displayTWO_secondary.style.background = "transparent";
	displayTWO_secondary.style.paddingTop = "0px";
	displayTWO_primary.innerHTML = "";
	displayTWO_primary.style.background = "transparent";
	displayTWO_primary.style.paddingTop = "0px";
	displayTWO_botline.innerHTML = "";
	displayTWO_botline.style.background = "transparent";
	displayTWO_botline.style.font = "bold 24px Arial";
	displayTWO_botline.style.textAlign = "right";

	displayONE_topline.innerHTML = "";
	displayONE_topline.style.background = "transparent";
	displayONE_primary.innerHTML = "";
	displayONE_primary.style.background = "transparent";
	displayONE_primary.style.paddingTop = "0px";
	displayONE_primary.style.font = "bold 16px Arial";
	displayONE_secondary.innerHTML = "";
	displayONE_secondary.style.background = "transparent";
	displayONE_secondary.style.paddingTop = "0px";
	displayONE_secondary.style.font = "bold 16px Arial";
	displayONE_botline.innerHTML = "";
	displayONE_botline.style.background = "transparent";

	powerLedOff();
}

var make_visible = function(display) {
	if(display == "displayONE") {
		displayONE_topline.style.background = "black";
		displayONE_primary.style.background = "black";
		displayONE_secondary.style.background = "black";
		displayONE_botline.style.background = "black";

		displayONE_topline.style.color = "white";
		displayONE_primary.style.color = "white";
		displayONE_secondary.style.color = "white";
		displayONE_botline.style.color = "white";
	}
	if(display == "displayTWO") { // data entry
		displayTWO_topline.style.background = "white";
		displayTWO_primary.style.background = "white";
		displayTWO_secondary.style.background = "white";
		displayTWO_botline.style.background = "white";

		displayTWO_topline.style.color = "black";
		displayTWO_primary.style.color = "black";
		displayTWO_secondary.style.color = "black";
		displayTWO_botline.style.color = "black";
	}
	if(display == "displayTHREE") { // block interaction
		displayTHREE_topline.style.background = "#FF6600";
		displayTHREE_primary.style.background = "#FF6600";
		displayTHREE_secondary.style.background = "#FF6600";
		displayTHREE_botline.style.background = "#FF6600";

		displayTHREE_topline.style.color = "white";
		displayTHREE_primary.style.color = "white";
		displayTHREE_secondary.style.color = "white";
	}
	if(display == "displayFOUR") { // controller notifications
		displayFOUR_topline.style.background = "black";
		displayFOUR_primary.style.background = "black";
		displayFOUR_secondary.style.background = "black";
		displayFOUR_botline.style.background = "black";

		displayFOUR_topline.style.color = "white";
		displayFOUR_primary.style.color = "white";
		displayFOUR_secondary.style.color = "white";
		displayFOUR_botline.style.color = "white";
	}
}

render_text = function(display, text) {
	text = text.replace(new RegExp("___", "g"), "<br/>")
			   .replace(new RegExp("__", "g"), "&nbsp;&nbsp;")
			   .replace(new RegExp("_", "g"), " ");
	display.innerHTML = text;
};

//configuration
require.config({
    //specify base url where require js will start looking when module paths are specified
    baseUrl: "../../../../../../client/app",
    //specify shortcut names for regularly used paths
    paths: {
        "ace": "../lib/ace",
        "d3": "../lib/d3",
        "pvsioweb": "formal/pvs/prototypebuilder"
    }
});

var onOutPutUpdated = function(error, event){
    console_demo.style.display = "none";
    read_gpcaui_state(event);

    // clear display
    clear_display();

    // update power led
    if(ctrl_state == "PowerOff") { powerLedOff(); }
    else { powerLedOn(); }

    // render content of displayFOUR
    if(dispMode == "DrivenByControllerMode") {
        if(primaryMsg != "Empty") {
            // make the display opaque
            make_visible("displayFOUR");
            // render primary message in primary display
            render_text(displayFOUR_primary, primaryMsg);
        }
        if(secondaryMsg != "Empty") {
            // make the display opaque
            make_visible("displayFOUR");
            // render content if non-empty
            render_text(displayFOUR_secondary, secondaryMsg);
        }
    }

    // render content of displayTHREE
    if((dispMode == "DataEntryModeVTBI" || dispMode == "DataEntryModeDoseRate") && block_interaction == "TRUE") {
        // make the display opaque
        make_visible("displayTHREE");

        // render information
        render_text(displayTHREE_primary, dePrimaryMsg);
        render_text(displayTHREE_secondary, deSecondaryMsg);
    }

    // render content of displayTWO
    if((dispMode == "DataEntryModeVTBI" || dispMode == "DataEntryModeDoseRate") && block_interaction == "FALSE") {
        // make the display opaque
        make_visible("displayTWO");

        // topline renders display mode
        render_text(displayTWO_topline, ctrl_state);

        // main display renders value; the cursor is rendered only in data entry mode
        render_dataentry(displayTWO_primary, display);

        if(dispMode == "DataEntryModeVTBI") {
            render_limits(displayTWO_secondary,
                    vtbiLowerHardLimit, vtbiUpperHardLimit,
                    vtbiLowerSoftLimit, vtbiUpperSoftLimit, vtbiUnit);
        }
        else if(dispMode == "DataEntryModeDoseRate") {
            render_limits(displayTWO_secondary,
                    doseRateLowerHardLimit, doseRateUpperHardLimit,
                    doseRateLowerSoftLimit, doseRateUpperSoftLimit, doseRateUnit);
        }

        // botline renders unit
        render_text(displayTWO_botline, unit);
    }

    // render content of displayONE
    if(dispMode == "SettingsSummaryMode") {
        // make the display opaque
        make_visible("displayONE");

        // leave topline bank to avoid cluttering the display content
        render_text(displayONE_topline, "");

        var labels = "";
        var settings = "";

        // render settings in main display
        labels = "VOLUME:&nbsp;<br/>DOSE rate:&nbsp;"
        settings = vtbi_value + " " + vtbi_unit + "<br/>"
                         + doseRATE_value + " " + doseRATE_unit;

        render_text(displayONE_secondary, labels);
        render_text(displayONE_primary, settings);

        // render instruction in botline
        render_text(displayONE_botline, secondaryMsg);
    }

    if(dispMode == "DisplayVTBIMode" || dispMode == "DisplayDoseRATEMode" || dispMode == "DisplayDrugInfoMode") {
        // make the display opaque
        make_visible("displayONE");

        var labels = "";
        var settings = "";

        if(dispMode == "DisplayVTBIMode") {
            displayONE_topline.innerHTML = "Value suggested by the drug library";
            labels = "<br/>VOLUME:&nbsp;"
            settings = "<br/>" + vtbiTypical + " " + vtbiUnit;
        }
        else if(dispMode == "DisplayDoseRATEMode"){
            displayONE_topline.innerHTML = "Value suggested by the drug library";
            labels = "<br/>DOSE rate:&nbsp;"
            settings = "<br/>" + doseRateTypical + " " + doseRateUnit;
        }
        else if(dispMode == "DisplayDrugInfoMode") {
            displayONE_topline.innerHTML = "";
            labels = "The loaded drug is:&nbsp;"
            settings = drugID; // todo: add concentration, amount, diluent
        }

        // render settings in main display
        render_text(displayONE_secondary, labels);
        render_text(displayONE_primary, settings);

        // render instruction in botline
        render_text(displayONE_botline, secondaryMsg);
    }

    if(dispMode == "InfusionNormalOperation" || dispMode == "BolusInProgress") {
        // make the display opaque
        make_visible("displayONE");

        // topline renders display mode
        if(dispMode == "InfusionNormalOperation") { render_text(displayONE_topline, ctrl_state); }
        else if(dispMode == "BolusInProgress") { render_text(displayONE_topline, "Bolus in progress..."); }

        // main display renders value, unit, and message "<Volume infused:"
        render_value(displayONE_primary, display, unit);
        render_text(displayONE_secondary, "<br/>Volume infused:&nbsp;");

        // render instruction in botline
        render_text(displayONE_botline, secondaryMsg);
    }

    // send a command to the controller every time the user interface registers an action from the user
    if(GUI_ACTION == 1) {
        // run the controller
        run_controller();
        GUI_ACTION = 0;
    }
};

require.config({
    baseUrl: "../../../client/app",
    paths: {
        d3: "../../lib/d3",
		"pvsioweb": "../plugins/prototypebuilder",
        "imagemapper": "../../lib/imagemapper",
        "text": "../../lib/text",
        "lib": "../../lib",
        "cm": "../../lib/cm"
    }
});

require(["NavKeys/scripts/gip.js", "PVSioWebClient"], function(_g, PVSioWebClient){

    var client = PVSioWebClient.getInstance();
    ws = client.getWebSocket();

    //register event listener for websocket connection from the client
	client.addListener('WebSocketConnectionOpened', function (e) {
        console_demo.innerHTML = "";
        console_demo.style.display = "block";
        ControllerConsole.style.display = "none";
        uiConsole.style.display = "none";
        log("Loading GPCA model...");
		//start pvs process
		client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "GPCA"}, function (err, event) {
            ws.sendGuiAction("GPCA_init(0);", onOutPutUpdated);
            log("Starting GPCA simulation...");
		});
	}).addListener("WebSocketConnectionClosed", function (e) {
		log("web socket closed");
	}).addListener("processExited", function (e) {
		var msg = "Warning!!!\r\nServer process exited. See console for details.";
		log(msg);
	});

	client.connectToServer();

//
//
//      var url = window.location.origin.indexOf("file") === 0 ?
//            "ws://localhost:8082" : ("ws://" + window.location.hostname + ":8082");
//
//		pvsws()
//		.lastState("GPCA_init(0)")
//		.serverUrl(url)
//		.addListener('ConnectionOpened', function(e){
//
//		}).addListener("ConnectionClosed", function(e){
//			console.style.display = "block";
//			log("Simulator back-end disconnected<br>Please type pvsio-web in a terminal to start the simulator back-end.");
//		}).addListener("InputUpdated", function(e){
//			pvsio_commands_log(JSON.stringify(e.data));
//		}).addListener("pvsoutput", function (e) {
//			ws.lastState(e.data);
//			//ws.value(e.data);
//			onOutPutUpdated(e);
//			console.style.display = "none";
//		}).logon()
//			.then(function (_ws) {
//				ws = _ws;
//				ws.startPVSProcess({fileName: "main.pvs", demoName: "../demos/GPCA"}, function (e, event) {
//					ws.sendGuiAction("GPCA_init(0);", onOutPutUpdated);
//                    log("Starting GPCA simulation...");
//				});
//				console.innerHTML = "";
//				console.style.display = "block";
//				ControllerConsole.style.display = "none";
//				uiConsole.style.display = "none";
//				log("Loading GPCA model...");
//			});
});
