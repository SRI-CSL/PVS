/**
 * plays a series of recorded user actions
 * @author Patrick Oladimeji
 * @date 3/24/14 17:51:31 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, setTimeout, Promise */
define(function (require, exports, module) {
    "use strict";
    var ws = require("websockets/pvs/WSManager").getWebSocket(),
        WidgetManager = require("pvsioweb/WidgetManager").getWidgetManager(),
        ScriptItemView = require("pvsioweb/forms/ScriptItemView"),
		StateParser = require("util/PVSioStateParser"),
        d3 = require("d3/d3"), playing = false;

    function render(stateString, display) {
		var state = StateParser.parse(stateString);
        display.render(state);
    }

    function getButtonPos(id) {
        var coords = d3.select("." + id).attr("coords");
        coords = coords.split(",");
        return {x1: +coords[0], y1: +coords[1], x2: +coords[2], y2: coords[3]};
    }

    function halo(pos) {
        var w = pos.x2 - pos.x1, hrad = w / 2, h = pos.y2 - pos.y1, vrad = h / 2, brad = hrad + "px " + vrad + "px";
        var mark = d3.select(".animation-halo");
        if (mark.empty()) {
            mark = d3.select("#imageDiv  .prototype-image-inner").append("div").attr("class", "animation-halo");
        }
        mark.style("top", pos.y1 + "px").style("left", pos.x1 + "px")
            .style("width", (pos.x2 - pos.x1) + "px").style("height", (pos.y2 - pos.y1) + "px")
            .style("border-top-left-radius", brad).style("border-top-right-radius", brad)
            .style("border-bottom-left-radius", brad).style("border-bottom-right-radius", brad);
    }

    /**
        recursively call play until all actions have been triggered. The next action in the list
        is called only after a sucessful callback from the server process.
        @param Array<object> actions the list of actions to play
        @param Widget display the display widget where we wish to render the result
        @param Number originalLength the initial length of the actions array (used to calculate the percentage of actions played
        @param HTMLElement el the element that was clicked
    */
    function play(actions, display, originalLength, el) {
        var action = actions.shift();
        var pos = getButtonPos(action.id);
        halo(pos);
        var command = action.action + "_" + action.functionText + "(" + ws.lastState().toString().replace(/,,/g, ",") + ");";
        var percentDone = (originalLength - actions.length) * 100 / originalLength;
        d3.select(el).select(".timeLine").style("width", percentDone + "%");
        ws.sendGuiAction(command, function (err, res) {
            var stateString = res.data.join("");
            render(stateString, display);
            if (actions.length && playing) {
                var interval = actions[0].ts - action.ts;
                setTimeout(function () {
                    play(actions, display, originalLength, el);
                }, interval);
            } else {
                //remove the halo at the end of the actions playback
                d3.select(".animation-halo").remove();
                d3.select(el).select(".timeLine").style("width", "100%");
                d3.select(el).select(".scriptItem").classed("glyphicon-play", true).classed("glyphicon-stop", false);
                playing = false;
            }
        });

    }

    function runTest(test) {
		function sendPVSaction(action) {
			return new Promise(function (resolve, reject) {
				ws.sendGuiAction(action + "(" + ws.lastState().toString().replace(/,,/g, ",") + ");", function (err, res) {
					if (err) { reject(err); }
					else {resolve(res); }
				});
			});
		}

		ws.lastState(test.init);
		var sequence = test.keySequence.map(function (k) {
			return "click_" + k;
		}), lastResult;

		function verifySafety(state) {
			var stateStr = state.data[0].toString();
			var command = test.designIssue.replace("$", stateStr);
			return new Promise(function (resolve, reject) {
				ws.sendGuiAction(command, function (err, res) {
					if (err) {
						reject(err);
					} else {resolve(res.data[0]);}
				});
			});
		}

		function _runTest() {
			var key = sequence.shift();
			return key ? sendPVSaction(key).then(function (res) {
				lastResult = res;
				return _runTest();
			}): verifySafety(lastResult);
		}

		return _runTest();
	}

    /**
        Adds the specified script to the list view
    */
    function addScriptToView(script) {
        d3.select("#emptyPlaceholder").remove();
        var actions = script.actions,
            startState = script.startState;
        var time = actions[actions.length - 1].ts - actions[0].ts;
        script.time = Math.round(time / 1000) + "s";
        script.startState = Array.isArray(startState) ? startState.join("") : startState;

        ScriptItemView.create(script).on("scriptClicked", function () {
            if (!playing) {
                playing = true;
                d3.select(this.el).select(".scriptItem").classed("glyphicon-play", false).classed("glyphicon-stop", true);
                var display = WidgetManager.getAllDisplays()[0];
                ws.lastState(script.startState);
                //render the last state
                if (script.startState !== "init(0)") {
                    render(script.startState, display);
                }
                play(script.actions.map(function (d) {
                    return d;
                }), display, script.actions.length, this.el);
            } else {
                playing = false;
            }
        });
    }

    module.exports = {
        play: play,
        addScriptToView: addScriptToView,
		runTest: runTest,
        clearView: function () {
            d3.select("#scripts ul").html("")
                .append("li").attr("id", "emptyPlaceholder").html("Please use the record button above to record interactions with the user interface prototype.")
                .style("text-align", "center").style("padding", "20px").style("font-size", "0.8em").style("white-space", "normal");
        }
    };
});
