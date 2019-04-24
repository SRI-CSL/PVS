/**
 * Simple Playback Player for Alloy traces
 * @author Paolo Masci
 * @date Oct 8, 2017
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global define, Promise */
define(function (require, exports, module) {
    "use strict";
    var d3 = require("d3/d3");
    var _instance;

    function AlloyScriptPlayer () {
        return this;
    }

    function render (_this) {
        d3.select(".pim-screen-controls .btn-playback-current-state span").node().innerHTML = "State " + _this.current_state;
        if (_this.current_state < _this.states.length) {
            var keys = Object.keys(_this.states[_this.current_state]);
            keys.forEach(function (key) {
                if (_this.states[_this.current_state][key].transform) {
                    d3.select(".pim-prototyper .overlay-images")
                        .select("#" + key)
                        .style("transition", "transform 0.3s")
                        .style("transform", _this.states[_this.current_state][key].transform);
                }
            });
        }
    }

    AlloyScriptPlayer.prototype.openScriptDialog = function () {
        var _this = this;
        return new Promise (function (resolve, reject) {
            require("filesystem/FileSystem").getInstance().readFileDialog({
                title: "Select a playback script",
                filter: require("util/MIME").getInstance().filter([".json"])
            }).then(function (descriptors) {
                if (descriptors && descriptors[0] && descriptors[0].content) {
                    var script = JSON.parse(descriptors[0].content);
                    _this.states = script.states;
                    _this.current_state = 0;
                    render(_this);
                    console.log(script);
                    return resolve(script);
                }
                reject({ msg: "Failed to load script"});
                console.log("Failed to load script");
            }).catch(function (err) { reject(err); });
        });
    };

    AlloyScriptPlayer.prototype.stepForward = function () {
        if (this.states && this.current_state < (this.states.length - 1)) {
            this.current_state++;
            render(this);
        }
        return this;
    };

    AlloyScriptPlayer.prototype.stepBackward = function () {
        if (this.states && this.current_state > 0) {
            this.current_state--;
            render(this);
        }
        return this;
    };

    AlloyScriptPlayer.prototype.pause = function () {
        this.animationPaused = true;
        return this;
    };

    function runAnimation (_this) {
        if (!_this.animationPaused && _this.states && _this.current_state < (_this.states.length - 1)) {
            _this.stepForward();
            window.setTimeout(function () {
                if (!_this.animationPaused) {
                    runAnimation(_this);
                }
            }, 1000);
        }
    }

    AlloyScriptPlayer.prototype.play = function () {
        this.animationPaused = false;
        runAnimation(this);
        return this;
    };

    module.exports = {
        getInstance: function () {
            _instance = _instance || new AlloyScriptPlayer();
            return _instance;
        }
    };
});
