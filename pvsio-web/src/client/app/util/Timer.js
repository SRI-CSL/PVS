/**
 * @author hogfather
 * @date Feb 29, 2012
 */
/**
 * Creates a timer object that can be easily used to manage timing functions
 *
 * @param interval
 * @param repeatCount
 * @param mode
 *            can be either interval or timeout
 * @returns {Timer}
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define, setTimeout, setInterval, clearInterval*/
define(function (require, exports, module) {
    "use strict";
    var timerConstants              = require("util/timerConstants"),
        eventDispatcher             = require("util/eventDispatcher");

    module.exports = function (i, repeatCount, mode) {
        var initialInterval = i;
        var _interval = i;
        var _repeatCount = repeatCount || Number.POSITIVE_INFINITY;
        var _mode = mode || timerConstants.Interval;
        var _currentCount = 0;
        var timerid = -1;
        var running = false;
        var event = {}, res = {};
        
        function timerTick() {
            if (!running) { return; }
    
            if (_currentCount < _repeatCount) {
                _currentCount++;
                event = {
                    type: timerConstants.TimerTicked,
                    currentCount: _currentCount
                };
                res.fire(event);
                if (_currentCount < _repeatCount) {
                    // recall the setTimeout method if the timer is in timeout mode
                    if (_mode === timerConstants.Timeout) {
                        timerid = setTimeout(timerTick, _interval);
                    }
                } else {
                    clearInterval(timerid);
                    event = {
                        type: timerConstants.TimerFinished,
                        currentCount: _currentCount
                    };
                    res.fire(event);
                }
            } else {
                clearInterval(timerid);
                event = {
                    type : timerConstants.TimerFinished,
                    currentCount : _currentCount
                };
                res.fire(event);
            }
        }

        function start() {
            if (!running) {
                switch (_mode) {
                case timerConstants.Interval:
                    timerid = setInterval(timerTick, _interval);
                    break;
                case timerConstants.Timeout:
                    timerid = setTimeout(timerTick, _interval);
                    break;
                }
                running = true;
            }
        }
        
        function interval(val) {
            if (val !== undefined) {
                _interval = val;
            }
            return _interval;
        }

        function stop() {
            if (running) {
                running = false;
                clearInterval(timerid);
            }
        }
        
        res = {
            getCurrentCount: function () {
                return _currentCount;
            },
            start: function () {
                start();
                return this;
            },
            interval: function (v) {
                if (v !== undefined) {
                    interval(v);
                    return this;
                }
                return interval();
            },
            stop: function () {
                stop();
                return this;
            },
            reset: function () {
                stop();
                _currentCount = 0;
                _interval = initialInterval;
                return this;
            },
    
            restart: function () {
                stop();
                start();
                return this;
            }
            
        };
        eventDispatcher(res);
        return res;
    };
});
