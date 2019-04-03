/**
 * Player APIs for automated playback of recorded user actions
 * Uses Google Chrome Speech Synthesis APIs for voice feedback -- see https://developers.google.com/web/updates/2014/01/Web-apps-that-talk-Introduction-to-the-Speech-Synthesis-API
 * Available voices:
 *  - de-DE (German)
 *  - en-US (US English)
 *  - en-GP (UK English Female/Male)
 *  - es-ES (Español)
 *  - es-US (Español de Estados Unidos)
 *  - fr-FR (Français)
 *  - hi-IN (हिन्दी)
 *  - id-ID (Bahasa Indonesia)
 *  - it-IT (Italiano)
 *  - ja-JP (日本語)
 *  - ko-KR (한국의)
 *  - nl-NL (Nederlands)
 *  - pl-PL (Polski)
 *  - pt-BR (Português do Brasil)
 *  - ru-RU (русский)
 *  - zh-CN (普通话（中国大陆))
 *  - zh-HK (粤語（香港))
 *  - zh-TW (國語（臺灣))
 *
 * @author Paolo Masci
 * @date Nov 5, 2017
 */
 /*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext:true */
 /*global SpeechSynthesisUtterance */
define(function (require, exports, module) {
    "use strict";
    const SELECT_TIMEOUT = 800; //msec

    function console_log(msg) {
        console.log(msg);
    }

    /**
     * @constructor
     */
    function Player (opt) {
        opt = opt || {};
        this.playlist = {
            seq: [],
            curr: 0
        };
        this.now = 0;
        this.timer = null;
        this.lang = opt.lang || "en-GB";
        this.pitch = opt.pitch || 1.04;
        this.rate = opt.rate || 1.03;
        // log-related attributes
        this._log = [];
        return this;
    }

    /**
     * @function load Loads the list of actions that needs to be played
     * @param actionList {Object} JSON object containing the list of actions to be executed
     *          The object is an array of actions. Each action contains (at least) the following attributes:
     *              - click: the input widget that needs to be clicked
     *              - timestamp: indicates when the action needs to be executed. The value of time is expressed as an absolute value wrt the beginning of time (which is 0)
     */
    Player.prototype.load = function (actionList) {
        this.playlist = {
            seq: actionList,
            curr: 0
        };
        return this;
    };

    Player.prototype.log = function (state) {
        this._log.push({
            time: new Date(),
            state: state
        });
        return this;
    };

    Player.prototype.getLog = function () {
        return this._log;
    };

    //---  utility functions  ----
    function viz(id, opt) {
        // console_log("revealing " + id);
        opt = opt || {};
        opt.duration = opt.duration || 300;
        if (d3.select(id).node()) {
            if (opt.fade && d3.select(id).style("display") !== "block") {
                d3.select(id).style("opacity", 0).transition().duration(opt.duration).style("opacity", 1).style("display", "block");
            } else {
                d3.select(id).transition().duration(0).style("opacity", 1).style("display", "block");
            }
        }
    }
    function hide(id) {
        // console_log("hiding " + id);
        d3.select(id).style("display", "none");
    }

    /**
     * @function play Plays the action file opened in the player
     */
    Player.prototype.play = function(opt) {
        opt = opt || {};
        function select_widget () {
            if (action.deselect) {
                theWidget.deselect();
            } else {
                theWidget.select({
                    borderColor: action.borderColor || "white",
                    classed: action.classed
                });
            }
        }
        function click_widget () {
            if (action.click) {
                theWidget.click({
                    borderColor: opt.borderColor || "white"
                });
            }
        }
        function cursor_move () {
            action.cursor.offset = action.cursor.offset || {};
            var yy = (isNaN(parseFloat(action.cursor.offset.top))) ?
                        (theWidget.getSize().height * 0.2)
                        : parseFloat(action.cursor.offset.top);
            var xx = (isNaN(parseFloat(action.cursor.offset.left))) ?
                        (theWidget.getSize().width * 0.8)
                        : parseFloat(action.cursor.offset.left);
            action.cursor.type.move({
                top: theWidget.getPosition().top + yy,
                left: theWidget.getPosition().left + xx
            }, { duration: action.cursor.speed || 1000 });
        }
        function cursor_click () {
            action.cursor.type.click({ fw_move: Math.min(theWidget.getSize().height / 4, theWidget.getSize().width / 4) });
            window.setTimeout(function () {
                select_widget();
            }, 250);
            window.setTimeout(function () {
                click_widget();
            }, timeout);
        }
        function cursor_longpress () {
            action.cursor.type.longPress();
            window.setTimeout(function () {
                select_widget();
            }, 250);
            window.setTimeout(function () {
                click_widget();
            }, timeout);
        }
        try {
            if (this.playlist.curr < this.playlist.seq.length) {
                console_log("Playback: action " + (this.playlist.curr + 1) + " of " + this.playlist.seq.length);
                var _this = this;
                var action = this.playlist.seq[this.playlist.curr];
                var duration = action.duration || 1000;
                var transitionTimingFunction = opt.transitionTimingFunction || "ease-out";
                var timeout = (action.timeout && action.timeout >= 0)? action.timeout : SELECT_TIMEOUT;
                var when = parseFloat(action.timeStamp - _this.now);
                if (when < 0) {
                    console.error("Timestamp is out of order");
                    when = 0;
                }
                let skip_speech = false;
                if (opt.from && !isNaN(parseFloat(opt.from))) {
                    when = when - (parseFloat(opt.from) - _this.now);
                    if (when < 0) {
                        when = 0;
                        skip_speech = true;
                    }
                }

                if ((action.hide || action.reveal) && !isNaN(when) && when >= 0) {
                    _this.timer = window.setTimeout(function () {
                        if (action.hide) {
                            if (action.hide.widget) {
                                action.hide.hide();
                            } else {
                                console_log("Hiding " + action.hide);
                                hide(action.hide);
                            }
                        } else if (action.reveal){
                            if (action.reveal.widget) {
                                action.reveal.reveal();
                            } else {
                                console_log("Revealing " + action.reveal);
                                viz(action.reveal, { fade: true, duration: 1000 });
                            }
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if ((action.click || action.select || action.deselect) && !isNaN(when) && when >= 0) {
                    var theWidget = action.click || action.select || action.deselect;
                    _this.timer = window.setTimeout(function () {
                        if (action.cursor && action.cursor.type) {
                            cursor_move();
                            window.setTimeout(function () {
                                if (action.cursor.longpress) {
                                    cursor_longpress();
                                } else {
                                    cursor_click();
                                }
                            }, action.cursor.speed || 1250);
                        } else {
                            select_widget();
                            window.setTimeout(function () {
                                click_widget();
                            }, timeout);
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.speak && !isNaN(when) && when >= 0) {
                    if (skip_speech) {
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    } else {
                        _this.timer = window.setTimeout(function () {
                            var msg = new SpeechSynthesisUtterance(action.speak);
                            msg.lang = _this.lang;
                            msg.localService = true;
                            msg.rate = _this.rate;
                            msg.pitch = _this.pitch;
                            console_log("Speaking: " + action.speak);
                            window.speechSynthesis.speak(msg);
                            _this.now = action.timeStamp;
                            _this.playlist.curr++;
                            _this.play(opt);
                        }, when);
                    }
                }
                if (action.input && !isNaN(when) && when >= 0) {
                    console.log("input " + action.input + ": " + action.value);
                    _this.timer = window.setTimeout(function () {
                        if (d3.select(action.input).node()) {
                            fill(action.input, action.value, {
                                    timeStamp: action.timeStamp,
                                    lineFeed: action.lineFeed
                                });
                            if (action.scroll) {
                                scrollTop(action.scroll.id, action.scroll.offset);
                            }
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.scroll && !isNaN(when) && when >= 0) {
                    console.log("scrolling " + action.scroll + " by " + action.offset + "px");
                    _this.timer = window.setTimeout(function () {
                        scrollTop(action.scroll, action.offset);
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.trans && !isNaN(when) && when >= 0) {
                    _this.timer = window.setTimeout(function () {
                        let zIndex = action.zIndex || "inherit";
                        if (action.trans.startsWith(".")) {
                            d3.selectAll(action.trans).style("z-index", zIndex).style("display", "block").style("opacity", 1).style("transform", action.transform).style("transition-duration", duration + "ms");
                        } else {
                            d3.select(action.trans).style("z-index", zIndex).style("display", "block").style("opacity", 1).style("transform", action.transform).style("transition-duration", duration + "ms");
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.reveal && !isNaN(when) && when >= 0) {
                    _this.timer = window.setTimeout(function () {
                        if (action.reveal.widget && typeof action.reveal.reveal === "function") {
                            action.reveal.reveal();
                        } else if (typeof action.reveal === "string") {
                            let opacity = isNaN(parseFloat(action.opacity)) ? 1 : action.opacity;
                            if (action.reveal.startsWith(".")) {
                                d3.selectAll(action.reveal).style("display", "block").style("opacity", opacity)
                                    .style("transition-duration", duration + "ms")
                                    .style("transition-timing-function", transitionTimingFunction);
                            } else {
                                d3.select(action.reveal).style("display", "block").style("opacity", opacity)
                                    .style("transition-duration", duration + "ms")
                                    .style("transition-timing-function", transitionTimingFunction);
                            }
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.move && !isNaN(when) && when >= 0) {
                    _this.timer = window.setTimeout(function () {
                        if (action.move.widget && typeof action.move.move === "function") {
                            action.move.move({ top: action.top, left: action.left }, {
                                duration: duration,
                                transitionTimingFunction: transitionTimingFunction
                            });
                        } else if (typeof action.move === "string") {
                            d3.select(action.move).style("display", "block").style("opacity", 1)
                                .style("top", action.top + "px").style("left", action.left + "px")
                                .style("transition-duration", duration + "ms")
                                .style("transition-timing-function", transitionTimingFunction);
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
                if (action.blink && !isNaN(when) && when >= 0) {
                    _this.timer = window.setTimeout(function () {
                        if (d3.select(action.blink).node()) {
                            d3.select(action.blink).classed("blink");
                        }
                        _this.now = action.timeStamp;
                        _this.playlist.curr++;
                        _this.play(opt);
                    }, when);
                }
            }
        } catch(play_error) {
            console.error(play_error);
        }
        return this;
    };

    function scrollTop(id, scrollHeight) {
        function scrollTopTween(scrollTop) {
            return function() {
                var i = d3.interpolateNumber(this.scrollTop, scrollTop);
                return function(t) { this.scrollTop = i(t); };
            };
        }
        d3.select(id).transition()
            .duration(500)
            .tween("PlayerScroller", scrollTopTween(scrollHeight));
    }

    function fill(id, val, opt) {
        opt = opt || {};
        if (val && typeof val === "string") {
            if (opt.lineFeed) {
                d3.select(id).attr("value", val);
                d3.select(id).text(val);
            } else {
                var current_value = d3.select(id).attr("value") || d3.select(id).text();
                var elapse = opt.delay || 250;
                val.split("").forEach(function (c) {
                    setTimeout(function () {
                        // for input fields
                        d3.select(id).attr("value", current_value + c);
                        // for text areas & DIVs
                        d3.select(id).text(current_value + c);
                        // console_log(current_value);
                        current_value = d3.select(id).attr("value");
                    }, elapse);
                    elapse += (c === "@")? 400 : (Math.random() * (150 - 200) + 100);
                });
            }
        }
    }


    /**
     * @function pause Pauses playback
     */
    Player.prototype.pause = function() {
        clearInterval(this.timer);
        return this;
    };

    /**
     * @function stop Stops playback
     */
    Player.prototype.stop = function() {
        this.pause();
        this.playlist.curr = 0;
        return this;
    };

    module.exports = Player;
});
