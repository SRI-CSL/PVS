/**
 * @module EmuTimer
 * @desc EmuTimer emulates periodic automatic transitions.
 * @author Paolo Masci
 * @date 2015/04/20
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
define(function (require, exports, module) {
    "use strict";
    var Widget = require("widgets/Widget"),
        d3 = require("d3/d3"),
        property = require("util/property"),
        Timer	= require("util/Timer"),
        Recorder    = require("util/ActionRecorder"),
        ButtonActionsQueue = require("widgets/ButtonActionsQueue").getInstance();
    //define timer for sensing hold down actions on buttons
    var btnTimer = new Timer(250), timerTickFunction = null;
    //add event listener for timer's tick
    btnTimer.addListener('TimerTicked', function () {
        if (timerTickFunction) {
            timerTickFunction();
        }
    });

    function EmuTimer(id, opt) {
        opt = opt || {};
        opt.timerEvent = opt.timerEvent || id;
        opt.timerRate = opt.timerRate || 1000;
        opt.evts = opt.evts || ["tick"];
        this.callback = opt.callback || function () {};
        this.evts = property.call(this, opt.evts);
        this.timerEvent = property.call(this, opt.timerEvent);
        this.timerRate = property.call(this, opt.timerRate);
        this.timerFunction = property.call(this, opt.timerEvent);
        this.imageMap = property.call(this);
        
        Widget.call(this, id, "timer");
        
        var parent = d3.select("map#prototypeMap");
        if (parent.empty()) {
            parent = d3.select("#prototype").append("map").attr("id", "prototypeMap")
                .attr("name", "prototypeMap");
        }
        this.area = parent.append("area")
                        .attr("id", id)
                        .attr("shape", "rect")
                        .attr("href", "#!")
                        .attr("class", id)
                        .attr("coords", "0,0,0,0");
        this.element(this.area);
        this.createImageMap({area: this.area, callback: opt.callback});
        return this;
    }


    EmuTimer.prototype = Object.create(Widget.prototype);
    EmuTimer.prototype.constructor = EmuTimer;
    EmuTimer.prototype.parentClass = Widget.prototype;
    /**
     * @function boundFunctions
     * @returns {String} A comma separated string representing the PVS functions modelling automatic actions.
     * @memberof module:EmuTimer
     */
    EmuTimer.prototype.boundFunctions = function () {
        var o = this;
        var res = o.evts().map(function (d) {
            return d + "_" + o.functionText();
        }).join(", ");
        return res;
    };

    /**
     * Returns a JSON object representation of automatic actions.
     * @returns {object}
     * @memberof module:EmuTimer
    */
    EmuTimer.prototype.toJSON = function () {
        return {
            id: this.id(),
            type: this.type(),
            evts: this.evts(),
            timerEvent: this.timerEvent(),
            timerRate: this.timerRate(),
            timerFunction: this.timerFunction()
        };
    };
    
    /**
     * @function tick
     * @description API to simulate a single timer event
     * @memberof module:EmuTimer
     */
    EmuTimer.prototype.tick = function (opt) {
        opt = opt || {};
        var timerFunction = this.timerFunction(),
            timerEvent = this.timerEvent(),
            id = this.id();
        
        console.log("automatic action fired: " + timerFunction);
        ButtonActionsQueue.queueGUIAction(timerFunction, this.callback);
        Recorder.addAction({
            id: id,
            timerEvent: timerEvent,
            timerFunction: timerFunction,
            ts: new Date().getTime()
        });
        return this;
    };
    
    /**
     * @function start
     * @description API to simulate periodic timer events
     * @memberof module:EmuTimer
     */
    EmuTimer.prototype.start = function (opt) {
        opt = opt || {};
        var timerFunction = this.timerFunction(),
            timerEvent = this.timerEvent(),
            id = this.id();
        var callback = this.callback;
        this.tick(opt);
        if (opt.timerRate && opt.timerRate > 100) {
            this.timerRate(opt.timerRate);
        }
        timerTickFunction = function () {
            ButtonActionsQueue.queueGUIAction(timerFunction, callback);
            //record action
            Recorder.addAction({
                id: id,
                timerEvent: timerEvent,
                timerFunction: timerFunction,
                ts: new Date().getTime()
            });
        };
        btnTimer.interval(this.timerRate()).start();
        return this;
    };
    
    EmuTimer.prototype.updateInterval = function (millis) {
        // minimum allowed interval is 100 millis
        if (millis > 100) {
            this.stop();
            this.start({ timerRate: millis });
        }
    };

    /**
     * @function stop
     * @description API to stop the timer
     * @memberof module:EmuTimer
     */
    EmuTimer.prototype.stop = function (opt) {
        btnTimer.reset();
        return this;
    };
    
    /**
     * @override
     * @function createImageMap
     * @memberof module:EmuTimer
     */
    EmuTimer.prototype.createImageMap = function (opt) {
        opt = opt || {};
        opt.callback = opt.callback || function () {};
        var area = opt.area || EmuTimer.prototype.parentClass.createImageMap.apply(this, arguments);
        this.imageMap(area);
        return area;
    };

    module.exports = EmuTimer;
});
