/**
 * @module Button
 * @desc Button Widget
 * @author Patrick Oladimeji
 * @date 10/31/13 11:26:16 AM
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

    function mouseup(e) {
        btnTimer.reset();
    }

    function Button(id, coords, opt) {
        opt = opt || {};
        opt.functionText = opt.functionText || id;
        opt.recallRate = opt.recallRate || 250;
        opt.evts = opt.evts || ["click"];
        this.evts = property.call(this, opt.evts);
        this.recallRate = property.call(this, opt.recallRate);
        this.functionText = property.call(this, opt.functionText);
        this.imageMap = property.call(this);

        Widget.call(this, id, "button");

        var parent = d3.select("map#prototypeMap");
        if (parent.empty()) {
            parent = d3.select("#prototype").append("map").attr("id", "prototypeMap")
                .attr("name", "prototypeMap");
        }

        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 32;
        this.height = coords.height || 32;

        this.area = opt.area || parent.append("area")
                        .attr("coords", this.left + "," + this.top + ","
                              + (this.left + this.width) + "," + (this.top + this.height))
                        .attr("id", id)
                        .attr("class", id);

        this.createImageMap({area: this.area, callback: opt.callback});
        return this;
    }


    Button.prototype = Object.create(Widget.prototype);
    Button.prototype.constructor = Button;
    Button.prototype.parentClass = Widget.prototype;
    /**
     * @function boundFunctions
     * @returns {String} A comma separated string representing the PVS functions modelling actions over this button.
     * @memberof module:Button
     */
    Button.prototype.boundFunctions = function () {
        var o = this;
        var res = o.evts().map(function (d) {
            if (d.indexOf("/") > -1) {
                return d.split("/").map(function (a) {
                    return a + "_" + o.functionText();
                }).join(", ");

            } else {
                return d + "_" + o.functionText();
            }
        }).join(", ");
        return res;
    };

    /**
     * Returns a JSON object representation of this Button.
     * @returns {object}
     * @memberof module:Button
    */
    Button.prototype.toJSON = function () {
        return {
            evts: this.evts(),
            id: this.id(),
            type: this.type(),
            recallRate: this.recallRate(),
            functionText: this.functionText(),
            boundFunctions: this.boundFunctions()
        };
    };

    /**
     * @function release
     * @description API to simulate a release action on the button
     * @memberof module:Button
     */
    Button.prototype.release = function (opt) {
        opt = opt || {};
        var f = this.functionText();
        
        ButtonActionsQueue.queueGUIAction("release_" + f, opt.callback);
        Recorder.addAction({
            id: this.id(),
            functionText: this.functionText(),
            action: "release",
            ts: new Date().getTime()
        });
        mouseup(d3.event);
        return this;
    };
    
    /**
     * @function press
     * @description API to simulate a single press action on the button
     * @memberof module:Button
     */
    Button.prototype.press = function (opt) {
        opt = opt || {};
        var f = this.functionText();
        
        ButtonActionsQueue.queueGUIAction("press_" + f, opt.callback);
        Recorder.addAction({
            id: this.id(),
            functionText: this.functionText(),
            action: "press",
            ts: new Date().getTime()
        });
        return this;
    };
    
    /**
     * @function pressAndHold
     * @description API to simulate a continuous press action on the button
     * @memberof module:Button
     */
    Button.prototype.pressAndHold = function (opt) {
        opt = opt || {};
        var f = this.functionText(),
        widget = this;
        
        this.press(opt);
        timerTickFunction = function () {
            console.log("timer ticked_" + f);
            ButtonActionsQueue.queueGUIAction("press_" + f, opt.callback);
            //record action
            Recorder.addAction({
                id: widget.id(),
                functionText: widget.functionText(),
                action: "press",
                ts: new Date().getTime()
            });
        };
        btnTimer.interval(this.recallRate()).start();        
        
        return this;
    };
    
    /**
     * @function click
     * @description API to simulate a click action on the button
     * @memberof module:Button
     */
    Button.prototype.click = function (opt) {
        opt = opt || {};
        var f = this.functionText();
        ButtonActionsQueue.queueGUIAction("click_" + f, opt.callback);
        Recorder.addAction({
            id: this.id(),
            functionText: this.functionText(),
            action: "click",
            ts: new Date().getTime()
        });
        return this;
    };
    
    /**
     * @override
     * @function createImageMap
     * @description Creates an image map area for this button and binds functions in the button's events property with appropriate
     * calls to function in the PVS model. Whenever a response is returned from the PVS function call, the callback
     * function is invoked.
     * @param {!pvsWSClient} ws A websocket client to use for sending gui actions to the server process
     * @param {function} callback A callback function to invoke when the pvs function call on the server process is returned
     * @returns {d3.selection} The image map area created for the button
     * @memberof Button
     */
    Button.prototype.createImageMap = function (opt) {
        opt = opt || {};
        opt.callback = opt.callback || function () {};

        var area = opt.area || Button.prototype.parentClass.createImageMap.apply(this, arguments),
            widget = this,
            f,
            evts;

        var onmouseup = function () {
            if (evts && evts.indexOf("press/release") > -1) {
                widget.release(opt);
            }
            mouseup(d3.event);
            area.on("mouseup", null);
        };
        area.on("mousedown", function () {
            f = widget.functionText();
            evts = widget.evts();
            //perform the click event if there is one
            if (evts && evts.indexOf('click') > -1) {
                widget.click(opt);
            } else if (evts && evts.indexOf("press/release") > -1) {
                widget.pressAndHold(opt);
            }
            //register mouseup/out events here
            area.on("mouseup", onmouseup);
        });
        widget.imageMap(area);
        return area;
    };

    module.exports = Button;
});
