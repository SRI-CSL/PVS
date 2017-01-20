/**
 * @module Button
 * @desc Button Widget
 * @author Patrick Oladimeji
 * @date 10/31/13 11:26:16 AM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50*/
/*global define*/
define(function (require, exports, module) {
    "use strict";
    var Widget = require("widgets/Widget"),
        d3 = require("d3/d3"),
        property = require("util/property"),
        Timer	= require("util/Timer"),
        Recorder    = require("util/ActionRecorder"),
        Speaker  = require("widgets/TextSpeaker"),
        StateParser = require("util/PVSioStateParser"),
        ButtonActionsQueue = require("widgets/ButtonActionsQueue").getInstance(),
        ButtonHalo = require("widgets/ButtonHalo").getInstance();
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
        opt.callback = opt.callback || function () {};
        opt.buttonReadback = opt.buttonReadback || "";
        opt.keyCode = opt.keyCode || "";
        opt.keyName = opt.keyName || "";
        opt.animation = opt.animation || function () {};
        opt.visibleWhen =  (!opt.visibleWhen || opt.visibleWhen === "") ? "true" : opt.visibleWhen;
        coords = coords || {};
        this.functionText = property.call(this, opt.functionText);
        this.customFunctionText = property.call(this, opt.customFunctionText);
        this.recallRate = property.call(this, opt.recallRate);
        this.evts = property.call(this, opt.evts);
        this.callback = opt.callback;
        this.imageMap = property.call(this);
        this.buttonReadback = property.call(this, opt.buttonReadback);
        this.keyCode = property.call(this, opt.keyCode);
        this.keyName = property.call(this, opt.keyName);
        this.animation = opt.animation;
        this.visibleWhen = property.call(this, opt.visibleWhen);
        this.cursor = opt.cursor || "pointer";

        Widget.call(this, id, "button");
        opt.parent = opt.parent || "prototype";
        opt.prototypeMap = opt.prototypeMap || "prototypeMap";

        var parent = d3.select("map#" + opt.prototypeMap);
        if (parent.empty()) {
            parent = d3.select("#" + opt.parent).append("map").attr("id", opt.prototypeMap)
                .attr("name", opt.prototypeMap);
        }
        this.prototypeMap = opt.prototypeMap;

        this.top = coords.top || 0;
        this.left = coords.left || 0;
        this.width = coords.width || 32;
        this.height = coords.height || 32;

        this.area = (opt.area) ? opt.area.append("area") : parent.append("area");
        var x2 = this.left + this.width;
        var x3 = this.top + this.height;
        this.area.attr("shape", "rect").attr("id", id).attr("class", id)
                 .attr("coords", this.left + "," + this.top + "," + x2 + "," + x3)
                 .style("cursor", this.cursor);

        this.createImageMap({ area: this.area, callback: this.callback });
        if (opt.keyCode) {
            ButtonHalo.installKeypressHandler(this, opt.keyCode);
        }
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
        var res = "";
        if (o.evts() && o.evts().length === 1 && o.evts()[0] === "custom") {
            res = o.customFunctionText();
        } else {
            res = o.evts().map(function (d) {
                if (d.indexOf("/") > -1) {
                    return d.split("/").map(function (a) {
                        return a + "_" + o.functionText();
                    }).join(", ");

                } else {
                    return d + "_" + o.functionText();
                }
            }).join(", ");
        }
        return res;
    };

    /**
     * Returns a JSON object representation of this Button.
     * @returns {object}
     * @memberof module:Button
    */
    Button.prototype.toJSON = function () {
        return {
            id: this.id(),
            type: this.type(),
            evts: this.evts(),
            recallRate: this.recallRate(),
            functionText: this.functionText(),
            customFunctionText: this.customFunctionText(),
            boundFunctions: this.boundFunctions(),
            buttonReadback: this.buttonReadback(),
            keyCode: this.keyCode(),
            keyName: this.keyName(),
            visibleWhen: this.visibleWhen()
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
        var anim = opt.animation || this.animation || function () {};
        opt.callback = opt.callback || this.callback;

        ButtonActionsQueue.queueGUIAction("release_" + f, opt.callback);
        anim();
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
     * @function hide
     * @description API to hide the button (disable actions & restore default mouse cursor)
     * @memberof module:Button
     */
    Button.prototype.hide = function (opt) {
        opt = opt || {};
        opt.prototypeMap = opt.prototypeMap || this.prototypeMap;
        this.cursor = opt.cursor || "default";
        return this.removeImageMap();
    };

    /**
     * @function reveal
     * @description API to reveal the button (disable actions & restore default mouse cursor)
     * @memberof module:Button
     */
    Button.prototype.reveal = function (opt) {
        opt = opt || {};
        opt.prototypeMap = opt.prototypeMap || this.prototypeMap;
        this.cursor = opt.cursor || "pointer";
        if (d3.select("#" + opt.prototypeMap) && d3.select("#" + opt.prototypeMap).node()) {
            if (!d3.select("#" + opt.prototypeMap).select("." + this.id()).node()) {
                // else, re-attach map area and event listeners
                d3.select("#" + opt.prototypeMap).node().append(this.area.node());
                return this.createImageMap({ area: this.area, callback: this.callback });
            }
        }
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
        var anim = opt.animation || this.animation || function () {};
        opt.callback = opt.callback || this.callback;

        ButtonActionsQueue.queueGUIAction("press_" + f, opt.callback);
        anim();
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
        var anim = opt.animation || this.animation || function () {};
        opt.callback = opt.callback || this.callback;

        this.press(opt);
        timerTickFunction = function () {
            console.log("timer ticked_" + f);
            ButtonActionsQueue.queueGUIAction("press_" + f, opt.callback);
            anim();
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
        var anim = opt.animation || this.animation || function () {};
        opt.callback = opt.callback || this.callback;

        if (this.customFunctionText()) {
            ButtonActionsQueue.queueGUIAction(this.customFunctionText(), opt.callback);
        } else {
            ButtonActionsQueue.queueGUIAction("click_" + this.functionText(), opt.callback);
        }
        anim();
        Recorder.addAction({
            id: this.id(),
            functionText: this.functionText(),
            action: "click",
            ts: new Date().getTime()
        });
        if (this.buttonReadback() && this.buttonReadback() !== "") {
            Speaker.speak(this.buttonReadback());
        }
        return this;
    };

    /**
     * @function render
     * @description API for updating properties of the button, e.g., whether it's enabled
     * @memberof module:Button
     */
    Button.prototype.render = function (txt, opt) {
        opt = opt || {};
        txt = txt || "";
        if (typeof txt === "object") {
            var expr = StateParser.simpleExpressionParser(this.visibleWhen());
            if (expr && expr.res) {
                if (expr.res.type === "constexpr" && expr.res.constant === "true") {
                    return this.reveal();
                } else if (expr.res.type === "boolexpr" && expr.res.binop) {
                    // txt in this case is a PVS state that needs to be parsed
                    var str = StateParser.resolve(txt, expr.res.attr);
                    if (str) {
                        str = StateParser.evaluate(str);
                        if ((expr.res.binop === "=" && str === expr.res.constant) ||
                             (expr.res.binop === "!=" && str !== expr.res.constant)) {
                                 return this.reveal();
                        }
                    }
                }
            }
        }
        return this.hide();
    };
    Button.prototype.renderSample = function (opt) {
        opt = opt || {};
        var txt = opt.txt || this.example;
        return this.render(txt, { visibleWhen: "true" });
    };


    /**
     * @override
     * @function removeImageMap
     * @description Removes the image map area for this button
     * @returns this
     * @memberof Button
     */
    Button.prototype.removeImageMap = function (opt) {
        opt = opt || {};
        opt.prototypeMap = opt.prototypeMap || this.prototypeMap;
        if (d3.select("#" + opt.prototypeMap).node() && d3.select("#" + opt.prototypeMap).select("." + this.id()).node()) {
            d3.select("#" + opt.prototypeMap).select("." + this.id()).node().remove();
        }
        return this;
    };


    /**
     * @override
     * @function createImageMap
     * @description Creates an image map area for this button and binds functions in the button's events property with appropriate
     * calls to function in the PVS model. Whenever a response is returned from the PVS function call, the callback
     * function is invoked.
     * @returns this
     * @memberof Button
     */
    Button.prototype.createImageMap = function (opt) {
        opt = opt || {};
        opt.callback = opt.callback || this.callback;
        opt.prototypeMap = opt.prototypeMap || this.prototypeMap;

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
            if (evts && (evts.indexOf('click') >= 0 || evts.indexOf("custom") >= 0)) {
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
