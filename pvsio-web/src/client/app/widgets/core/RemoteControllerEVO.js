/**
 * @module RemoteControllerEVO
 * @version 1.0
 * @description Remote controller, reads input data from gyroscope and/or joystick, and send it to the server using a special message style "controller" and a client ID
 * @author Paolo Masci
 * @date Nov 12, 2018
 *
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, esnext: true */
/*global define */
define(function (require, exports, module) {
    "use strict";
    const WidgetEVO = require("widgets/core/WidgetEVO");

    class RemoteControllerEVO extends WidgetEVO {
        constructor (id, coords, opt) {
            opt = opt || {};
            opt.fontColor = opt.fontColor || "black";
            opt.overflow = "visible";

            coords = coords || { top: 0, left: 0, width: 200, height: 40 };

            // override default style options of WidgetEVO as necessary before creating the DOM element with the constructor of module WidgetEVO
            opt.type = opt.type || "RemoteControllerEVO";
            
            // invoke WidgetEVO constructor to create the widget
            super(id, coords, opt);

            this.ws = opt.ws;
            this.lastData = {};

            let _this = this;
            // this function prevents flooding clients with control data
            this.timer = setInterval(function () {
                if (_this.ws) {
                    _this.ws.ctrl({
                        channelID: "helloworld",
                        gscope: _this.lastData.gscope
                    });
                }
            }, 500);

            if (window.DeviceOrientationEvent) {
                _this.base.text("gscope-pre");
                this._deviceorientation = window.addEventListener("deviceorientation", function (evt) {
                    // console.log(evt);
                    if (evt.gamma) {
                        // beta is the horizontal rotation. Range is between -180 and 180. Zero means axis is flush.
                        // gamma is the vertical axis. Ranges between -90 and 90.
                        _this.base.text(evt.gamma.toFixed(0));
                        _this.lastData = {
                            gscope: {
                                alpha: evt.alpha.toFixed(0),
                                beta: evt.beta.toFixed(0),
                                gamma: evt.gamma.toFixed(0)
                            }
                        };
                    } else {
                        // send dummy value for testing
                        _this.lastData = {
                            gscope: {
                                alpha: 45,
                                beta: 40,
                                gamma: 30
                            }
                        };
                    }
                    // if(evt.beta !== null && evt.gamma !== null){
                    //     // let z = evt.alpha.toFixed(2); // In degree in the range [-360,360]
                    //     let x = evt.beta.toFixed(2); // In degree in the range [-180,180]
                    //     let y = evt.gamma.toFixed(2); // In degree in the range [-90,90]
                    //     if(useSensitivity){
                    //         // sensitivityValue higher than 75% or else the rotation will not be perceptible due to gyroscope sensor optics.        
                    //         GyroscopeController.prototype.rotateSteeringAngleWithSensitivity(x,y,sensitivityValue);
                    //     }else{
                    //         GyroscopeController.prototype.rotateSteeringAngle(x,y);
                    //     }
                    // }
                }, false);
            } else {
                console.error("Gyroscope info not available :/");
            }
    
            return this.reveal();
        }

    }

    module.exports = RemoteControllerEVO;
});
