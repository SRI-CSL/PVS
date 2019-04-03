/**
 *
 * @author Paolo Masci, Patrick Oladimeji
 * @date 27/03/15 20:30:33 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
require.config({
    baseUrl: "../../client/app",
    paths: {
        d3: "../lib/d3",
        "pvsioweb": "plugins/prototypebuilder",
        "imagemapper": "../lib/imagemapper",
        "text": "../lib/text",
        "lib": "../lib",
        "cm": "../lib/cm",
        stateParser: './util/PVSioStateParser'
    }
});

require([
        "widgets/core/ButtonEVO",
        "widgets/core/NumericDisplayEVO",
        "widgets/LED",
        "widgets/LED2",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"],
    function (TouchscreenButton,
              BasicDisplay,
              LED,
              LED2,
              ButtonActionsQueue,
              stateParser,
              PVSioWebClient) {
        "use strict";
        var d3 = require("d3/d3");
        var client = PVSioWebClient.getInstance();

        // utility function
        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            return str;
        }

        function animate_reset1 () {
            d3.select("#btn1").transition().duration(200).style("opacity", 0.2);
            d3.select("#btn1_forward").transition().duration(200).style("opacity", 1);
            window.setTimeout(function () {
                d3.select("#btn1").transition().duration(200).style("opacity", 1);
                d3.select("#btn1_forward").transition().duration(200).style("opacity", 0);
            }, 200);
        }
        function animate_reset2 () {
            d3.select("#btn2_up").transition().duration(200).style("opacity", 0.2);
            d3.select("#btn2_down").transition().duration(200).style("opacity", 1);
            window.setTimeout(function () {
                d3.select("#btn2_up").transition().duration(200).style("opacity", 1);
                d3.select("#btn2_down").transition().duration(200).style("opacity", 0);
            }, 200);
        }
        function animate_reset3 () {
            d3.select("#btn3_up").transition().duration(200).style("opacity", 0.2);
            d3.select("#btn3_down").transition().duration(200).style("opacity", 1);
            window.setTimeout(function () {
                d3.select("#btn3_up").transition().duration(200).style("opacity", 1);
                d3.select("#btn3_down").transition().duration(200).style("opacity", 0);
            }, 200);
        }
        function onMessageReceived_reset3 (err, res) {
            animate_reset3();
            onMessageReceived(err, res);
        }
        function onMessageReceived_reset2 (err, res) {
            animate_reset2();
            onMessageReceived(err, res);
        }
        function onMessageReceived_reset1 (err, res) {
            animate_reset1();
            onMessageReceived(err, res);
        }

        var device = { };

        device.disp1 = new BasicDisplay("disp1", {
            top: 413,
            left: 322,
            width: 316,
            height: 16
        }, {
            parent: "facit",
            backgroundColor: "transparent",
            fontColor: "white",
            align: "right",
            maxIntegerDigits: 16,
            maxDecimalDigits: 0,
            letterSpacing: 20.1
        });
        device.disp2 = new BasicDisplay("disp2", {
            top: 266,
            left: 681,
            width: 176,
            height: 16
        }, {
            parent: "facit",
            backgroundColor: "transparent",
            fontColor: "white",
            align: "right",
            maxIntegerDigits: 9,
            maxDecimalDigits: 0,
            letterSpacing: 21
        });
        device.disp3 = new BasicDisplay("disp3", {
            top: 266,
            left: 322,
            width: 308,
            height: 16
        }, {
            parent: "facit",
            backgroundColor: "transparent",
            fontColor: "white",
            align: "right",
            maxIntegerDigits: 16,
            maxDecimalDigits: 0,
            letterSpacing: 20
        });

        device.key1 = new TouchscreenButton("key1", {
            top: 693,
            left: 474,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 49
        });
        device.key2 = new TouchscreenButton("key2", {
            top: 694,
            left: 538,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 50
        });
        device.key3 = new TouchscreenButton("key3", {
            top: 695,
            left: 603,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 51
        });
        device.key4 = new TouchscreenButton("key4", {
            top: 622,
            left: 478,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 52
        });
        device.key5 = new TouchscreenButton("key5", {
            top: 623,
            left: 541,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 53
        });
        device.key6 = new TouchscreenButton("key6", {
            top: 624,
            left: 604,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 54
        });
        device.key7 = new TouchscreenButton("key7", {
            top: 556,
            left: 480,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 55
        });
        device.key8 = new TouchscreenButton("key8", {
            top: 556,
            left: 542,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 56
        });
        device.key9 = new TouchscreenButton("key9", {
            top: 557,
            left: 604,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 57
        });
        device.key0 = new TouchscreenButton("key0", {
            top: 766,
            left: 472,
            width: 172,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 48
        });
        device.shift_left = new TouchscreenButton("shift_left", {
            top: 684,
            left: 312,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 37 // arrow left
        });
        device.shift_right = new TouchscreenButton("shift_right", {
            top: 684,
            left: 374,
            width: 40,
            height: 60
        }, {
            parent: "numberpad",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 39 // arrow right
        });
        // device.shift_max = new TouchscreenButton("shift_max", {
        //     top: 684,
        //     left: 701,
        //     width: 40,
        //     height: 60
        // }, {
        //     parent: "numberpad",
        //     callback: onMessageReceived,
        //     backgroundColor: "transparent"
        //     //keyCode: ...//
        // });
        device.mul = new TouchscreenButton("mul", {
            top: 398,
            left: 1080,
            width: 90,
            height: 100
        }, {
            parent: "facit",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 38 // arrow up
        });
        device.sub = new TouchscreenButton("sub", {
            top: 568,
            left: 1080,
            width: 90,
            height: 110
        }, {
            parent: "facit",
            callback: onMessageReceived,
            backgroundColor: "transparent",
            keyCode: 40 // arrow down
        });
        device.reset3 = new TouchscreenButton("reset3", {
            top: 682,
            left: 24,
            width: 60,
            height: 165
        }, {
            parent: "facit",
            callback: onMessageReceived_reset3,
            backgroundColor: "transparent",
            noHalo: true,
            keyCode: 65 // A
        });
        device.reset2 = new TouchscreenButton("reset2", {
            top: 682,
            left: 108,
            width: 60,
            height: 165
        }, {
            parent: "facit",
            callback: onMessageReceived_reset2,
            backgroundColor: "transparent",
            noHalo: true,
            keyCode: 83 // S
        });
        device.reset1 = new TouchscreenButton("reset1", {
            top: 680,
            left: 1134,
            width: 50,
            height: 190
        }, {
            parent: "facit",
            callback: onMessageReceived_reset1,
            backgroundColor: "transparent",
            noHalo: true,
            keyCode: 68 // D
        });
        device.shift_LED0 = new LED("shift_LED0", {
            top: 241,
            left: 849,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 0",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED1 = new LED("shift_LED1", {
            top: 241,
            left: 829,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 1",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED2 = new LED("shift_LED2", {
            top: 241,
            left: 808,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 2",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED3 = new LED("shift_LED3", {
            top: 241,
            left: 787,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 3",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED4 = new LED("shift_LED4", {
            top: 241,
            left: 766,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 4",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED5 = new LED("shift_LED5", {
            top: 241,
            left: 745,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 5",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED6 = new LED("shift_LED6", {
            top: 241,
            left: 725,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 6",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED7 = new LED("shift_LED7", {
            top: 241,
            left: 705,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 7",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.shift_LED8 = new LED("shift_LED8", {
            top: 241,
            left: 684,
            width: 16,
            height: 16
        }, {
            parent: "facit",
            visibleWhen: "shift = 8",
            callback: onMessageReceived,
            color: "white" // does this button light up?
        });
        device.direction = new BasicDisplay("direction", {
            top: 266,
            left: 666,
            width: 10,
            height: 16
        }, {
            parent: "facit",
            backgroundColor: "red"
        });
        device.bell = d3.select("#bell");




        /**
         function to handle when an output has been received from the server after sending a guiAction
         if the first parameter is truthy, then an error occured in the process of evaluating the gui action sent
         */
        function onMessageReceived(err, event) {
            function zeropad(x, n) {
                function zero(k) {
                    var ans = "";
                    while (k-- > 0) {
                        ans += "0";
                    }
                    return ans;
                }
                return zero(n - x.length) + x;

            }
            if (!err) {
                client.getWebSocket().lastState(event.data);
                // rendering
                var res = event.data.toString();
                device.key0.render();
                device.key1.render();
                device.key2.render();
                device.key3.render();
                device.key4.render();
                device.key5.render();
                device.key6.render();
                device.key7.render();
                device.key8.render();
                device.key9.render();
                device.shift_left.render();
                device.shift_right.render();
                // device.shift_max.render();
                device.mul.render();
                device.sub.render();
                device.reset3.render();
                device.reset2.render();
                device.reset1.render();
                if (res.indexOf("(#") === 0) {
                    res = stateParser.parse(event.data.toString());
                    if (res) {
                        device.disp1.render(res.disp1.d.replace(/"/g, ""));
                        device.disp2.render(zeropad(res.disp2, 9));
                        device.disp3.render(zeropad(res.disp3, 16));
                        if (parseInt(res.direction) === -1) {
                            device.direction.render();
                        } else {
                            device.direction.hide();
                        }
                        if (res.bell === "TRUE") {
                            device.bell.classed("animated shake", true);
                            window.setTimeout(function() {
                                device.bell.classed("animated shake", false);
                            }, 400);
                        }
                        //...
                        device.shift_LED0.render(res);
                        device.shift_LED1.render(res);
                        device.shift_LED2.render(res);
                        device.shift_LED3.render(res);
                        device.shift_LED4.render(res);
                        device.shift_LED5.render(res);
                        device.shift_LED6.render(res);
                        device.shift_LED7.render(res);
                        device.shift_LED8.render(res);
                    }
                }
            } else {
                console.log(err);
            }
        }

        d3.select("#rotate_cw").on("click", function () { device.mul.click(); });
        d3.select("#rotate_ccw").on("click", function () { device.sub.click(); });
        d3.select("#push_fw").on("click", function () {
            device.reset1.click();
            animate_reset1();
        });
        d3.select("#push_dn2").on("click", function () {
            device.reset2.click();
            animate_reset2();
        });
        d3.select("#push_dn3").on("click", function () {
            device.reset3.click();
            animate_reset3();
        });

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "facit/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init;", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
