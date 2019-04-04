/**
 *
 * @author Paolo Masci
 * @date Dec 12, 2017
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
        "widgets/SliderWidget",
        "widgets/ToggleButton",

        "util/playback/Player",
        "widgets/ButtonActionsQueue",
        "stateParser",
        "PVSioWebClient"
    ], function (
        ButtonEVO,
        BasicDisplay,
        Slider,
        ToggleButton,

        Player,
        ButtonActionsQueue,
        stateParser,
        PVSioWebClient
    ) {
        "use strict";
        var client = PVSioWebClient.getInstance();

        var tick;
        function start_tick() {
            if (!tick) {
                tick = setInterval(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
                }, 1600);
            }
        }
        function stop_tick() {
            if (tick) {
                clearInterval(tick);
                tick = null;
            }
        }
        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            var ans = (v < 100) ? v.toFixed(1).toString() : v.toFixed(0).toString();
            return parseFloat(ans);
        }

        // Function automatically invoked by PVSio-web when the back-end sends states updates
        function onMessageReceived(err, event) {
            if (!err) {
                // get new state
                client.getWebSocket().lastState(event.data);
                // parse and render new state
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    render(stateParser.parse(res));
                    console.log(res.replace(/\s\s+/g, ' '));
                }
            } else {
                console.log(err);
            }
        }

        var device = {};

        device.battery_level = new BasicDisplay("battery_level", {
            width: 30,
            height: 16,
            top: 28,
            left: 75
        }, {
            fontColor: "white",
            backgroundColor: "transparent",
            // opacity: "0.2",
            fontsize: 11,
            parent: "battery"
        });
        device.reservoir = new Slider("reservoir", {
            top: 48,
            left: 530,
            width: 44,
            height: 74
        }, {
            max: 500,
            min: 0,
            init: 0,
            style: "level-indicator",
            // orientation: "horizontal",
            // handle: {
            //     type: "hidden"
            // },
            // zero_padding: true,
            // backgroundColor: "transparent",
            // borderColor: "transparent",
            tooltip: {
                arrowColor: "white",
                position: "left",
                fontSize: 11
            },
            // track: {
            //     color: "#3ac441"
            // },
            // readonly: true,
            labelFormat: function (value) {
                if (value === 1) {
                    return value + " unit";
                }
                return value + " units";
            },
            parent: "topline_display"
        });

        device.on_off = new ButtonEVO("on_off", {
            width: 60,
            height: 60,
            top: 176,
            left: 576
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            parent: "device",
            borderRadius: "8px",
            callback: onMessageReceived
        });

        device.next_screen = new ButtonEVO("next_screen", {
            width: 70,
            height: 50,
            top: 210,
            left: 402
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderRadius: "4px",
            fontsize: 34,
            parent: "ready_screen",
            callback: onMessageReceived
        });
        device.previous_screen = new ButtonEVO("previous_screen", {
            width: 70,
            height: 50,
            top: 210,
            left: 0
        }, {
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderRadius: "4px",
            fontsize: 34,
            parent: "ready_screen",
            callback: onMessageReceived
        });

        // basal profile screen
        device.edit_basal_profiles = new ButtonEVO("edit_basal_profiles", {
            width: 472,
            height: 50,
            top: 0,
            left: 0
        }, {
            softLabel: "Edit Basal Profiles",
            backgroundColor: "indigo",
            opacity: "0.9",
            borderRadius: "8px",
            fontsize: 20,
            parent: "basal_management_screen",
            callback: onMessageReceived
        });
        device.activate_basal_profiles = new ButtonEVO("activate_basal_profiles", {
            width: 472,
            height: 50,
            top: 70,
            left: 0
        }, {
            softLabel: "Activate Basal Profiles",
            backgroundColor: "indigo",
            opacity: "0.85",
            borderRadius: "8px",
            fontsize: 20,
            parent: "basal_management_screen",
            callback: onMessageReceived
        });
        device.manage_temporary_basal = new ButtonEVO("manage_temporary_basal", {
            width: 472,
            height: 50,
            top: 140,
            left: 0
        }, {
            softLabel: "Manage Temporary Basal",
            backgroundColor: "indigo",
            opacity: "0.8",
            borderRadius: "8px",
            fontsize: 20,
            parent: "basal_management_screen",
            callback: onMessageReceived
        });

        // bolus profile screen
        device.edit_food_database = new ButtonEVO("edit_food_database", {
            width: 472,
            height: 50,
            top: 0,
            left: 0
        }, {
            softLabel: "Edit Food Database",
            backgroundColor: "seagreen",
            opacity: "0.9",
            borderRadius: "8px",
            fontsize: 20,
            parent: "bolus_management_screen",
            callback: onMessageReceived
        });
        device.start_bolus = new ButtonEVO("start_bolus", {
            width: 472,
            height: 120,
            top: 70,
            left: 0
        }, {
            softLabel: "Start Bolus",
            backgroundColor: "seagreen",
            opacity: "0.9",
            borderRadius: "8px",
            fontsize: 20,
            parent: "bolus_management_screen",
            callback: onMessageReceived
        });

        // pump configuration screen
        device.set_time = new ButtonEVO("set_time", {
            width: 472,
            height: 50,
            top: 0,
            left: 0
        }, {
            softLabel: "Set Time",
            backgroundColor: "slategray",
            opacity: "0.9",
            borderRadius: "8px",
            fontsize: 20,
            parent: "pump_configuration_screen",
            callback: onMessageReceived
        });
        device.configure_pump_settings = new ButtonEVO("configure_pump_settings", {
            width: 472,
            height: 120,
            top: 70,
            left: 0
        }, {
            softLabel: "Configure Pump Settings",
            backgroundColor: "slategray",
            opacity: "0.85",
            borderRadius: "8px",
            fontsize: 20,
            parent: "pump_configuration_screen",
            callback: onMessageReceived
        });

        // event data maangement screen
        device.review_bg_readings = new ButtonEVO("review_bg_readings", {
            width: 472,
            height: 50,
            top: 0,
            left: 0
        }, {
            softLabel: "Review BG Readings",
            backgroundColor: "gainsboro",
            fontColor: "black",
            opacity: "0.8",
            borderRadius: "8px",
            fontsize: 20,
            parent: "event_data_management_screen",
            callback: onMessageReceived
        });
        device.review_alarm_log = new ButtonEVO("review_alarm_log", {
            width: 472,
            height: 50,
            top: 70,
            left: 0
        }, {
            softLabel: "Alarm Log",
            backgroundColor: "gainsboro",
            fontColor: "black",
            opacity: "0.75",
            borderRadius: "8px",
            fontsize: 20,
            parent: "event_data_management_screen",
            callback: onMessageReceived
        });
        device.review_infusion_statistics = new ButtonEVO("review_infusion_statistics", {
            width: 472,
            height: 50,
            top: 140,
            left: 0
        }, {
            softLabel: "Infusion Statistics",
            backgroundColor: "gainsboro",
            fontColor: "black",
            opacity: "0.7",
            borderRadius: "8px",
            fontsize: 20,
            parent: "event_data_management_screen",
            callback: onMessageReceived
        });

        function hide_all_screens(res) {
            d3.select("#power_on_screen").style("display", "none");
            d3.select("#post_screen").style("display", "none");
            d3.select("#prime_screen").style("display", "none");
            d3.select("#ready_screen").style("display", "none");
            d3.select("#basal_subscreens").style("display", "none");
        }

        var screens = { NORMAL_OPERATION_SCREEN: 0,
                        BASAL_MANAGEMENT_SCREEN: 1,
                        BOLUS_MANAGEMENT_SCREEN: 2,
                        PUMP_CONFIGURATION_SCREEN: 3,
                        EVENT_DATA_MANAGEMENT_SCREEN: 4 };

        function NORMAL_OPERATION_MODE (res) {
            return res.mode === "NORMAL_OPERATION" || res.mode === "BASAL_MANAGEMENT"
                    || res.mode === "BOLUS_MANAGEMENT" || res.mode === "PUMP_CONFIGURATION"
                    || res.mode === "EVENT_DATA_MANAGEMENT";
        }
        function BASAL_PROFILE_SUBMODE (res) {
            return res.mode === "EDIT_BASAL_PROFILES";
        }

        function viz(id, opt) {
            opt = opt || {};
            if (opt.fade && d3.select(id).style("display") !== "block") {
                d3.select(id).style("opacity", 0).transition().duration(300).style("opacity", 1).style("display", "block");
            } else {
                d3.select(id).style("display", "block");
            }
        }
        function hide(id) {
            d3.select(id).style("display", "none");
        }

        // todo: battery widget
        function render_battery (res) {
            device.battery_level.render(res.battery_level + "%");
            d3.selectAll(".battery-icon").style("display", "none");
            if (res.battery_level > 90) {
                d3.select("#battery-full").style("display", "block");
            } else if ( res.battery_level > 70 && res.battery_level <= 90) {
                d3.select("#battery-three-quarters").style("display", "block");
            } else if ( res.battery_level > 40 && res.battery_level <= 70) {
                d3.select("#battery-half").style("display", "block");
            } else if ( res.battery_level > 10 && res.battery_level <= 40) {
                d3.select("#battery-quarter").style("display", "block");
            } else {
                d3.select("#battery-empty").style("display", "block");
            }
            if (res.battery_level < 10 && !d3.select("#battery_level").classed("blink")) {
                d3.select("#battery").classed("blink", true);
            }
        }

        device.basal_profiles = {};
        function render_basal_profiles (db) {
            for (var key in device.basal_profiles) {
                device.basal_profiles[key].remove();
            }
            var index = 0;
            for (var key in db) {
                var profile_name = db[key].name.replace(/"/g, '');
                device.basal_profiles[key] = new ButtonEVO("basal_profile_" + key, {
                    width: 468,
                    height: 50,
                    top: (50 * index),
                    left: 0
                }, {
                    softLabel: profile_name,
                    backgroundColor: "gainsboro",
                    fontColor: "black",
                    opacity: "0.9",
                    borderRadius: "0px",
                    borderColor: "black",
                    borderWidth: 1,
                    fontsize: 16,
                    parent: "basal_profiles_list",
                    callback: onMessageReceived
                });
                device.basal_profiles[key].render();
                index++;
            }
            device.basal_profiles["new_basal_profile"] = new ButtonEVO("new_basal_profile", {
                width: 468,
                height: 50,
                top: (50 * index),
                left: 0
            }, {
                softLabel: "Add New Profile",
                backgroundColor: "indigo",
                opacity: "0.9",
                borderRadius: "0 2px 2px 0",
                borderColor: "black",
                borderWidth: 1,
                fontsize: 16,
                parent: "basal_profiles_list",
                callback: onMessageReceived
            });
            device.basal_profiles["new_basal_profile"].render();
        }

        device.basal_profiles_done = new ButtonEVO("basal_profiles_done", {
            width: 64,
            height: 34,
            top: 219,
            left: 204
        }, {
            customFunctionText: "click_back",
            softLabel: "",
            backgroundColor: "steelblue",
            opacity: "0.2",
            borderRadius: "16px",
            borderColor: "black",
            borderWidth: 1,
            fontsize: 16,
            parent: "basal_pager",
            callback: onMessageReceived
        });
        device.basal_profiles_done.render();


        function render(res) {
            if (res.mode !== "POWERED_OFF") {
                viz("#topline_display");
            }
            hide_all_screens(res);
            if (res.mode === "POWER_ON") {
                viz("#power_on_screen", { fade: true });
            } else if (res.mode === "POST") {
                viz("#post_screen", { fade: true });
            } else  if (res.mode === "PRIME") {
                viz("#prime_screen", { fade: true });
            } else if (NORMAL_OPERATION_MODE(res)) {
                viz("#ready_screen");
                if (res.mode === "NORMAL_OPERATION") {
                    $('.carousel').carousel(screens.NORMAL_OPERATION_SCREEN);
                } else if (res.mode === "BASAL_MANAGEMENT") {
                    $('.carousel').carousel(screens.BASAL_MANAGEMENT_SCREEN);
                } else if (res.mode === "BOLUS_MANAGEMENT") {
                    $('.carousel').carousel(screens.BOLUS_MANAGEMENT_SCREEN);
                } else if (res.mode === "PUMP_CONFIGURATION") {
                    $('.carousel').carousel(screens.PUMP_CONFIGURATION_SCREEN);
                } else if (res.mode === "EVENT_DATA_MANAGEMENT") {
                    $('.carousel').carousel(screens.EVENT_DATA_MANAGEMENT_SCREEN);
                }
            } else if (BASAL_PROFILE_SUBMODE(res)) {
                viz("#basal_subscreens");
                if (res.mode === "EDIT_BASAL_PROFILES") {
                    viz("#edit_basal_profiles_screen");
                    render_basal_profiles(res.bps.db);
                }
            }

            // the following elements are always visible / enabled in normal operation mode
            render_battery(res);
            device.on_off.render();
            device.reservoir.render(res.volume);
            // tick
            if (res.mode === "POWER_ON" || res.mode === "POST" || res.mode === "PRIME") {
                start_tick();
            } else {
                stop_tick();
            }
        }

        // TODO: carousel widget
        function init_carousel () {
            // BASAL_MANAGEMENT
            device.edit_basal_profiles.render();
            device.activate_basal_profiles.render();
            device.manage_temporary_basal.render();
            // BOLUS_MANAGEMENT
            device.edit_food_database.render();
            device.start_bolus.render();
            // PUMP_CONFIGURATION
            device.set_time.render();
            device.configure_pump_settings.render();
            // EVENT_DATA_MANAGEMENT
            device.review_bg_readings.render();
            device.review_alarm_log.render();
            device.review_infusion_statistics.render();
            // navigator
            device.next_screen.render();
            device.previous_screen.render();
            // set carousel options
            $('.carousel').carousel({
                wrap: false
            }).carousel('pause');
        }
        // this creates the buttons on the carousel
        init_carousel();

        // TODO: digital clock widget
        var hour = d3.select("#div_hour");
        var am_pm = d3.select("#div_am_pm");
        var date = d3.select("#div_date");
        var hhmmss = d3.select("#div_hhmmss");
        function set_clock() {
            var d = new Date();
            var hh = (d.getHours() < 10) ? "0" + d.getHours() : (d.getHours() < 12) ? d.getHours() : d.getHours() - 12;
            var mm = (d.getMinutes() < 10) ? "0" + d.getMinutes() : d.getMinutes();
            var ss = (d.getSeconds() < 10) ? "0" + d.getSeconds() : d.getSeconds();
            var xx = (d.getHours() <= 12) ? "AM" : "PM";
            hour.text(hh + ":" + mm);
            date.text(d.toLocaleDateString("en-US", { weekday: 'short', year: 'numeric', month: 'long', day: 'numeric', timeZoneName: 'short'}));
            am_pm.text(xx);
            hhmmss.text(hh + ":" + mm + ":" + ss);
        }
        set_clock();
        var clock = setInterval(set_clock, 1000);

        var demoFolder = "GIIP";

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket()
                .startPVSProcess({name: "GIIP.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
                client.getWebSocket().sendGuiAction("init;", onMessageReceived);
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                // start_tick();
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();
    });
