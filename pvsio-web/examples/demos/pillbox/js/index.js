/**
 *
 * @author Paolo Masci, Patrick Oladimeji, Piergiuseppe Mallozzi
 * @date 27/03/15 20:30:33 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50, unused: false */
/*global*/
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
        "widgets/Button",
        "widgets/TouchscreenButton",
        "widgets/TouchscreenDisplay",
        "widgets/BasicDisplay",
        "widgets/NumericDisplay",
        "widgets/SingleDisplay", // FIXME: call this multiline display
        "widgets/DoubleDisplay",
        "widgets/TripleDisplay",
        "widgets/LED",
        "stateParser",
        "PVSioWebClient",
        "widgets/ButtonActionsQueue"],
    function (Button,
            TouchscreenButton,
            TouchscreenDisplay,
            BasicDisplay,
            NumericDisplay,
            SingleDisplay,
            DoubleDisplay,
            TripleDisplay,
            LED,
            stateParser,
            PVSioWebClient,
            ButtonActionsQueue) {
        "use strict";

        var d3 = require("d3/d3");
        var serverLogs = [], maxLogSize = 40;
        var client = PVSioWebClient.getInstance();
        //create a collapsible panel using the pvsiowebclient instance
        var imageHolder = client.createCollapsiblePanel({
            parent: "#content",
            headerText: "Simulation of IPCA's PillBox device.",
            showContent: true,
            isDemo: true
        }).style("position", "relative");
        //insert the html into the panel (note that this could have used templates or whatever)
        imageHolder.html('<img src="pillbox.png" usemap="#prototypeMap"/>').attr("id", "prototype");

        var pause_simulation = false;
        var content = imageHolder.append("div").style("position", "absolute").style("top", "0px").style("left", "1250px")
            .style("height", "40px").style("width", "800px").attr("class", "dbgbuttons");
        // content.append("button").text("Pause Simulation").attr("id", "pause_simulation").attr("style", "margin:0 20px 0 20px;");
        // content.append("span").attr("id", "simulation_status").text("Ready!");
        // content.append("button").text("Resume Simulation").attr("id", "resume_simulation").attr("style", "margin:0 20px 0 20px;");

        content = imageHolder.append("div").style("position", "absolute").style("top", "40px").style("left", "1250px")
            .style("height", "900px").style("width", "800px").attr("class", "dbg");

        function log(evt) {
            function prettyprintState(str) {
                var state = stateParser.parse(str);
                return JSON.stringify(state, null, " ");
            }

            serverLogs.push(evt);
            if (serverLogs.length > maxLogSize) {
                serverLogs = serverLogs.slice(-maxLogSize);
            }
            var logLines = d3.select(".dbg").selectAll("textarea").data(serverLogs, function (d, i) {
                return d.id;
            });
            logLines.enter()
                .insert("textarea", "textarea").html(function (d) {
                    return d.date.toString() + "\n" + d.data;
                }).style("width", "100%")
                .attr("readonly", true)
                .attr("rows", function (d) {
                    return d.data.split("\n").length + 1;
                }).attr("class", function (d) {
                    return d.type;
                });
            logLines.exit().remove();
        }

        function evaluate(str) {
            var v = +str;
            if (str.indexOf("/") >= 0) {
                var args = str.split("/");
                v = +args[0] / +args[1];
            }
            return (v < 100) ? v.toFixed(1).toString() : v.toFixed(0).toString();
        }

        var tick;
        function single_tick() {
            ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
        }
        function start_tick() {
            if (!tick && !pause_simulation) {
                tick = setInterval(function () {
                    ButtonActionsQueue.getInstance().queueGUIAction("tick", onMessageReceived);
                }, 2000);
            }
        }
        function stop_tick() {
            if (tick) {
                clearInterval(tick);
                tick = null;
            }
        }
        d3.select("#pause_simulation").on("click", function () {
            pause_simulation = true;
            stop_tick();
            d3.select("#simulation_status").text("Paused").attr("class", "blink");
        });
        d3.select("#resume_simulation").on("click", function () {
            pause_simulation = false;
            start_tick();
            d3.select("#simulation_status").text("Ready!").attr("class", "ready");
        });


        // Function automatically invoked by PVSio-web when the back-end sends states updates
        function onMessageReceived(err, event) {
            if (!err) {
                // get new state
                client.getWebSocket().lastState(event.data);
                // parse and render new state
                var res = event.data.toString();
                if (res.indexOf("(#") === 0) {
                    var data = stateParser.parse(res);
                    render(data);
                    log({ data: JSON.stringify(data, null, " "), date: new Date(), id: event.id, type: "frompvs" });
                    //console.log(res);
                }
            } else {
                console.log(err);
            }
        }

        var patients = [
            {
                name: "Kylie Minogue",
                dob: "May 28, 1968",
                id_card: "PT-123-456-789",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Bruce Willis",
                dob: "Mar 19, 1955",
                id_card: "AS-523-235-21",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Chris Bale",
                dob: "Jan 30, 1974",
                id_card: "SAR-13-12-789",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Charlize Theron",
                dob: "Aug 7, 1975",
                id_card: "LA-222-466-789",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Natalie Portman",
                dob: "Jun 9, 1981",
                id_card: "US-155-46-589",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Emma Stone",
                dob: "Nov 6, 1988",
                id_card: "GE-121-234-739",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Scarlett Johansson",
                dob: "Nov 22, 1984",
                id_card: "FA-21-045-345",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
            {
                name: "Mila Kunis",
                dob: "Aug 14, 1983",
                id_card: "GN-552-436-779",
                carer: "+33 099 566 299",
                mob: "+44 823 394 111"
            },
        ];
        var meds = [
            {
                medicine: "Lisinopril",
                dosage: "2.5mg",
                validity: "Dec 2018",
                quantity: "32",
                column: "1"
            },
            {
                medicine: "Bystolic",
                dosage: "5mg",
                validity: "Jan 2019",
                quantity: "16",
                column: "2"
            },
            {
                medicine: "Norvasc",
                dosage: "2.5mg",
                validity: "Nov 2019",
                quantity: "30",
                column: "3"
            },
            {
                medicine: "Diovan",
                dosage: "10mg",
                validity: "Jun 2019",
                quantity: "10",
                column: "4"
            },
            {
                medicine: "Azor",
                dosage: "5mg",
                validity: "Jan 2019",
                quantity: "20",
                column: "5"
            },
            {
                medicine: "Benicar",
                dosage: "5mg",
                validity: "Jun 2019",
                quantity: "10",
                column: "6"
            },
            {
                medicine: "Coreg",
                dosage: "10mg",
                validity: "Nov 2019",
                quantity: "30",
                column: "7"
            },
            {
                medicine: "Lopressor",
                dosage: "2.5mg",
                validity: "Nov 2019",
                quantity: "30",
                column: "8"
            },
        ];
        var pillbox = {}; // this variable collects references to buttons and displays
        pillbox.key1 = new TouchscreenButton("key1",
                            { top: 216, left: 230, height: 64, width: 64 },
                            {
                                softLabel: "home",
                                visibleWhen: "action.key1 = TRUE", //fixme, constants should not be case-sensitive!
                                backgroundColor: "green",
                                fontsize: 16,
                                callback: onMessageReceived,
                                parent: "prototype"
                            });
        pillbox.key2 = new TouchscreenButton("key2",
                            { top: 282, left: 230, height: 64, width: 64 },
                            {
                                softLabel: "patients",
                                visibleWhen: "action.key2 = TRUE",
                                backgroundColor: "steelblue",
                                fontsize: 16,
                                callback: onMessageReceived,
                                parent: "prototype"
                            });
        pillbox.key3 = new TouchscreenButton("key3",
                            { top: 348, left: 230, height: 64, width: 64 },
                            {
                                softLabel: "MED",
                                visibleWhen: "action.key3 = TRUE",
                                backgroundColor: "#8c3434",
                                fontsize: 16,
                                callback: onMessageReceived,
                                parent: "prototype"
                            });
        pillbox.field_password = new TouchscreenButton("field_password",
                            { top: 300, left: 500, height: 32, width: 256 },
                            {
                                softLabel: "******",
                                functionText: "",
                                visibleWhen: "field.password.visible = TRUE",
                                fontColor: "steelblue",
                                backgroundColor: "white",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.confirm = new TouchscreenButton("confirm",
                            { top: 300, left: 756, height: 32, width: 146 },
                            {
                                softLabel: "Confirm",
                                visibleWhen: "action.confirm = TRUE",
                                fontColor: "white",
                                backgroundColor: "steelblue",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.quit = new TouchscreenButton("quit",
                            { top: 380, left: 756, height: 32, width: 146 },
                            {
                                softLabel: "Quit",
                                visibleWhen: "action.quit = TRUE",
                                fontColor: "white",
                                backgroundColor: "red",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.create = new TouchscreenButton("create",
                            { top: 220, left: 756, height: 32, width: 146 },
                            {
                                softLabel: "Create",
                                visibleWhen: "action.create = TRUE",
                                fontColor: "white",
                                backgroundColor: "green",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.medicines = new TouchscreenButton("medicines",
                            { top: 75, left: 360, height: 32, width: 200 },
                            {
                                softLabel: "medicines",
                                visibleWhen: "action.medicines = TRUE",
                                fontColor: "white",
                                backgroundColor: "#8c3434",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        // FIXME: introduce z-index
        pillbox.patients_db = new SingleDisplay("patients_db",
                            { top: 200, left: 360, height: 240, width: 350 },
                            {
                                //backgroundColor: "grey",
                                parent: "prototype"
                            });
        pillbox.meds_db = new SingleDisplay("meds_db",
                            { top: 200, left: 360, height: 240, width: 350 },
                            {
                                //backgroundColor: "grey",
                                parent: "prototype"
                            });
        pillbox.fields_new_patient = new SingleDisplay("fields_new_patient",
                            { top: 226, left: 360, height: 170, width: 350 },
                            {
                                //backgroundColor: "grey",
                                parent: "prototype"
                            });
        pillbox.fields_new_med = new SingleDisplay("fields_new_med",
                            { top: 226, left: 360, height: 170, width: 350 },
                            {
                                //backgroundColor: "grey",
                                parent: "prototype"
                            });
        pillbox.scan_LED = new LED("scan_LED", { top: 848, left: 604, height: 20, width: 20 },
                            {   visibleWhen: "mode = scan",
                                color: "lime",
                                blinking: true,
                                parent: "prototype" });
        pillbox.scan_success_LED = new LED("scan_success_LED", { top: 848, left: 670, height: 20, width: 20 },
                            {   visibleWhen: "mode = scan_enabled",
                                color: "white",
                                parent: "prototype" });
        pillbox.scan_message = new TouchscreenButton("scan_message",
                            { top: 300, left: 400, height: 32, width: 456 },
                            {
                                softLabel: "Please scan your fingerprint",
                                customFunctionText: "",
                                visibleWhen: "mode = scan",
                                fontColor: "springgreen",
                                backgroundColor: "black",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.scan_ok = new TouchscreenButton("scan_ok",
                            { top: 300, left: 400, height: 32, width: 456 },
                            {
                                softLabel: "Scan Ok, please wait...",
                                customFunctionText: "",
                                visibleWhen: "mode = scan_enabled",
                                fontColor: "white",
                                backgroundColor: "black",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.creation_success = new TouchscreenButton("creation_success",
                            { top: 300, left: 400, height: 32, width: 500 },
                            {
                                softLabel: "Patient registered successfully!",
                                customFunctionText: "",
                                visibleWhen: "mode = creation_success",
                                fontColor: "white",
                                backgroundColor: "black",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });
        pillbox.fingerprint_scanner = new TouchscreenButton("fingerprint_scanner",
                            { top: 830, left: 610, height: 40, width: 70 },
                            {
                                customFunctionText: "tap_fingerprint_scanner",
                                backgroundColor: "transparent",
                                visibleWhen: "mode = scan",
                                callback: onMessageReceived,
                                parent: "prototype"
                            });
        pillbox.creation_succcess_message = new TouchscreenButton("creation_succcess_message",
                            { top: 300, left: 400, height: 32, width: 456 },
                            {
                                softLabel: "Patient profile created successfully!",
                                customFunctionText: "",
                                visibleWhen: "mode = creation_success",
                                fontColor: "white",
                                backgroundColor: "green",
                                callback: onMessageReceived,
                                fontsize: 16,
                                parent: "prototype"
                            });

        function render(res) {
            if (res.action.tick === "TRUE") {
                single_tick();
            }
            // if (res.action.tick === "TRUE") {
            //     start_tick();
            // } else {
            //     stop_tick();
            // }
            // function keys
            pillbox.key1.render(res);
            pillbox.key2.render(res);
            pillbox.key3.render(res);
            // fields
            pillbox.field_password.render(res);
            // actions
            pillbox.confirm.render(res);
            pillbox.quit.render(res);
            pillbox.create.render(res);
            // database
            function render_patients_db(res) {
                var db = [];
                var sel = [];
                var p = res.patients_db;
                for (var i = 0; i < p.length; i++) {
                    if (p[i].val.name.visible === "TRUE") {
                        db.push(i + ": " + patients[i].name);
                    } else {
                        db.push(i + ": ----- ");
                    }
                    sel.push(p[i].val.name.selected);
                    // the following is for interline spacing
                    db.push("");
                    sel.push(false);
                }
                if (res.mode === "patient_list") {
                    pillbox.patients_db.renderMultiline(db, { selected: sel, align: "left" });
                } else {
                    pillbox.patients_db.hide();
                }
            }
            render_patients_db(res);
            //---
            function render_meds_db(res) {
                var db = [];
                var sel = [];
                var m = res.meds_db;
                for (var i = 0; i < m.length; i++) {
                    if (m[i].val.medicine.visible === "TRUE") {
                        db.push(i + ": " + meds[i].medicine);
                    } else {
                        db.push(i + ": ----- ");
                    }
                    sel.push(m[i].val.medicine.selected);
                    // the following is for interline spacing
                    db.push("");
                    sel.push(false);
                }
                if (res.mode === "db_med_list") {
                    pillbox.meds_db.renderMultiline(db, { selected: sel, align: "left" });
                } else {
                    pillbox.meds_db.hide();
                }
            }
            render_meds_db(res);
            // new patient
            function render_fields_new_patient(res) {
                var record = [];
                if (res.field.name.entered === "TRUE") {
                    record.push("Name: " + patients[parseInt(res.p_max)].name);
                } else { record.push("Name: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.dob.entered === "TRUE") {
                    record.push("Date of birth: " + patients[parseInt(res.p_max)].dob);
                } else { record.push("Date of birth: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.id_card.entered === "TRUE") {
                    record.push("Identity card number: " + patients[parseInt(res.p_max)].id_card);
                } else { record.push("Identity card number: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.carer.entered === "TRUE") {
                    record.push("Caretaker phone: " + patients[parseInt(res.p_max)].carer);
                } else { record.push("Caretaker phone: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.mob.entered === "TRUE") {
                    record.push("Patient phone: " + patients[parseInt(res.p_max)].mob);
                } else { record.push("Patient phone: ____________"); }
                if (res.mode === "new_patient_details") {
                    pillbox.fields_new_patient.renderMultiline(record, { align: "left" });
                } else {
                    pillbox.fields_new_patient.hide();
                }
            }
            render_fields_new_patient(res);
            // new medicine
            function render_fields_new_medicine(res) {
                var record = [];
                if (res.field.medicine.entered === "TRUE") {
                    record.push("Medicine: " + meds[parseInt(res.m_max)].medicine);
                } else { record.push("Medicine: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.dosage.entered === "TRUE") {
                    record.push("Dosage: " + meds[parseInt(res.m_max)].dosage);
                } else { record.push("Dosage: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.validity.entered === "TRUE") {
                    record.push("Validity: " + meds[parseInt(res.m_max)].validity);
                } else { record.push("Validity: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.quantity.entered === "TRUE") {
                    record.push("Quantity: " + meds[parseInt(res.m_max)].quantity);
                } else { record.push("Quantity: ____________"); }
                record.push(""); // this is for interline spacing
                if (res.field.column.entered === "TRUE") {
                    record.push("Column: " + meds[parseInt(res.m_max)].column);
                } else { record.push("Column: ____________"); }
                if (res.mode === "new_med") {
                    pillbox.fields_new_med.renderMultiline(record, { align: "left" });
                } else {
                    pillbox.fields_new_med.hide();
                }
            }
            render_fields_new_medicine(res);
            // fingerprint scanner
            pillbox.scan_LED.render(res);
            pillbox.scan_message.render(res);
            pillbox.scan_ok.render(res);
            pillbox.creation_success.render(res);
            pillbox.scan_success_LED.render(res);
            pillbox.fingerprint_scanner.render(res);
            // medicines
            pillbox.medicines.render(res);
        }

        //register event listener for websocket connection from the client
        client.addListener('WebSocketConnectionOpened', function (e) {
            console.log("web socket connected");
            //start pvs process
            client.getWebSocket().startPVSProcess({name: "main.pvs", demoName: "pillbox/pvs"}, function (err, event) {
                d3.select(".demo-splash").style("display", "none");
                d3.select(".content").style("display", "block");
                ButtonActionsQueue.getInstance().queueGUIAction("", onMessageReceived);
            });
        }).addListener("WebSocketConnectionClosed", function (e) {
            console.log("web socket closed");
        }).addListener("processExited", function (e) {
            var msg = "Warning!!!\r\nServer process exited. See console for details.";
            console.log(msg);
        });

        client.connectToServer();

    });
