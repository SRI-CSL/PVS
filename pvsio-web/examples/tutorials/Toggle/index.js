/**
 * @module Toggle
 * @version 1.0
 * @author Paolo Masci
 * @date 14/01/2018
 * @description This tutorial demonstrates how to create an interactive prototype that responds to user actions.<br>
    You will learn:
    <li> How to create and use input widgets;</li>
    <li> How to send commands to the PVSio-web back-end.</li>

    This tutorial assumes the reader has already successfully completed the HelloWorld tutorial.

    <h4 style="margin-top:34px;margin-bottom:4px;font-weight:bold;">Interactive prototypes</h4>
    Interactive prototypes that respond to user actions build on the use of input elements such as buttons, knobs, and sliders.<br>
    PVSio-web provides an <em>input widgets library</em> that can be used by developers to transform selected regions of a picture into interactive areas
    that emulate input elements of a device. More specifically, the <em>input widgets library</em> can be used for:
    <li> capturing user actions performed on selected regions of the prototype;</li>
    <li> animating selected regions of the prototype in response to user actions (e.g., to visualise a knob that rotates);</li>
    <li> translating user actions into commands that drive the evaluation of the device model simulated on the PVSio-web back-end.</li>

    <p>
    In this tutorial, we will demonstrate the use an input widget, <em>ButtonEVO</em>, from the <em>core</em> library of PVSio-web
    (see also <a href="../../../docs/widgets-library/core" target=_blank>docs/widgets-library/core</a>).<br>
    An example prototype will be developed to demonstrate the APIs of the button widget.
    It extends the prototype developed in the HelloWorld tutorial by introducing the following functionality:
    every time the user clicks the home button of the device, the display color toggles between steelblue and green.
    </p>

    The steps for creating the button widget as follows:<br>
    1. the widget library is imported programmatically in the Javascript file, using the <em>require</em> mechanism (see line 7 in the code snippet below)<br>
    2. the widget is instantiated using the constructor method, which takes three arguments (see lines 15-22 in the code snippet below);<br>
    3. the <em>render</em> method is used to enable the widget (see line 23 in the code snippet below);<br>
    4. a <em>WebSocket client</em> is instantiated, that is used by the widget for sending a "&lt;ping&gt;" command to the PVSio-web back-end (see lines 41-49 in the code snippet below);<br>
    5. a <em>callback</em> function is defined, that is by the widget for refreshing the content of the display widget on-demand, when the PVSio-web back-end responds to the "&lt;ping&gt;" command (see lines 12-14 in the code snippet below).<br>

<pre class="prettyprint linenums"><code>require.config({
    baseUrl: "../../client/app",
    paths: { d3: "../lib/d3", text: "../lib/text" }
});
require([
    "widgets/core/BasicDisplayEVO",
    "widgets/core/ButtonEVO",
    "PVSioWebClient"
], function (BasicDisplay, Button, PVSioWebClient) {
    "use strict";
    // callback function
    function onMessageReceived(err, res) {
        toggle_disp_color();
    }
    // input widget
    var toggle_btn = new Button("toggle_btn", {
        top: 493, left: 168, height: 54, width: 54
    }, {
        borderRadius: 8,
        customFunctionText: "<ping>",
        callback: onMessageReceived
    });
    toggle_btn.render();

    // display widget
    var disp = new BasicDisplay("disp", {
        top: 62, left: 44, height: 410, width: 294
    }, {
        fontSize: 12, backgroundColor: "steelblue",
    });
    disp.render("...ready to start...");

    // auxiliary variable and function used by onMessageReceived for toggling screen color
    var color = "steelblue";
    function toggle_disp_color() {
        color = (color === "steelblue") ? "olive" : "steelblue";
        disp.render(color, { backgroundColor: color });
    }

    // web socket client
    var client = PVSioWebClient.getInstance();
    client.addListener("WebSocketConnectionOpened", function (res) {
        console.log("Connection opened!");
    }).addListener("WebSocketConnectionClosed", function (evt) {
        console.log("Connection closed :((");
    });
    client.connectToServer();
});
</code></pre><br>

    <h4 style="margin-top:34px;margin-bottom:4px;font-weight:bold;">Additional notes on the PVSioWebClient library</h4>
    The <em>PVSioWebClient</em> library integrates input widgets and standard WebSocket clients.

    <p>
    The main APIs of the library are as follows:<br>
    - <em>getInstance</em>, a method for obtaining an instance of the PVSio-web client;<br>
    - <em>serverUrl</em>, a method for setting the URL of the PVSio-web back-end (default is ws://localhost);<br>
    - <em>port</em>, a method for setting the port on the PVSio-web back-end for establishing the connection (default is 8082);<br>
    - <em>connectToServer</em>, a method for opening a WebSocket connection with the PVSio-web back-end.
    </p>

    <p>
    Changes to the state of the WebSocket connection are notified through the following events:<br>
    - <em>WebSocketConnectionOpened</em>: this event indicates that the client has successfully opened a WebSocket connection with the PVSio-web back-end;<br>
    - <em>WebSocketConnectionClosed</em>: this event indicates that the WebSocket connection has been closed by the PVSio-web back-end.
    </p>

    The standard pattern for using the <em>PVSioWebClient</em> library is therefore as follows:<br>
    <pre class="prettyprint linenums"><code>require([ "PVSioWebClient", ... ], function (PVSioWebClient, ...) {
        ...
        var client = PVSioWebClient.getInstance();
        client.addListener("WebSocketConnectionOpened", function (res) {
           console.log("Connection opened!");
        }).addListener("WebSocketConnectionClosed", function (evt) {
           console.log("Connection closed :((");
        });
        client.connectToServer();
    });
    </code></pre>

    <h4 style="margin-top:34px;margin-bottom:4px;font-weight:bold;">Loading and executing the prototype</h4>
    <p>
    The HTML code for loading the PVSio-web APIs and the Javascript file of the prototype in the same used for the HellowWorld tutorial.
    To execute the prototype, start the PVSio-web back-end, and then open a web browser at the following page: <a href="http://localhost:8082/tutorials/Toggle" target=_blank>http://localhost:8082/tutorials/Toggle</a><br>
    The following screenshots illustrate the output sequence rendered in the browser when the home button of the device is clicked:<br>
    <img src="ready-to-start.png" style="width:200px;"> &gt;click &gt; <img src="olive.png" style="width:200px;"> &gt;click &gt; <img src="steelblue.png" style="width:200px;"></div>
 */
/* jslint esnext:true */
require.config({
    baseUrl: "../../client/app",
    paths: { d3: "../lib/d3", text: "../lib/text" }
});

require([
    "widgets/core/BasicDisplayEVO",
    "widgets/core/ButtonEVO",
    "PVSioWebClient"
], function (BasicDisplay, Button, PVSioWebClient) {
    "use strict";
    // callback function
    function onMessageReceived(err, res) {
        toggle_disp_color();
    }
    // input widget
    var toggle_btn = new Button("toggle_btn", {
        top: 493, left: 168, height: 54, width: 54
    }, {
        borderRadius: 8,
        customFunctionText: "<ping>",
        callback: onMessageReceived
    });
    toggle_btn.render();

    // display widget
    var disp = new BasicDisplay("disp", {
        top: 62, left: 44, height: 410, width: 294
    }, {
        fontSize: 12, backgroundColor: "steelblue",
    });
    disp.render("...ready to start...");

    // auxiliary variable and function used by onMessageReceived for toggling screen color
    var color = "steelblue";
    function toggle_disp_color() {
        color = (color === "steelblue") ? "olive" : "steelblue";
        disp.render(color, { backgroundColor: color });
    }

    // web socket client
    var client = PVSioWebClient.getInstance();
    client.addListener("WebSocketConnectionOpened", function (res) {
        console.log("Connection opened!");
    }).addListener("WebSocketConnectionClosed", function (evt) {
        console.log("Connection closed :((");
    });
    client.connectToServer();
});
