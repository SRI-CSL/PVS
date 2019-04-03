require.config({
    baseUrl: "../../client/app",
    paths: {
        d3: "../lib/d3",
        lib: "../lib",
        text: "../lib/text",
        stateParser: './util/PVSioStateParser'
    }
});
require(["PVSioWebClient", "stateParser",
        "widgets/BasicDisplay","widgets/NumericDisplay", "widgets/TouchscreenButton", "widgets/TouchscreenDisplay",
        "widgets/ToggleButton", "widgets/SliderWidgetWithButtons", "widgets/car/Gauge", "widgets/LED", "widgets/core/ButtonEVO"],
function (PVSioWebClient, stateParser,
            BasicDisplay, NumericDisplay, TouchscreenButton, TouchscreenDisplay,
            ToggleButton, SliderWidget, Gauge, LED, ButtonEVO) {
     "use strict";

     var device = {};

    //  device.disp = new BasicDisplay("disp", {
    //      top: 100, left: 120, height: 24, width: 120
    //  }, {
    //      displayKey: "disp",
    //      borderColor: "blue",
    //      borderWidth: 2,
    //      align: "right"
    //  });
     //
    //  device.dispNumeric = new NumericDisplay("dispNumeric", {
    //      top: 150, left: 120, height: 24, width: 120
    //  }, {
    //      displayKey: "disp",
    //      cursorName: "cur",
    //      borderColor: "blue",
    //      borderWidth: 2,
    //      align: "right"
    //  });
     //
    //  device.touchscreenOk = new TouchscreenButton("touchscreenOk", {
    //      top: 200, left: 120, height: 24, width: 120
    //  }, {
    //      softLabel: "Ok",
    //      fontColor: "white",
    //      backgroundColor: "black",
    //      borderColor: "blue",
    //      borderWidth: 2,
    //      fontsize: 16,
    //      toggleButton: true,
    //      callback: onMessageReceived
    //  });
     //
    //  device.slider = new SliderWidget("slider", {
    //      top: 250, left: 120, width: 40, height: 260
    //  }, {
    //      max: 340,
    //      min: 0,
    //      init: 50, // initial value selected by the slider
    //      orientation: "horizontal",
    //      buttonStep: 10,
    //      buttons: {
    //          backgroundColor: "#121e2b",
    //          borderColor: "#8aa1b6",
    //          fontColor: "#cad7e4",
    //          opacity: 1
    //      },
    //      handle: {
    //          backgroundColor: "#121e2b",
    //          borderColor: "#9bb5cc",
    //          fontColor: "#cad7e4",
    //          opacity: 0.9,
    //          zIndex: 2
    //      },
    //      innerImage: {
    //          type: "triangle"
    //      },
    //      callback: onMessageReceived
    //  });

    device.btnEVO = new ButtonEVO("disp", {
        top: 100, left: 120, height: 24, width: 120
    }, {
        softLabel: "Ok",
        fontColor: "white",
        backgroundColor: "steelblue",
        borderColor: "black",
        borderWidth: 0,
        borderStyle: "dashed",
        // fontsize: 16,
        overlayColor: "green",
        toggleButton: true,
        parent: "widgetPanel",
        touchscreenMode: false,
        callback: onMessageReceived
    });
    // device.btnEVO.move({ top: 30 }, { duration: 1000 });



     //--------------- the following functions take care of rendering state updates sent by the PVSio-web back-end
     function render(res) {
         for (var key in device) {
             device[key].render(res);
         }
     }
     function onMessageReceived(err, event) {
         if (!err) {
             // get new state
             client.getWebSocket().lastState(event.data);
             // parse and render new state
             var res = event.data.toString();
             if (res.indexOf("(#") === 0) {
                 render(stateParser.parse(res));
                 console.log(res);
             }
         }
     }

     //--------------- the following code takes care of connecting the JavaScript front-end to the PVSio-web back-end
     var demoFolder = "quickStart";
     var client = PVSioWebClient.getInstance();
     //register event listener for websocket connection from the client
     client.addListener('WebSocketConnectionOpened', function (e) {
         console.log("web socket connected");
         //start pvs process
         client.getWebSocket()
         .startPVSProcess({name: "main.pvs", demoName: demoFolder + "/pvs"}, function (err, event) {
             // first thing, initialise the pvs model
             client.getWebSocket().sendGuiAction("init;", onMessageReceived);
         });
     }).addListener("WebSocketConnectionClosed", function (e) {
         console.log("web socket closed");
     }).addListener("processExited", function (e) {
         var msg = "Warning!!!\r\nServer process exited. See console for details.";
         console.log(msg);
     });
     client.connectToServer();

});
