/**
 * Interactive prototype builder for PVSio based on the html map attribute
 * @author Patrick Oladimeji
 * @date Dec 3, 2012 : 4:42:55 PM
 */
/*jslint vars: true, plusplus: true, devel: true, nomen: true, indent: 4, maxerr: 50 */
/*global require*/
require.config({
    baseUrl: 'app',
    paths: {
        "d3": "../lib/d3",
        "pvsioweb": "plugins/prototypebuilder",
        "imagemapper": "../lib/imagemapper",
        "text": "../lib/text",
        "lib": "../lib",
        "cm": "../lib/cm"
    }
});

require(["main"], function (main) {
    "use strict";
    main.start();
});
