/**
 * @description: SimpleServer tutorial: demonstrates a simple ExpressJS server, see https://github.com/websockets/ws#server-example for additional information
 * @author: Paolo Masci
 * @date Jan 10, 2018
 */
/* jslint esnext:true */
/* global __dirname */

const express = require('express');
const http = require('http');
const url = require('url');
const WS = require('ws');

const tutorial_folder = __dirname + "/..";
const client_folder = __dirname + "/../../../src/client";

const app = express();
// this give access to the tutorial files
app.use("/tutorials", express.static(tutorial_folder));
// this gives access to the pvsio-web APIs
app.use("/client", express.static(client_folder));

const server = http.createServer(app);
const wss = new WS.Server({ server });

wss.on('connection', function connection(ws, req) {
    const location = url.parse(req.url, true);
    ws.on('open', function () {
        console.log(location);
    });
    ws.on('message', function incoming(message) {
        console.log('received: %s', message);
        setTimeout(function timeout() {
            console.log('sending echo: %s', message);
            ws.send(message);
        }, 500);
    });
});

server.listen(8082, function listening() {
    // console.log(__dirname);
    console.log('Listening on %d', server.address().port);
});
