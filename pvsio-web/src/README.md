PVSio-web [![Build Status](https://travis-ci.org/thehogfather/pvsio-web.svg?branch=alpha)](https://travis-ci.org/thehogfather/pvsio-web)
=========

Running for development
-----------------------
In order to automatically run the required compilation steps for the frontend (primarily compiling the CSS styling), the `dev` Grunt task should be used (after installing the required libraries with `npm install`):

    grunt dev

This will also automatically run the standard `start.sh` script, described above.


Directory structure
-------------------
This project has the following setup:

* start.sh - the script used to initiate the server.
* examples/ - this directory contains projects and demos
* src/ - this directory contains the pvsio-web source code
    * client/ - this directory contains the source code for the pvsio-web client. This code is executed in the user's browser
    * server/ - this directory contains the source code for the pvsio-web server. This code is executed in the node.JS environment, and manages communication between pvs/pvsio and the client code.



Testing the installation
------------------------
To test the client, start the pvsio-web backend by running the following command in a Terminal window (and leave the Terminal window open):
    ./start.sh

and type the following address in a browser window:

    http://localhost:8082/tests

To test the server, run the following command in a Terminal window

    npm test


