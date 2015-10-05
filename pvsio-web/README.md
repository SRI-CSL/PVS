PVSio-web [![Build Status](https://travis-ci.org/thehogfather/pvsio-web.svg?branch=alpha)](https://travis-ci.org/thehogfather/pvsio-web)
=========

PVSio-web is a new graphical tool to prototype and analyse user interface software. It provides the typical functionalities of an animation environment that allows designers to load and simulate realistic user interfaces. Underneath, it uses SRI's state-of-the-art theorem prover PVS for analysis, and the pvsio component as a basis for simulation.

Simulations created with PVSio-web can be watched in this youtube video https://www.youtube.com/watch?v=T0QmUe0bwL8

Live version
------------

Realistic prototypes created using PVSio-web can be found at the following links:
* http://www.pvsioweb.org/demos/AlarisGP (Commercial infusion pump prototype - full model)
* http://www.pvsioweb.org/demos/BBraun   (Commercial infusion pump prototype - data entry system only)
* http://www.pvsioweb.org/demos/GPCA-UI_PVS/NavKeys/ (FDA's Generic Infusion Pump prototype - full model)

The full PVSio-web tool with limited features is also available at http://www.pvsioweb.org
(please note that the web server has limited processing power so its response time might not be optimal; also, note that the file system is read-only, so you will not be able to save new prototypes or compile new models using this live version).

![Screenshot](screenshot.png?raw=true)


Running pvsio-web
-----------------
To run pvsio-web, a backend and a frontend need to be started.

To start the backend: open a Terminal window in the PVS directory, and use the following command (and leave the Terminal window open):

    ./pvsio-web

To start the frontend: open a browser (Firefox 21 or greater, or Chrome 45 or greater), and type the following address in the address bar:

    http://localhost:8082/


Examples
--------
Realistic simulations created with PVSio-web can be watched in this youtube video:
* https://www.youtube.com/watch?v=T0QmUe0bwL8

All simulation examples demonstrated in the youtube video are included in the PVSio-web distribution in examples/projects. To open these examples, start pvsio-web and click the "Open Projects" button of the pvsio-web frontend and select one of the examples from the list.


Directory structure
-------------------
This project has the following setup:

* examples/ - this directory contains projects and demos
* src/ - this directory contains the pvsio-web source code
    * client/ - this directory contains the source code for the pvsio-web client. This code is executed in the user's browser
    * server/ - this directory contains the source code for the pvsio-web server. This code is executed in the node.JS environment, and manages communication between pvs/pvsio and the client code.

