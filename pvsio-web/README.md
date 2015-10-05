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


Installation
------------
To install pvsio-web, first you need to install PVS and NodeJS, and then clone the PVSio-web github repository. This can be done as follows.

#### Step 1: Install PVS and add PVS executables to your PATH
PVS is required to run PVSio-web. The tool can be downloaded from http://pvs.csl.sri.com/download.shtml Installation instructions are on the aforementioned website.

Once PVS is installed, please add the following PVS executable files to your PATH: pvs, pvsio and proveit. A simple way to do this is to create symbolic links to those files, and place the symbolic links in /usr/bin. For instance, if PVS is installed in /opt/pvs6.0/pvs, the following commands executed in a Terminal window create the required symbolic links:

    sudo ln -s /opt/pvs6.0/pvs /usr/bin/pvs
    sudo ln -s /opt/pvs6.0/pvsio /usr/bin/pvsio
    sudo ln -s /opt/pvs6.0/proveit /usr/bin/proveit

Please note that the ln command requires a full path.

#### Step 2: Install NodeJS
NodeJS is required to run PVSio-web. Please download and install NodeJS from http://nodejs.org
Installation instructions are on the aforementioned website.

#### Step 3: Clone the PVSio-web repository
Create a directory where you would like to install PVSio-web on your local computer. Open a Terminal window in the created directory, and use 'git' to clone the PVSio-web repository:

    git clone https://github.com/thehogfather/pvsio-web.git
    cd pvsio-web
    npm install

PVSio-web is now installed on your local computer!

Running pvsio-web
-----------------
To run pvsio-web, a backend and a frontend need to be started.

To start the backend: open a Terminal window in the pvsio-web directory, and use the following command (and leave the Terminal window open):

    ./start.sh

To start the frontend: open a browser (Firefox 21 or greater, or Chrome), and type the following address in the address bar:

    http://localhost:8082/

Updating pvsio-web
------------------
To update pvsio-web to the latest version, open a Terminal window, and execute the following command from the pvsio-web directory:

    git pull

Examples
--------
Realistic simulations created with PVSio-web can be watched in this youtube video:
* https://www.youtube.com/watch?v=T0QmUe0bwL8

All simulation examples demonstrated in the youtube video are included in the PVSio-web distribution in examples/projects. To open these examples, start pvsio-web and click the "Open Projects" button of the pvsio-web frontend and select one of the examples from the list.


Directory structure
-------------------
This project has the following setup:

* start.sh - the script used to initiate the server.
* examples/ - this directory contains projects and demos
* src/ - this directory contains the pvsio-web source code
    * client/ - this directory contains the source code for the pvsio-web client. This code is executed in the user's browser
    * server/ - this directory contains the source code for the pvsio-web server. This code is executed in the node.JS environment, and manages communication between pvs/pvsio and the client code.


Nightly builds
--------------
To obtain the latest development versions of pvsio-web, you can clone our alpha branch on the github repository.

To clone the alpha branch, create a new directory (for example, pvsioweb-alpha), open a Terminal window, and execute the following commands from the created directory:

    git clone https://github.com/thehogfather/pvsio-web.git -b alpha
    cd pvsio-web
    npm install


Testing the installation
------------------------
To test the client, start the pvsio-web backend by running the following command in a Terminal window (and leave the Terminal window open):
    ./start.sh

and type the following address in a browser window:

    http://localhost:8082/tests

To test the server, run the following command in a Terminal window

    npm test


Uninstallation :(
--------------
To uninstall, delete the pvsio-web folder from your computer.

