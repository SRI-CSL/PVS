#Javascript tools
This directory contains tools for running and interacting with PVS within your browser. At the moment there are two tools: ProofExplorer and PVSio-web.

###Software Requirements
* PVS version 6+ (with server functionality)
* nodejs [download here](http://nodejs.org/download/)

Before running any of these tools you will need to install PVS and [nodejs](http://nodejs.org/download/). NodeJS is used to manage communication between the PVS process and the Javascript running in your browser.

##Getting and updating the Javascript tools
These tools are included in this repository as submodules. Consequently, the first time you clone this repository, the directories `ProofExplorer` and `pvsioweb` will be empty. To initialise the directories you need to run `git submodule init` and to fetch all the data for the folders run `git submodule update`.

##ProofExplorer
The ProofExplorer is a graphical frontend for visualising and manipulating proof trees in the PVS system.

###How to setup and run
Ensure the ProofExplorer submodule has been initialised and updated.
See `javascript/ProofExplorer/README.md` for further setup instructions.

###Issues
Kindly report any issues to [https://github.com/thehogfather/ProofExplorer](https://github.com/thehogfather/ProofExplorer).

##PVSio-web
PVSio-web is a new graphical tool to prototype and analyse user interface software. It provides the typical functionalities of an animation environment that allows designers to load and simulate realistic user interfaces. Underneath, it uses state-of-the-art theorem prover PVS for analysis, and the pvsio component as a basis for simulation.

##How to setup and run
Ensure the PVSio-web submodule has been initialised and updated.
See `javascript/pvsio-web/README.md` for further setup instructions.

#Issues
Kindly report any issues to [https://github.com/thehogfather/pvsio-web](https://github.com/thehogfather/pvsio-web).

