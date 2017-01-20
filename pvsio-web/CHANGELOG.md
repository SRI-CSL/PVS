#Changelog
##2.1.1 (Dec 2016)
    Added new feature: PVS verification templates for Emucharts
    Added new feature: UPPAAL file importer for Emucharts
    Improved overall PVSio-web user interface

##2.1 (Sep/Oct 2016)
    Added new PVSio-web tool: Storyboard editor/simulator
    Added new widgets: Touchscreen elements
    Added new example project: Flight Control Unit
    Improved Prototype Builder (live widgets preview, direct support for different types of buttons, displays, LEDs)
    Refactored widgets library

##2.0.2 (April 2016)
    PVSio-web goes live on UMinho servers (http://pvsioweb.lsd.di.uminho.pt)
    Added new feature: physical keyboard keys can be linked to prototype keys
    Added new feature: voice readback for button widgets
    Added new feature: wallclock for automatic execution of tick transitions defined in the PVS model
    Improved Emucharts Editor

##2.0.1b (Sep 2015)
    Added new modelling extension: Presentation Interaction Models (PIMs)
    Added new feature: test case generation for PIM / Emucharts
    Added PVSio-web network controller for distributed simulations

##2.0.1 (April 2015)
    Added new Model Generators: VMD-SL (Vienna Development Method)
    Added new prototypes (full Alaris GP, Pacemaker, GPCA-Simulink)
    Added new display widgets (multi-line displays, LEDs)
    Added new file browser

##2.0.0 (Jan 2015)
    PVSio-web goes live on the Heroku cloud (http://www.pvsioweb.org)

##1.1.0 (Dec 2014)
    Added new Model Generators: MAL (Modal Action Logic), and PIM (Presentation Interaction Models)
    Improved Prototype Builder (widgets selection list, image scale)
    Improved server backend (file system watcher, autosave current project)

##1.0.0 (July 2014)
	Added changelog file
    Added grunt file configurations for building project
    Added new protytpes (Baxter Sigma, Smiths Medical MedFusion, Alaris PC)
    Added new tool: Emucharts Editor (replaces Emulink, new architecture/implementation, supports multiple model/code generators)
    Added new tool: Model Editor (replaces Text Editor, new implementation based on CodeMirror)
    Improved tool architecture and code quality (JSHint static analyser, Jasmine unit testing)
    PVSio-web goes live on the Amazon cloud

##0.0.18 (Mar 2014)
    Added support for logging/playback of user interactions
    Added voice feedback function for display widgets
    Added new protytpes (Arcomedical Syramed)

##0.0.17 (Dec 2013)
    Added new tool: Emulink, for creating pvs models using Statechart-like graphical diagrams
    PVSio-web license changes from MIT to GPL3

##0.0.16 (Nov 2013)
    Added new prototypes (BBraun Perfusor, Baxter Colleague, Zymed Syringe, TCAS)
    Improved Prototype Builder (cursor displays)

##0.0.15 (Sep 2013)
    Added new tool: PVSio-web browser for navigating the file system
    Added logging mechanisms
    Improved Text Editor (autocomplete, syntax highlighting)
    Improved prototypes (full GPCA demo based on the FDA Simulink Model)

##0.0.13 (Jun 2013)
    Improved architecture (introduced callback mechanisms for sending/receiving pvsio commands)

##0.0.10 (Apr 2013)
    Added new tool: Text Editor for editing PVS models
    Introduced templating mechanism based on underscore.js

##0.0.6 (Feb 2013)
    Added example prototypes (Alaris GP, Alaris Asena, GPCA)
    Improved Prototype Builder (save/load projects, widgets creation wizard)
    Improved server backend to support multiple clients

##0.0.5 (Dec 2012)
    First public release of PVSio-web, featuring a Prototype Builder for developing interactive prototypes based on formal models
