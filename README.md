PVS Specification and Verification System
=========================================

[![Build Status](https://travis-ci.org/SRI-CSL/PVS.svg?branch=master)](https://travis-ci.org/SRI-CSL/PVS)

PVS is a verification system: that is, a specification language integrated with support tools and a theorem prover. It is intended to capture the state-of-the-art in mechanized formal methods and to be sufficiently rugged that it can be used for significant applications. PVS is a research prototype: it evolves and improves as we develop or apply new capabilities, and as the stress of real use exposes new requirements.

For documentation and pre-built binaries, please visit http://pvs.csl.sri.com/.

Making PVS 8.0 from sources
---------------------------

In short:
0. For Macs, you'll need to install xcode, in Ubuntu, apt-get install build-essential
1. Use your favorite package manager to get SBCL and Emacs
2. git clone https://github.com/SRI-CSL/PVS.git
3. cd PVS
4. Run ./configure, use your package manager if, e.g., curl is missing
5. Run make

Notes:
1. This works for Mac M1 natively
2. For some reason the make on Intel Macs pauses a long time at the
   compile for list-decls; just be patient.
3. There is a Dockerfile that will create a Docker image for PVS; this can
   be used on a Windows platform.
4. If you're using the current version of NASA's pvslib, you will get some
   errors. You can temporarily use the pvs8.0 branch of 
     https://github.com/samowre/pvslib.git,
   which is a fork of it upgraded to work with PVS 8.0 and SBCL.

Let us know at pvs@csl.sri.com if you have problems or suggestions.

Source layout
-------------
Files:

* README           - this file
* pvs              - the shell script for invoking pvs
* pvsio-web        - the shell script for invoking the pvsio-web prototyping tool
* pvs.sty	   - the style file supporting LaTeX output
* pvs-tex.sub      - the default substitution file for generating LaTeX

Directories:
* Examples - some simple example specifications
* emacs    - Emacs files.
* wish     - Tcl/Tk files
* bin      - shell scripts and executables
* lib      - prelude, help files, and libraries
* pvsio-web  - PVSio-web prototyping tool and example prototypes
* javascript -  experimental javascript front-ends for PVS. See [javascript/README.md](javascript/README.md) for more info on how to run the tools.
