#!/bin/bash
npm install
#npm install -g node-inspector
if [ -d "src/server/lib" ] && [ -f "src/server/lib/glassfish-4-1.zip.partaa" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partab" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partac" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partad" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partae" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partaf" ] && 
	[ -f "src/server/lib/glassfish-4-1.zip.partag" ] && 
	[ ! -d "src/server/lib/glassfish4" ]; then
  cd src/server/lib/
  ./installNC.sh
  cd ../../..
fi
if [ -d "src/server/ext" ] && [ ! -d "src/server/ext/IVY" ] &&
    [ -f "src/server/ext/IVY.zip.partaa" ] && 
	[ -f "src/server/ext/IVY.zip.partab" ] && 
	[ -f "src/server/ext/IVY.zip.partac" ]; then
  cd src/server/ext/
  ./installIVY.sh
  cd ../../..
fi
if [ -d "PVS" ] && [ -f "PVS/pvs" ] && [ -f "PVS/pvsio" ] && [ -f "PVS/proveit" ]; then
    PVS_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )/PVS"
    echo "PVS installation found at $PVS_DIR"
	if [ -d "PVS/nasalib" ]; then
		NASALIB_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )/PVS/nasalib"
		echo "NASA LIB found at $NASALIB_DIR"
		export PVS_LIBRARY_PATH=$NASALIB_DIR
		cd PVS/nasalib
		./install-scripts
		cd ../..
	else
		cd PVS
		bin/relocate
		cd ..
	fi
	cd src/server
	node-debug pvssocketserver.js pvsdir:$PVS_DIR restart
else
	cd src/server
	node-debug pvssocketserver.js restart
fi