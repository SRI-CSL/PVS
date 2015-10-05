#!/bin/bash
npm install
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
if [ -d "pvs6.0" ] && [ -f "pvs6.0/pvs" ] && [ -f "pvs6.0/pvsio" ] && [ -f "pvs6.0/proveit" ]; then
	if [ -d "pvs6.0/nasalib" ]; then
		NASALIB_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )/pvs6.0/nasalib"
		echo "NASA LIB found at $NASALIB_DIR"
		export PVS_LIBRARY_PATH=$NASALIB_DIR
		cd pvs6.0/nasalib
		./install-scripts
		cd ../..
	else
		cd pvs6.0
		bin/relocate
		cd ..
	fi
	cd src/server
	node pvssocketserver.js pvsdir:pvs6.0 start
else
	cd src/server
	node pvssocketserver.js start
fi
