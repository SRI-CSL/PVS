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
	node pvssocketserver.js pvsdir:$PVS_DIR restart
elif [ -f "../pvs" ] && [ -f "../pvsio" ] && [ -f "../proveit" ]; then
    PVS_DIR=${PWD%/*}
    pvsioweb_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    echo "PVS installation found at $PVS_DIR"
	if [ -d "$PVS_DIR/nasalib" ]; then
		NASALIB_DIR="$PVS_DIR/nasalib"
		echo "NASA LIB found at $NASALIB_DIR"
		export PVS_LIBRARY_PATH=$NASALIB_DIR
		cd $PVS_DIR/nasalib
		./install-scripts
		cd $pvsioweb_DIR
	else
		cd $PVS_DIR
		bin/relocate
		cd $pvsioweb_DIR
	fi
	cd src/server
	node pvssocketserver.js pvsdir:$PVS_DIR restart
else
	pvsio -version
	if [ $? -eq 0 ]; then
		cd src/server
		node pvssocketserver.js restart
	else
		#FAIL
        echo "================================================================"
        echo "================================================================"
		echo "====   ERROR: Failed to locate PVS executable files         ===="
        echo "================================================================"
        echo "====   Please install PVSio-web within the PVS folder       ===="
        echo "====   or alternatively place the PVS executable files on   ===="
        echo "====   your PATH (see README.md for installation details).  ===="
        echo "================================================================"
        echo "================================================================"
	fi
fi