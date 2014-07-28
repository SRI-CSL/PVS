#!/bin/sh

set -e

URL="https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2"

cd $HOME
curl $URL -L | tar -jx
ln -s sbcl* sbcl
