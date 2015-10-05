#!/bin/bash
echo "Starting PVSio-web Network Controller..."
glassfish4/bin/asadmin start-domain --verbose=true 2> glassfish.err
