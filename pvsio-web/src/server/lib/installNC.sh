#!/bin/bash
echo "Installing PVSio-web Network Controller..."
cat glassfish-4-1.zip.part* > glassfish-4.1.zip
rm -rf glassfish4
unzip -qq glassfish-4.1.zip
cp NetworkController.war glassfish4/glassfish/domains/domain1/autodeploy/
rm -rf glassfish-4.1.zip
echo "PVSio-web Network Controller installed successfully!"
