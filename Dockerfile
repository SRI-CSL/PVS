# This Dockerfile can be used to build a PVS 8.0 image.
# Assuming Docker is installed, you can run 
#   docker build . --tag pvs8
#   docker run --name pvs8 -it pvs8 ./pvs
# Note that you can give arguments to pvs, e.g.,
#   docker run --name pvs8 -it pvs8 ./pvs -raw -port 22334

# Builds using Ubuntu image
FROM ubuntu

MAINTAINER Sam Owre <owre@csl.sri.com>

# Update package manager, and install needed packages
RUN apt-get update \
 && apt-get -y dist-upgrade \
 && apt-get -y install apt-utils git sbcl emacs build-essential curl sudo

# Set up the /home/pvs-user account, with password "pvs-user" and Bash shell
RUN useradd -ms /bin/bash pvs-user \
 && echo "pvs-user:pvs-user" | chpasswd \
 && adduser pvs-user sudo

USER pvs-user

WORKDIR /home/pvs-user

# Without setting core.compression, I was getting timeouts in the build
RUN git config --global core.compression 0 \
 && git clone https://github.com/SRI-CSL/PVS.git \
 && git clone https://github.com/samowre/pvslib.git --branch pvs8.0

# NASA PVS Library
ENV PVS_LIBRARY_PATH=/home/pvs-user/pvslib
 
WORKDIR PVS

# Build PVS 8.0
RUN ./configure; make
