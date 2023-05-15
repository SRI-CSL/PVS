FROM ubuntu
MAINTAINER Sam Owre <owre@csl.sri.com>
RUN apt-get update && apt-get -y dist-upgrade && apt-get -y install apt-utils git sbcl emacs build-essential curl
RUN useradd -ms /bin/bash pvs-user
USER pvs-user
WORKDIR /home/pvs-user
RUN git config --global core.compression 0 && git clone https://github.com/SRI-CSL/PVS.git
WORKDIR PVS
RUN ./configure
RUN make
