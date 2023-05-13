FROM ubuntu
MAINTAINER Sam Owre <owre@csl.sri.com>
RUN apt-get update
RUN apt-get -y dist-upgrade
RUN apt-get -y install apt-utils
RUN apt-get -y install git
RUN apt-get -y install sbcl
RUN apt-get -y install emacs
RUN apt-get -y install build-essential # gcc, etc.
RUN apt-get -y install curl
RUN git config --global core.compression 0
RUN git clone https://github.com/SRI-CSL/PVS.git
WORKDIR PVS
RUN ./configure
RUN make
