#!/bin/bash
set -x
set -e

VERSION=24.5
INSTALL_DIR=/opt/emacs

APPLICATION=emacs-${VERSION}
ARCHIVE=${APPLICATION}.tar.gz

apt-get update -y
apt-get install -y build-essential
apt-get build-dep -y emacs

cd /tmp
curl -O http://ftp.gnu.org/gnu/emacs/${ARCHIVE}
tar -xzvf ${ARCHIVE}
rm ${ARCHIVE}

cd ${APPLICATION}/
./configure --prefix=$INSTALL_DIR
make
make install

cd ../
rm -r ${APPLICATION}/

ln -s ${INSTALL_DIR}/bin/${APPLICATION} /usr/bin/emacs

