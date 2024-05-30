#!/bin/bash

git clone https://github.com/BLAKE2/libb2.git
cd libb2
sudo apt install autoconf libtool
./autogen.sh
./configure
make
sudo make install
