#!/bin/bash
rm -rf build
mkdir build
cd build
cmake ..
cmake ..
make -j 2
make install
install/bin/LunarGOO ../test/test.vert ../test/test.frag
