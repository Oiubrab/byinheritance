#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
gfortran discrete_flesh.f95 network.f95 -o megalomaniac
reset
./megalomaniac open 50 10 10 0 debug
rm megalomaniac
