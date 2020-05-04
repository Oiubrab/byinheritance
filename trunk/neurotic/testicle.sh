#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
gfortran discrete_flesh.f95 network.f95 -o megalomaniac
reset
./megalomaniac closed 50000 30 60 0 yes
rm megalomaniac
