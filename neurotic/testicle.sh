#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
reset
pgfortran -traceback discrete_flesh.f95 network.f95 -o megalomaniac
./megalomaniac open 200 15 20 1 15 0 yes
rm megalomaniac network.o discrete_flesh.o discrete_flesh.mod
