#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
reset
pgfortran -traceback discrete_flesh.f95 network.f95 -o megalomaniac
./megalomaniac open 100000 70 30 15 5 0 yes
rm megalomaniac network.o discrete_flesh.o discrete_flesh.mod
