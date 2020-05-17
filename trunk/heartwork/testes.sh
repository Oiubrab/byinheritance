#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
reset
pgfortran -traceback flesh.f95 blood.f95 -o megalomaniac
./megalomaniac 15 40 1
rm megalomaniac blood.o flesh.o flesh.mod
