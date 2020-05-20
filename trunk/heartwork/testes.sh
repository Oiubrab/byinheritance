#!bin/bash

if [ -f 'megalomaniac' ]; then rm megalomaniac; fi
reset
pgfortran -traceback flesh.f95 blood.f95 -o megalomaniac
./megalomaniac 200 15 20 0 yes
rm megalomaniac blood.o flesh.o flesh.mod
