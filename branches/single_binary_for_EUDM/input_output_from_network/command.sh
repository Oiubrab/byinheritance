#!bin/bash

gfortran -o questions  discrete_flesh.f95  emerge.f95  flesh.f95  flesh_out.f95 answers.f95  questions.f95 
rm will.txt 
reset 
./questions 0.0 10 40 11 10 0 0.001 0.1 yes
