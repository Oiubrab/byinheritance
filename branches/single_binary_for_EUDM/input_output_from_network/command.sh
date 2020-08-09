#!bin/bash

gfortran -o questions  discrete_flesh.f95  emerge.f95  flesh.f95  flesh_out.f95 answers.f95  questions.f95 
if [ "$#" -eq 1 ]
then
if [ $1 == "delete" ]
then
	rm will.txt 
fi
fi
reset 
./questions 0.0 10 40 11 10 0 0.1 0.1 no
